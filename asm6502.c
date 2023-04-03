/* ASM6502
 *
 * Copyright (c) 2022 Bernd Böckmann
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#define ID_LEN  32  /* maximum length of identifiers (variable names etc.) */
#define STR_LEN 128 /* maximum length of string literals */

#ifdef __BORLANDC__
#pragma warn -sig
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <time.h>
#include "asm6502.h"

static int debug = 0;      /* set DEBUG env variable to enable debug output */      

static char *text = NULL;  /* holds the assembler source */
static char *code = NULL;  /* holds the emitted code */
static int text_len = 0;   /* total length of the source */
static int line;           /* currently processed line number */

/* program counter and output counter may not be in sync */
/* this happens if an .org directive is used, which modifies the */
/* program counter but not the output counter. */

static u16 pc = 0;    /* program counter of currently assembled instruction */
static u16 oc = 0;    /* counter of emitted output bytes */

/* The text variable holds all the assembler source.
   The main assembler file and all files included are joined by an EOF (0x1A)
   character and stored as a whole in the text variable. Binary zero
   marks the end of the assembler text.
   The filenames variable stores the filenames of all included files.
   When parsing the source the filenames_idx variable is incremented
   when en EOF character ist encountered, and the line counter variable is
   set to filelines[filenames_idx], current filename is set to
   filenames[filenames_idx].
*/

#define MAX_FILENAMES 33 /* maximum include files */

static char *filenames[MAX_FILENAMES];
static int filelines[MAX_FILENAMES];
static int filenames_idx = 0;
static int filenames_len = 0;

#define EOF_CHAR 0x1a

/* program listing data */
static int listing = 0;       /* do program listing? */
static char *list_pos;
static u16 list_oc, list_pc;
static FILE *list_file;

/* data type used when evaluating expressions */
/* the value may be undefined */
typedef struct value {
   u16 v;   /* the numeric value */
   u8  t;   /* type (none, byte or word) and state (defined or undefined) */
} value;

#define TYPE_NONE  0
#define TYPE_BYTE  1
#define TYPE_WORD  2

/* data type for storing symbols (labels and variables) */
typedef struct symbol {
   char name[ID_LEN];
   value value;
   u8 kind;                /* is it a label or a variable? */
   struct symbol *next;
   struct symbol *locals;  /* local subdefinitions */
} symbol;

#define KIND_LBL  1
#define KIND_VAR  2


static symbol *symbols = NULL;         /* global symbol table */
static symbol *current_label = NULL;   /* search scope for local labels */

/* symbol specific preprocessor directives */
#define IS_LBL(x) (((x).kind & KIND_LBL) != 0)
#define IS_VAR(x) (((x).kind & KIND_VAR) != 0)
#define KIND_DEFINED(x) ((x) != 0)

/* value specific preprocessor directives */
#define VALUE_DEFINED 0x40
#define DEFINED(x) (((x).t & VALUE_DEFINED) != 0)
#define UNDEFINED(x) (((x).t & VALUE_DEFINED) == 0)
#define SET_DEFINED(v) ((v).t = ((v).t | VALUE_DEFINED))
#define SET_UNDEFINED(v) ((v).t = (v).t & 0x3f);
#define INFERE_DEFINED(a,b) \
          if (UNDEFINED(a) || UNDEFINED(b)) { SET_UNDEFINED(a); } \
          else { SET_DEFINED(a); }

/* type specific preprocessor directives */
#define TYPE(v) ((v).t & 0x3f)
#define SET_TYPE(v, u) ((v).t = ((v).t & VALUE_DEFINED) | (u))
#define NUM_TYPE(x) (((x) < 0x100) ? TYPE_BYTE : TYPE_WORD)
#define INFERE_TYPE(a,b) (((a).v >= 0x100) || ((b).v >= 0x100)) ? \
          SET_TYPE((a), TYPE_WORD) : SET_TYPE((a), MAXINT(TYPE(a),(TYPE(b))))


symbol *lookup(const char *name, symbol *start)
{
   symbol *table = start;
   while (table) {
      if (!strcmp(name, table->name)) return table;
      table = table->next;
   }
   return NULL;
}

symbol * new_symbol(const char *name)
{
   symbol *sym = malloc(sizeof(symbol));
   strcpy(sym->name, name);
   sym->value.v = 0;
   sym->value.t = 0;
   sym->kind = 0;
   sym->locals = NULL;
   return sym;   
}

void free_symbols(symbol **sym)
{
   symbol *curr, *next;
   curr = *sym;

   while (curr) {
      if (curr->locals) free_symbols(&(curr->locals));
      next = curr->next;
      free(curr);
      curr = next;
   }

   *sym = NULL;
}

symbol *aquire(const char *name)
{
   symbol *sym = lookup(name, symbols);
   if (!sym) {
      sym = new_symbol(name);
      sym->next = symbols;
      symbols = sym;
   }
   return sym;
}

symbol *aquire_local(const char *name)
{
   symbol *sym;
   if (!current_label) return NULL;
   sym = lookup(name, current_label->locals);
   if (!sym) {
      sym = new_symbol(name);
      sym->next = current_label->locals;
      current_label->locals = sym;
   }
   return sym;
}

char sym_f2c(u8 kind)
{
   switch (kind) {
      case KIND_LBL:
         return 'L';
      case KIND_VAR:
         return 'V';
   }
   return '-';
}

char sym_t2c(u8 typ)
{
   if ((typ & 0x3f) == 0) return 'U';
   switch (typ & 0x3f) {
      case TYPE_BYTE:
         return 'B';
      case TYPE_WORD:
         return 'W';
   }
   return '?';
}

void dump_symbols(void)
{
   symbol *sym = symbols;
   symbol *locals;

   for (; sym; sym = sym->next) {
      if (DEFINED(sym->value))
         printf("%c %c %04x %s\n", sym_f2c(sym->kind), sym_t2c(sym->value.t), sym->value.v, sym->name);
      else
         printf("%c %c    ? %s\n", sym_f2c(sym->kind), sym_t2c(sym->value.t), sym->name);
      if (IS_LBL(*sym)) {
         
         for (locals = sym->locals; locals; locals = locals->next) {
            printf("           %04x @%s\n", locals->value.v, locals->name);
         }
      }
   }
}

#define ERR_NUM         1
#define ERR_UNBALANCED  2
#define ERR_ID          3
#define ERR_IDLEN       4
#define ERR_STMT        5
#define ERR_EOL         6
#define ERR_REDEF       7
#define ERR_INSTR       8
#define ERR_AM          9
#define ERR_LBLREDEF    10
#define ERR_CLBR        11
#define ERR_INX         12
#define ERR_INY         13
#define ERR_ILLAM       14
#define ERR_OPUNDEFT    15
#define ERR_NODIRECTIVE 16
#define ERR_UNDEF       17
#define ERR_ILLTYPE     18
#define ERR_RELRNG      19
#define ERR_STREND      20
#define ERR_BYTERNG     21
#define ERR_LOCAL_REDEF 22
#define ERR_NO_GLOBAL   23
#define ERR_CHR         24
#define ERR_STRLEN      25
#define ERR_STR         26
#define ERR_OPEN        27
#define ERR_MAXINC      28
#define ERR_NO_BYTE     29

char *err_msg[] = {
   "",
   "value expected",
   "unbalanced parentheses",
   "identifier expected",
   "identifier length exceeded",
   "illegal statement",
   "end of line expected",
   "illegal redefinition",
   "unknown instruction mnemonic",
   "invalid addressing mode for instruction",
   "symbol already defined as label",
   "missing closing brace",
   "malformed indirect X addressing",
   "malformed indirect Y addressing",
   "malformed addressing mode",
   "undefined operand size",
   "unknown directive",
   "undefined value",
   "illegal type",
   "relative jump target out of range",
   "string not terminated",
   "byte value out of range",
   "illegal redefinition of local label",
   "local label definition requires previous global label",
   "malformed character constant",
   "string too long",
   "string expected",
   "can not open file",
   "maximum number of include files reached",
   "byte sized value expected"
};

#define ERROR_NORM 1
#define ERROR_EXT  2 /* extended error with additional message */
static char error_hint[128];
static int errors = 0;
static int error_type = 0;

jmp_buf error_jmp;
void error(int err)
{
   errors++;
   error_type = ERROR_NORM;
   longjmp(error_jmp, err);
}

void error_ext(int err, const char *msg)
{
   errors++;
   error_type = ERROR_EXT;
   strncpy(error_hint, msg, sizeof(error_hint)-1);
   longjmp(error_jmp, err);
}

symbol * define_label(const char *id, u16 v)
{
   symbol *sym = aquire(id);
   if (IS_VAR(*sym) || (DEFINED(sym->value) && (sym->value.v != v))) error(ERR_REDEF);
   sym->value.v = v;
   sym->value.t = TYPE_WORD | VALUE_DEFINED;
   sym->kind = KIND_LBL;
   return sym;
}

symbol * define_local_label(char *id, u16 v)
{
   symbol *sym;

   if (!current_label) error(ERR_NO_GLOBAL);

   sym = aquire_local(id);
   if ((DEFINED(sym->value) && (sym->value.v != v))) error(ERR_LOCAL_REDEF);
   sym->value.v = v;
   sym->value.t = TYPE_WORD | VALUE_DEFINED;
   sym->kind = KIND_LBL;
   return sym;
}

symbol * reserve_label(const char *id)
{
   symbol *sym = aquire(id);
   if (DEFINED(sym->value)) error(ERR_REDEF);
   sym->value.v = 0;
   sym->value.t = TYPE_WORD;
   sym->kind = KIND_LBL;
   return sym;
}

void define_variable(const char *id, const value v)
{
   symbol *sym = aquire(id);
   if (DEFINED(sym->value) &&
         ((sym->value.v != v.v) || (sym->value.t != v.t))) error(ERR_REDEF);
   sym->value = v;
   /* if previously defined as label make it word sized */
   if (IS_LBL(*sym)) SET_TYPE(sym->value, TYPE_WORD);
   sym->kind = KIND_VAR;
}

value to_byte(value v)
{
   if (DEFINED(v) && (v.v > 0xff)) error(ERR_BYTERNG);
   SET_TYPE(v, TYPE_BYTE);
   return v;
}

#define IS_HEXDIGIT(x) (isdigit((x)) || (((x) >= 'a') && ((x) <= 'f')) || \
                       (((x) >= 'A') && ((x) <= 'F')))

u16 digit(const char *p)
{
   if (*p <= '9') return (u16)(*p - '0');
   if (*p <= 'F') return (u16)(*p + 10 - 'A');
   return (u16)(*p + 10 - 'a');
}

#define IS_EOL(p) (((p) == 0x0a) || ((p) == 0x0d))
#define IS_END(p) (((!(p)) || ((p) == EOF_CHAR) || (p) == 0x0a) || ((p) == 0x0d))

void skip_eol(char **p)
{
   if (**p == EOF_CHAR) (*p)++;
   if (**p == 0x0d) (*p)++;
   if (**p == 0x0a) (*p)++;
}

void skip_white(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
}

void skipl(char **p)
{
   while (!IS_END(**p)) (*p)++;
}

void skip_white_and_comment(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
   if (**p == ';') {
      (*p)++;
      while(!IS_END(**p)) (*p)++;
   }
}

void skip_curr_and_white(char **p)
{
   (*p)++;
   while ((**p == ' ') || (**p == '\t')) {
      (*p)++;
   }
}

value number(char **p)
{
   value num= {0,0};
   char *pt = *p;
   u8 typ;

   if (**p == '$') {
      (*p)++;
      if (!IS_HEXDIGIT(**p)) error(ERR_NUM);
      do {
         num.v = (num.v << 4) + digit((*p)++);
      }
      while (IS_HEXDIGIT(**p));
      typ = ((*p-pt)>3) ? TYPE_WORD : NUM_TYPE(num.v);
      SET_TYPE(num, typ);
      SET_DEFINED(num);
   }
   else if (**p == '%') {
      (*p)++;
      if ((**p != '0') && (**p != '1')) error(ERR_NUM);
      do {
         num.v = (num.v << 1) + (**p - '0');
         (*p)++;
      }
      while ((**p == '0') || (**p == '1'));
      typ = ((*p-pt)>9) ? TYPE_WORD : NUM_TYPE(num.v);
      SET_TYPE(num, typ);
      SET_DEFINED(num);
   }

   else {
      if (!isdigit(**p)) error(ERR_NUM);
      do {
         num.v = num.v * 10 + digit((*p)++);
      }
      while (isdigit(**p));
      SET_TYPE(num, ((*p-pt)>3) ? TYPE_WORD : NUM_TYPE(num.v));
      SET_DEFINED(num);
   }

   return num;
}

void _ident(char **p, char *id, int numeric)
{
   int i=0;

   if ((!numeric && !isalpha(**p) && (**p != '_'))
      || (!isalnum(**p) && (**p != '_'))) error(ERR_ID);

   do {
      *id++ = *(*p)++;
      i++;
      if (i >= ID_LEN) error(ERR_IDLEN);
   }
   while (isalnum(**p) || (**p == '_'));

   *id = '\0';
}

/* read identifier which may not start with a digit */
void ident(char **p, char *id)
{
   _ident(p, id, 0);
}

/* read identifier which may start with a digit */
void nident(char **p, char *id)
{
   _ident(p, id, 1);
}

/* read identifier and convert to upper case */
void ident_upcase(char **p, char *id)
{
   int i=0;

   if (!isalpha(**p)) error(ERR_ID);
   do {
      *id++ = (char)toupper(*(*p)++);
      i++;
      if (i >= ID_LEN) error(ERR_IDLEN);
   }
   while (isalnum(**p));

   *id = '\0';
}

value expr(char**);

value primary(char **p)
{
   value res;
   char id[ID_LEN];
   symbol *sym;

   skip_white(p);
   if (**p == '(') {
      (*p)++;
      res = expr(p);
      skip_white(p);
      if (**p != ')') error(ERR_UNBALANCED);
      (*p)++;
   }
   else if (**p == '?') {
      (*p)++;
      res.v = 0;
      res.t = 0;
   }
   else if (**p == '@') {
      (*p)++;
      
      if (isalnum(**p)) {  /* local label*/
         nident(p, id);
         sym = lookup(id, current_label->locals);
         if (sym) {
            res = sym->value;
         }
         else {
            res.v = 0;
            res.t = 0;
         }
      }
      else {               /* current program counter */
         res.v = pc;
         res.t = TYPE_WORD | VALUE_DEFINED;
      }
   }
   else if (**p == '\'') {
      (*p)++;
      if (IS_END(**p) || (**p < 0x20)) error(ERR_CHR);

      res.v = **p;
      res.t = TYPE_BYTE | VALUE_DEFINED;

      (*p)++;
      if (**p != '\'') error(ERR_CHR);
      (*p)++;
   }
   else if (isalpha(**p)) {
      ident(p, id);
      sym = lookup(id, symbols);
      if (!sym) sym = reserve_label(id);
      res = sym->value;
   }
   else res = number(p);
   return res;
}

value product(char **p)
{
   value  n2, res;
   char op;

   res = primary(p);

   skip_white(p);
   op = **p;

   while((op == '*') || (op == '&') || (op == '/')) {
      (*p)++;
      n2 = primary(p);

      switch (op) {
         case '*':
            res.v = (u16)(res.v * n2.v); break;
         case '/':
            res.v = (u16)(res.v / n2.v); break;
         case '&':
            res.v = (u16)(res.v & n2.v); break;
      }

      INFERE_TYPE(res, n2);
      INFERE_DEFINED(res, n2);
      skip_white(p);
      op = **p;
   }

   return res;
}

value term(char **p)
{
   value n2, res;
   char op;

   skip_white(p);
   if (**p == '-') {
      (*p)++;
      res = product(p);
      res.v = -res.v;
      SET_TYPE(res, TYPE_WORD);
   }
   else {
      if(**p == '+') {
         (*p)++;
      }
      res = product(p);
   }

   skip_white(p);
   op = **p;

   while ((op == '+') || (op == '-') || (op == '|') || (op == '^')) {
      (*p)++;
      n2 = product(p);

      switch (op) {
         case '+':
            res.v = res.v + n2.v; break;
         case '-':
            res.v = res.v - n2.v; break;
         case '|':
            res.v = res.v | n2.v; break;
         case '^':
            res.v = res.v ^ n2.v; break;
      }
      INFERE_TYPE(res, n2);
      INFERE_DEFINED(res, n2);
      skip_white(p);
      op = **p;
   }

   return res;
}

value expr(char **p)
{
   value v;

   skip_white(p);
   if (**p == '>') {
      (*p)++;
      v = term(p);
      SET_TYPE(v, TYPE_BYTE);
      v.v = v.v >> 8;
   }
   else if (**p == '<') {
      (*p)++;
      v = term(p);
      SET_TYPE(v, TYPE_BYTE);
      v.v = v.v & 0xff;
   }
   else if (**p == '!') {
      (*p)++;
      v = term(p);
      SET_TYPE(v, TYPE_WORD);
   }
   else v = term(p);
   return v;
}

void upcase(char *p)
{
   for (; *p; p++) *p = (char)toupper(*p);
}

idesc *getidesc(const char *p)
{
   int i;
   for (i=0; i < (int)(sizeof(itbl)/sizeof(idesc)); i++) {
      if (!strcmp(p, itbl[i].mn)) return &itbl[i];
   }
   return NULL;
}

void emit_byte(u8 b, int pass)
{
   if (pass == 2) {
      code[oc] = b;
   }
   oc+=1;
}

void emit(const char *p, u16 len, int pass)
{
   u16 i=0;

   if (pass == 2) {
      for (i=0; i<len; i++) {
         code[oc+i] = p[i];
      }      
   }
   oc+=len;
}

void emit_word(u16 w, int pass)
{
   if (pass == 2) {
      code[oc] = w & 0xff;
      code[oc+1] = w >> 8;
   }
   oc+=2;
}

/* emit instruction without argument */
void emit_instr_0(idesc *instr, int am, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
   }
   oc+=1;
}

/* emit instruction with byte argument */
void emit_instr_1(idesc *instr, int am, u8 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o;
   }
   oc+=2;
}

/* emit instruction with word argument */
void emit_instr_2(idesc *instr, int am, u16 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o & 0xff;
      code[oc+2] = o >> 8;
   }
   oc+=3;
}

int instruction_imp_acc(int pass, idesc *instr)
{
   int am = AM_INV;

   if (instr->op[AM_ACC] != INV) am = AM_ACC;
   else if (instr->op[AM_IMP] != INV) am = AM_IMP;
   else error(ERR_AM);

   emit_instr_0(instr, am, pass);

   return am;
}

int instruction_imm(char **p, int pass, idesc *instr)
{
   int am = AM_IMM;
   value v;

   (*p)++;
   if (instr->op[am] == INV) error(ERR_AM);
   v = expr(p);
   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);
   }
   emit_instr_1(instr, am, (u8)to_byte(v).v, pass);
   return am;
}

int instruction_rel(int pass, idesc *instr, value v)
{
   int am = AM_REL;
   u16 pct = pc + 2u;
   u16 off;

   /* relative branch offsets are in 2-complement */
   /* have to calculate it by hand avoiding implementation defined behaviour */
   /* using unsigned int because int may not be in 2-complement */
   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);

      if ((v.v >= pct) && ((u16)(v.v - pct) > 127u)) error(ERR_RELRNG);
      else if ((pct > v.v) && ((u16)(pct - v.v) > 128u)) error(ERR_RELRNG);
   }
   if (v.v >= pct) off = v.v - pct;
   else off = (u16)((~0u) - (pct - v.v - 1u));
   emit_instr_1(instr, am, off & 0xffu, pass);

   return am;
}

/* handle indirect addressing modes */
int instruction_ind(char **p, int pass, idesc *instr)
{
   char id[ID_LEN];
   int am = AM_INV;
   value v;

   (*p)++;
   v = expr(p);
   skip_white(p);

   /* indirect X addressing mode? */
   if (**p == ',') {
      skip_curr_and_white(p);
      ident_upcase(p, id);
      if (strcmp(id, "X")) error(ERR_INX);
      am = AM_INX;
      skip_white(p);
      if (**p != ')') error(ERR_CLBR);
      skip_curr_and_white(p);
   }
   else {
      if (**p != ')') error(ERR_CLBR);
      skip_curr_and_white(p);
      /* indirect Y addressing mode? */
      if (**p == ',') {
         skip_curr_and_white(p);
         ident_upcase(p, id);
         if (strcmp(id, "Y")) error(ERR_INY);
         am = AM_INY;
      }
      else {
         am = AM_IND;
      }
   }

   if ((instr->op[am]) == INV) error(ERR_AM);

   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);
      if (((am == AM_INX) || am == (AM_INY)) && (TYPE(v) != TYPE_BYTE)) error(ERR_ILLTYPE);
   }

   if (am == AM_IND) {
      emit_instr_2(instr, am, v.v, pass);
   }
   else {
      emit_instr_1(instr, am, (u8)v.v, pass);
   }

   return am;
}

/* handle absolute x and y, zeropage x and y addressing modes */
int instruction_abxy_zpxy(char **p, int pass, idesc *instr, value v)
{
   char id[ID_LEN];
   int am = AM_INV;

   ident_upcase(p, id);
   /* test for absolute and zeropage X addressing */
   if (!strcmp(id, "X")) {
      if ((TYPE(v) == TYPE_BYTE) && AM_VALID(*instr, AM_ZPX)) am = AM_ZPX;
      else if (AM_VALID(*instr, AM_ABX)) am = AM_ABX;
      else error(ERR_AM);
   }
   /* test for absolute and zeropage Y addressing */
   else if (!strcmp(id, "Y")) {
      if ((TYPE(v) == TYPE_BYTE) && AM_VALID(*instr, AM_ZPY)) am = AM_ZPY;
      else if (AM_VALID(*instr, AM_ABY)) am = AM_ABY;
      else error(ERR_AM);
   }
   else error(ERR_AM);

   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);
   }

   if ((am == AM_ZPX) || (am == AM_ZPY)) {
      emit_instr_1(instr, am, (u8) v.v, pass);
   }
   else {
      emit_instr_2(instr, am, v.v, pass);
   }


   return am;
}

/* handle absolute and zeropage addressing modes */
int instruction_abs_zp(int pass, idesc *instr, value v)
{
   int am = AM_INV;

   if ((TYPE(v) == TYPE_BYTE) && AM_VALID(*instr, AM_ZP)) {
      am = AM_ZP;
      if (pass == 2) {
         if (UNDEFINED(v)) error(ERR_UNDEF);
      }
      emit_instr_1(instr, am, (u8)v.v, pass);
   }
   else if (AM_VALID(*instr, AM_ABS)) {
      am = AM_ABS;
      if (pass == 2) {
         if (UNDEFINED(v)) error(ERR_UNDEF);
      }
      emit_instr_2(instr, am, v.v, pass);
   }
   else error(ERR_AM);
   return am;
}

/* process one instruction */
void instruction(char **p, int pass)
{
   char id[ID_LEN];
   idesc *instr;
   int am = AM_INV;
   value v;

   /* first get instruction for given mnemonic */
   ident_upcase(p, id);
   instr = getidesc(id);
   if (!instr) error(ERR_INSTR);

   /* if found get addressing mode */
   skip_white_and_comment(p);
   if (IS_END(**p)) {
      am = instruction_imp_acc(pass, instr);
   }
   else if (**p == '#') {
      am = instruction_imm(p, pass, instr);
   }

   /* handle indirect addressing modes */
   else if (**p == '(') {
      am = instruction_ind(p, pass, instr);
   }

   /* relative and absolute addressing modes */
   else {
      v = expr(p);
      skip_white(p);
      /* relative instruction mode if instruction supports it */
      if (instr->op[AM_REL] != INV) {
         am = instruction_rel(pass, instr, v);
      }
      /* else we go through the possible absolute addressing modes */
      else if (**p == ',') {
         skip_curr_and_white(p);
         am = instruction_abxy_zpxy(p, pass, instr, v);
      }
      /* must be absolute or zeropage addressing */
      else {
         am = instruction_abs_zp(pass, instr, v);
      }
   }

   /* update program counter */
   if (am == AM_INV) error(ERR_AM);
   pc += am_size[am];
}

int string_lit(char **p, char *buf, int bufsize)
{
   char *start = buf;

   if (**p != '"') error (ERR_STR);
   (*p)++;
   while (**p != '"') {
      if (buf - start >= bufsize - 1) error(ERR_STRLEN);
      if (IS_END(**p)) error(ERR_STREND);
      *(buf++) = **p;
      (*p)++;
   }
   *buf = '\0';
   (*p)++;
   return (int)(buf - start);
}

void directive_byte(char **p, int pass)
{
   value v;
   int next, len;
   char buf[STR_LEN];

   do {
      next = 0;
      skip_white(p);

      if (**p == '"') {
         len = string_lit(p, buf, STR_LEN);
         pc += (u16)len;
         emit(buf, (u16)len, pass);
      }
      else {
         v = expr(p);

         if (pass == 2) {
            if (UNDEFINED(v)) error (ERR_UNDEF);
            if (NUM_TYPE(v.v) != TYPE_BYTE) error(ERR_NO_BYTE);
         }
         emit_byte((u8)to_byte(v).v, pass);

         pc++;
      }

      skip_white(p);
      if (**p == ',') {
         skip_curr_and_white(p);
         next = 1;
      }
   }
   while (next);
}

void directive_word(char **p, int pass)
{
   value v;
   int next;

   do {
      next = 0;
      skip_white(p);

      v = expr(p);

      if (pass == 2) {
         if (UNDEFINED(v)) error (ERR_UNDEF);
      }
      emit_word(v.v, pass);

      pc+=2;
      skip_white(p);
      if (**p == ',') {
         skip_curr_and_white(p);
         next = 1;
      }
   }
   while (next);
}

static long file_size(const char *fn)
{
   FILE *f = fopen(fn, "rb");
   long size;
   if (!f) {
      error_ext(ERR_OPEN, fn);
   }
   fseek(f, 0, SEEK_END);
   size = ftell(f);
   fclose(f);
   return size;
}

static int read_file(const char *fn, char *buf)
{
   FILE *f = fopen(fn, "rb");
   long size;
   if (!f) return 0;
   fseek(f, 0, SEEK_END);
   size = ftell(f);
   fseek(f, 0, SEEK_SET);
   fread(buf, 1, size, f);
   fclose(f);

   return 1;
}

static char * str_copy(const char *src)
{
   char *dst = malloc(strlen(src)+1);
   strcpy(dst, src);
   return dst;
}

static void add_include_filename(const char *filename)
{
   int i;

   /* store filename and line information for error messages */
   if (filenames_len + 2 > MAX_FILENAMES)
      error(ERR_MAXINC);

   for (i = filenames_len-1; i>filenames_idx+1; i--) {
      filenames[i] = filenames[i-2];
      filelines[i] = filelines[i-2];
   }
   filenames[filenames_idx+1] = str_copy(filename);
   filenames[filenames_idx+2] = str_copy(filenames[filenames_idx]);
   filelines[filenames_idx+1] = 1;
   filelines[filenames_idx+2] = line;
   filenames_len +=2;   
}

void directive_include(char **p, int pass)
{
   char filename[STR_LEN];
   char *dir_start = *p;
   int start_offset, end_offset;
   int filesize, remaining_len;
   int old_len = text_len;

   (void)pass;

   /* find beginning of include directive */
   while (*dir_start != '.') dir_start--;
   start_offset = (int)(dir_start - text);

   /* read filename */
   skip_white(p);
   string_lit(p, filename, STR_LEN);
   skip_white_and_comment(p);
   if (!IS_END(**p)) error(ERR_EOL);

   /* calculate offset into source after include directive... */
   end_offset = (int)(*p - text);
   /* ... and the remaining source length */
   remaining_len = text_len - end_offset;

   filesize = file_size(filename);
   
   /* calculate new source length and aquire memory */
   text_len = (int)(text_len + filesize - (*p - dir_start) + 2);
   if (text_len > old_len) text = realloc(text, text_len + 1);

   /* make space for include file: 
      move remaining part of source to end of buffer */
   memmove(text + start_offset + filesize + 2, text + end_offset, remaining_len + 1);

   /* read content of included file to reserverd part of buffer */
   if (!read_file(filename, text + start_offset + 1)) error_ext(ERR_OPEN, filename);

   /* separate source files with EOF character */
   text[start_offset] = EOF_CHAR;
   text[start_offset + filesize + 1] = EOF_CHAR;

   /* set source pointer to beginning of included file */
   *p = text + start_offset;

   add_include_filename(filename);
}

void directive_fill(char **p, int pass)
{
   value count, filler;

   count = expr(p);
   if (UNDEFINED(count)) error (ERR_UNDEF);
   
   pc += count.v;

   skip_white(p);
   if (**p == ',') {
      /* check for filler value, otherwise fill with zero */
      skip_curr_and_white(p);
      filler = expr(p);
      if (UNDEFINED(filler)) error (ERR_UNDEF);
      if (NUM_TYPE(filler.v) != TYPE_BYTE) error(ERR_NO_BYTE);
   }
   else {
      filler.v = 0;
   }

   if (pass == 2) {
      memset(code + oc, filler.v, count.v);
      oc += count.v;
   }
}

void directive(char **p, int pass)
{
   char id[ID_LEN];
   value v;

   ident_upcase(p, id);

   if (!strcmp(id, "ORG")) {
      v = expr(p);
      if (UNDEFINED(v)) error (ERR_UNDEF);
      pc = v.v;
   }
   else if (!strcmp(id, "FILL")) {
      directive_fill(p, pass);
   }
   else if (!strcmp(id, "BYTE")) {
      directive_byte(p, pass);
   }
   else if (!strcmp(id, "WORD")) {
      directive_word(p, pass);
   }
   else if (!strcmp(id, "INCLUDE")) {
      directive_include(p, pass);
   }
   else {
      error(ERR_NODIRECTIVE);
   }
}

int ismnemonic(const char *id)
{
   char id1[ID_LEN];
   int i;
   
   strcpy(id1, id);
   upcase(id1);

   for (i=0; i<itbl_size; i++) {
      if (!strcmp(id1, itbl[i].mn)) return 1;
   }
   return 0;
}

/* processes one statement or assembler instruction */
void statement(char **p, int pass)
{
   char id1[ID_LEN];
   value v1;
   char *pt;

   skip_white_and_comment(p);
   if (IS_END(**p)) return;
   pt = *p;

   /* first check for variable or label definition */
   if (isalpha(**p)) {
      ident(p, id1);
      skip_white(p);
      if (**p == '=') {       /* variable definition */
         (*p)++;
         v1 = expr(p);
         define_variable(id1, v1);
         return;
      }
      else if ((**p == ':') || (!ismnemonic(id1))) {
         if (**p == ':') (*p)++;
         
         current_label = define_label(id1, pc);
         
         skip_white_and_comment(p);
         if (IS_END(**p)) return;
      }
      else *p = pt;
   }

   /* local label definition */
   else if (**p == '@') {
      (*p)++;
      nident(p, id1);
      
      define_local_label(id1, pc);
      
      skip_white(p);
      if (**p == ':') (*p)++;
      skip_white_and_comment(p);
      if (IS_END(**p)) return;
   }

   /* check for directive or instruction */
   if (**p == '.') {
      (*p)++;
      directive(p, pass);
   }
   else if (isalpha(**p)) {
      instruction(p, pass);
   }
   else error(ERR_STMT);
}

void list_statement(char *p)
{
   int count = 0;
   if (list_oc < oc)
      /* output program counter, but only if we emitted code */
      fprintf(list_file, "%04X  %04X  ", list_oc, list_pc);
   else
      fputs("            ", list_file);

   while (list_oc < oc && count < 3) {
      fprintf(list_file, "%02X ", (int)code[list_oc++] & 0xff);
      count++;
   }

   if (list_oc + count < oc)
      fputs("...", list_file);
   else {
      while (count < 4) {
         fputs("   ", list_file);
         count++;
      }
   }
   fprintf(list_file, "%6d: ", line);
   fwrite(list_pos, 1, (int)(p - list_pos), list_file);
   fputs("\n", list_file);
}

void pass(char **p, int pass)
{
   int err;
   line = 1;
   current_label = NULL;
   pc = 0;
   oc = 0;
   filenames_idx = 0;

   if (!(err = setjmp(error_jmp))) {
      while (**p) {
         statement(p, pass);
         skip_white_and_comment(p);

         if (!IS_END(**p)) error(ERR_EOL);

         if (listing)
            list_statement(*p);

         if (**p == EOF_CHAR) {
            (*p)++;
            filenames_idx++;
            line = filelines[filenames_idx];
            if (IS_EOL(**p)) {
               line++;
               skip_eol(p);               
            }

            if (listing)
               fprintf(list_file,
                  "                          "
                  "<<< FILE %s >>>\n", filenames[filenames_idx]);
         }
         else {
            line++;
            skip_eol(p);
         }

         if (listing) {
            list_oc = oc;
            list_pc = pc;
            list_pos = *p;
         }
      }
   }
   else {
      if (error_type == ERROR_NORM)
         printf("%s:%d: error: %s\n", filenames[filenames_idx], line, err_msg[err]);
      else
         printf("%s:%d: error: %s %s\n", filenames[filenames_idx], line, err_msg[err], error_hint);
   }      
}

char *read_main(const char *fn)
{
   char *buf = NULL;
   FILE *f = fopen(fn, "rb");
   long size;
   if (!f) return 0;
   fseek(f, 0, SEEK_END);
   size = ftell(f);
   fseek(f, 0, SEEK_SET);
   buf = malloc(size+1);
   if (!buf) return NULL;
   fread(buf, 1, size, f);
   fclose(f);
   buf[size] = '\0';

   text = buf;
   text_len = size;

   filenames[0] = str_copy(fn);
   filelines[0] = 1;
   filenames_idx = 0;
   filenames_len = 1;

   return buf;
}

int save_code(const char *fn, const char *data, int len)
{
   FILE *f = fopen(fn, "wb");
   if (!f) return 0;
   if ((fwrite(data, len, 1, f) == 0) && (pc != 0)) {
      fclose(f);
      return 0;
   }
   fclose(f);
   return 1;
}

void dump_filenames(void)
{
   int i;
   for (i=0; i<filenames_len; i++) {
      printf("file %s, line %d\n", filenames[i], filelines[i]);
   }
}

void free_filenames(void)
{
   int i;
   for (i=0; i < filenames_len; i++) {
      free(filenames[i]);
   }
}

int init_listing(char *fn)
{
   time_t t;
   struct tm *tm;
   char ts[80];

   list_file = fopen(fn, "wb");

   if (!list_file) return 0;

   listing = 1;   
   list_pos = text;
   list_oc = 0;
   list_pc = 0;

   time(&t);
   tm = localtime(&t);
   strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M", tm);

   fprintf(list_file, "ASM6502 LISTING FOR %s @ %s\n\n", filenames[0], ts);
   fprintf(list_file, "FPos  PC    Code          Line# Assembler text\n");

   return 1;
}

int main(int argc, char *argv[])
{
   char *ttext;

   debug = (getenv("DEBUG") != NULL);
   if (argc < 3) {
      printf("Usage: asm6502 input output [listing]\n");
      return EXIT_SUCCESS;
   }
   if (!strcmp(argv[1], argv[2])) {
      printf("refuse to overwrite your source ;-)\n");
      return EXIT_FAILURE;
   }

   if (!read_main(argv[1])) {
      printf("error loading file\n");
      errors = 1;
      goto ret0;
   }

   ttext = text;
   pass(&ttext, 1);
   if (errors) {
      goto ret1;
   }

   /*initialize listing */
   if (argc == 4) {
      if (!strcmp(argv[1], argv[3]) || !strcmp(argv[2], argv[3])) {
         printf("refuse to overwrite your files ;-)\n");
         errors = 1;
         goto ret1;
      }
      if (!init_listing(argv[3])) {
         printf("error opening listing file\n");
         errors = 1;
         goto ret1;
      }
      printf("writing listing to %s\n", argv[3]);
   }

   ttext = text;
   code = malloc(oc);
   pass(&ttext, 2);
   if (errors) {
      goto ret2;
   }

   printf("output size = %d bytes\n", oc);

   if (!save_code(argv[2], code, oc)) {
      printf("error saving file\n");
      errors = 1;
      goto ret2;
   }

   if (debug) {
      dump_filenames();
      printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
      dump_symbols();
      printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n%s\n", text);
   }

ret2:
   if (list_file) fclose(list_file);
   free(code);
ret1:
   /*ttext = text;
   while (*ttext) {
      if (*ttext > 0x1f)
         putchar(*ttext);
      printf("%02X ", *ttext);
      ttext++;
   }
   printf("\n");*/
   free(text);
ret0:
   free_symbols(&symbols);
   free_filenames();

   if (errors) return EXIT_FAILURE;
   else return EXIT_SUCCESS;
}
