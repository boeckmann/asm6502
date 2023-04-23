/* ASM6502
 *
 * Copyright (c) 2022-2023 Bernd Boeckmann
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

/* customize the following to adapt to the syntax of other assemblers */
#define DIRECTIVE_LETTER       '.'
#define LOCAL_LABEL_LETTER     '@'
#define PROGRAM_COUNTER_LETTER '@'

#define AND_LETTER             '&'
#define OR_LETTER              '|'
#define EOR_LETTER             '^'

#define ID_LEN  32  /* maximum length of identifiers (variable names etc.) */
#define STR_LEN 255 /* maximum length of string literals */

#define MAX_FILES     64   /* maximum include files */
#define MAX_POS_STACK 32   

#ifdef __BORLANDC__
#pragma warn -sig
#endif

#define _CRT_SECURE_NO_WARNINGS /* Visual Studio */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <time.h>

#if __STDC_VERSION__ >= 201112L
#include <stdnoreturn.h>
#else
#define noreturn
#endif

#include "asm6502.h"

static int debug = 0;      /* set DEBUG env variable to enable debug output */      

static unsigned char *code = NULL;  /* holds the emitted code */
static int line;                    /* currently processed line number */

/* program counter and output counter may not be in sync */
/* this happens if an .org directive is used, which modifies the */
/* program counter but not the output counter. */

static u16 pc = 0;    /* program counter of currently assembled instruction */
static u16 oc = 0;    /* counter of emitted output bytes */


/* file and position structures */

typedef struct asm_file {
   char *filename;
   char *text;
   size_t size;
} asm_file;

typedef struct pos_stack {
   asm_file *file;
   char *pos;
   int line;
   char listing;
   char list_statements;  
} pos_stack;

/* All files that are read during the assembly passes are stored here. */
static asm_file asm_files[MAX_FILES];
static int asm_file_count = 0;
static asm_file *current_file;             /* currently processed file */
static char filename_buf[STR_LEN];

/* position stack is used when processing include files. Every time an
   include file is about to be processed, the position after the include
   directive gets pushed onto the stack */
static pos_stack pos_stk[MAX_POS_STACK];
static int pos_stk_ptr = 0;


/* program listing data */
static int listing = 0;           /* per-file listing enabled? must be true 
                                     for the following to have any effect */
static int list_statements = 0;   /* statement listing activated ? */
static int list_skip_one = 0;     /* suppress current statement in listing */
static FILE *list_file;

/* if 0 processing of statements is disabled by conditional assembly
   directives */
static int process_statements = 1;

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
   u8 flags;
   struct symbol *next;
   struct symbol *locals;  /* local subdefinitions */
} symbol;

#define SYMTBL_SIZE 1024
symbol *symtbl[SYMTBL_SIZE];
static int symbol_count = 0;           /* number of global symbols */
static symbol *current_label = NULL;   /* search scope for local labels */

#define KIND_LBL  1
#define KIND_VAR  2


typedef struct if_state {
   char process_statements;
   char condition_met;         /* 1 = condition was met for if */
} if_state;

#define IF_STATE_MAX 32
if_state if_stack[IF_STATE_MAX];
int if_stack_count = 0;

/* symbol specific preprocessor directives */
#define IS_LBL(x) (((x).kind & KIND_LBL) != 0)
#define IS_VAR(x) (((x).kind & KIND_VAR) != 0)

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

static char* err_msg[] = {
   "",
#define ERR_NUM         1
   "value expected",
#define ERR_UNBALANCED  2
   "unbalanced parentheses",
#define ERR_ID          3
   "identifier expected",
#define ERR_IDLEN       4
   "identifier length exceeded",
#define ERR_STMT        5
   "illegal statement",
#define ERR_EOL         6
   "end of line expected",
#define ERR_REDEF       7
   "illegal redefinition",
#define ERR_INSTR       8
   "unknown instruction mnemonic",
#define ERR_AM          9
   "invalid addressing mode for instruction",
#define ERR_CLBR        10
   "missing closing brace",
#define ERR_INX         11
   "malformed indirect X addressing",
#define ERR_INY         12
   "malformed indirect Y addressing",
#define ERR_NODIRECTIVE 13
   "unknown directive",
#define ERR_UNDEF       14
   "undefined value",
#define ERR_ILLTYPE     15
   "illegal type",
#define ERR_RELRNG      16
   "relative jump target out of range",
#define ERR_STREND      17
   "string not terminated",
#define ERR_BYTERNG     18
   "byte value out of range",
#define ERR_NO_GLOBAL   19
   "no scope for local definition",
#define ERR_CHR         20
   "malformed character constant",
#define ERR_STRLEN      21
   "string too long",
#define ERR_STR         22
   "string expected",
#define ERR_OPEN        23
   "can not read file",
#define ERR_MAXINC      24
   "maximum number of files exceeded",
#define ERR_NO_BYTE     25
   "byte sized value expected",
#define ERR_NO_MEM      26
   "insufficient memory",
#define ERR_MISSING_IF  27
   "missing .IF",
#define ERR_MISSING_ENDIF 28
   "missing .ENDIF",
#define ERR_MAX_IF      29
   "too many if nesting levels"
};

#define ERROR_NORM 1
#define ERROR_EXT  2 /* extended error with additional message */

static char error_hint[128];
static int errors = 0;
static int error_type = 0;

jmp_buf error_jmp;

noreturn static void error(int err)
{
   errors++;
   error_type = ERROR_NORM;
   longjmp(error_jmp, err);
}

noreturn static void error_ext(int err, const char* msg)
{
   errors++;
   error_type = ERROR_EXT;
   strncpy(error_hint, msg, sizeof(error_hint) - 1);
   longjmp(error_jmp, err);
}

static unsigned name_hash( const char *name )
{
   unsigned h = 0;
   while ( *name ) {
      h = h * 57 + ( *name++ - 'A' );
   }
   return h;
}

static symbol *lookup(const char *name, symbol *start)
{
   symbol *table = start;
   unsigned h;

   if (start == NULL) {
      h = name_hash( name ) & (SYMTBL_SIZE-1);
      table = symtbl[h];
   }
   
   while (table) {
      if (!strcmp(name, table->name)) return table;
      table = table->next;
   }

   return NULL;
}

static symbol * new_symbol(const char *name)
{
   symbol *sym = malloc(sizeof(symbol));
   if (!sym) error(ERR_NO_MEM);

   strcpy(sym->name, name);
   sym->value.v = 0;
   sym->value.t = 0;
   sym->kind = 0;
   sym->flags = 0;
   sym->locals = NULL;
   return sym;   
}

static void free_symbols(symbol *sym)
{
   symbol *curr, *next;
   curr = sym;

   while (curr) {
      if (curr->locals) free_symbols(curr->locals);
      next = curr->next;
      free(curr);
      curr = next;
   }
}

static void free_symtbl(void)
{
   int i;
   for (i = 0; i < SYMTBL_SIZE; i++) {
      free_symbols(symtbl[i]);
      symtbl[i] = NULL;
   }
}

static symbol *aquire(const char *name)
{
   unsigned h;

   symbol *sym = lookup(name, NULL);
   if (!sym) {
      h = name_hash( name ) & (SYMTBL_SIZE-1);
      sym = new_symbol(name);
      sym->next = symtbl[h];
      symtbl[h] = sym;
      symbol_count++;
   }
   return sym;
}

static symbol *aquire_local(const char *name, symbol *parent)
{
   symbol *sym;
   if (!parent) return NULL;
   sym = lookup(name, parent->locals);
   if (!sym) {
      sym = new_symbol(name);
      sym->next = parent->locals;
      parent->locals = sym;
   }
   return sym;
}

static char sym_kind_to_char(u8 kind)
{
   switch (kind) {
      case KIND_LBL:
         return 'L';
      case KIND_VAR:
         return 'V';
   }
   return '-';
}

static char sym_type_to_char(u8 typ)
{
   if ((typ & 0x3f) == 0) return '?';
   switch (typ & 0x3f) {
      case TYPE_BYTE:
         return 'b';
      case TYPE_WORD:
         return 'w';
   }
   return '?';
}

#if 0
static void dump_symbols(void)
{
   symbol *sym = symbols;
   symbol *locals;

   for (; sym; sym = sym->next) {
      if (DEFINED(sym->value))
         printf("%c %c %04x %s\n", sym_kind_to_char(sym->kind), 
            sym_type_to_char(sym->value.t), sym->value.v, sym->name);
      else
         printf("%c %c    ? %s\n", sym_kind_to_char(sym->kind),
            sym_type_to_char(sym->value.t), sym->name);
      if (IS_LBL(*sym)) {
         
         for (locals = sym->locals; locals; locals = locals->next) {
            printf("           %04x @%s\n", locals->value.v, locals->name);
         }
      }
   }
}
#endif

static symbol * define_label(const char *id, u16 v, symbol *parent)
{
   symbol *sym;
   if (parent) sym = aquire_local(id, parent);
   else sym = aquire(id);

   if (IS_VAR(*sym) || (DEFINED(sym->value) && (sym->value.v != v)))
      error(ERR_REDEF);

   sym->value.v = v;
   sym->value.t = ((TYPE(sym->value) == TYPE_WORD) ? TYPE_WORD : NUM_TYPE(v))
                | VALUE_DEFINED;
   sym->kind = KIND_LBL;

   return sym;
}

static symbol * reserve_label(const char *id, symbol *parent)
{
   symbol *sym;
   if (parent) sym = aquire_local(id, parent);
   else sym = aquire(id);

   if (DEFINED(sym->value)) error(ERR_REDEF);
   sym->value.v = 0;
   sym->value.t = TYPE_WORD;
   sym->kind = KIND_LBL;
   return sym;
}

static void define_variable(const char *id, const value v, symbol *parent)
{
   symbol *sym;
   if (parent) sym = aquire_local(id, parent);
   else sym = aquire(id);

   /* if already defined make sure the value did not change */
   if (DEFINED(sym->value) && sym->value.v != v.v) error(ERR_REDEF);
   sym->value.v = v.v;

   /* if the type is already set do not change it */
   if (TYPE(sym->value)) {
      /* if (NUM_TYPE(v.v) > TYPE(sym->value)) error(ERR_REDEF); */
      if (DEFINED(v)) SET_DEFINED(sym->value);
   }
   else sym->value.t = v.t;

   /* if previously defined as label make it word sized */
   if (IS_LBL(*sym)) SET_TYPE(sym->value, TYPE_WORD);
   sym->kind = KIND_VAR;
}

static value to_byte(value v)
{
   if (DEFINED(v) && (v.v > 0xff)) error(ERR_BYTERNG);
   SET_TYPE(v, TYPE_BYTE);
   return v;
}

#define IS_HEXDIGIT(x) (isdigit((x)) || (((x) >= 'a') && ((x) <= 'f')) || \
                       (((x) >= 'A') && ((x) <= 'F')))

static u16 digit(const char *p)
{
   if (*p <= '9') return (u16)(*p - '0');
   if (*p <= 'F') return (u16)(*p + 10 - 'A');
   return (u16)(*p + 10 - 'a');
}

#define IS_END(p) (((!(p)) || (p) == 0x0a) || ((p) == 0x0d))

static void skip_eol(char **p)
{
   if (**p == 0x0d) (*p)++;
   if (**p == 0x0a) (*p)++;
}

static void skip_white(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
}

static void skip_white_and_comment(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
   if (**p == ';') {
      (*p)++;
      while(!IS_END(**p)) (*p)++;
   }
}

static void skip_curr_and_white(char **p)
{
   (*p)++;
   while ((**p == ' ') || (**p == '\t')) {
      (*p)++;
   }
}

static void skip_to_eol(char **p)
{
   while (**p != 0x0a && **p != 0x0d) (*p)++;
}

static int starts_with(char *text, char *s)
{
   while (*s) {
      if (toupper(*text++) != toupper(*s++)) return 0;
   }
   return 1;
}

static value number(char **p)
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

static void _ident(char **p, char *id, int numeric)
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
static void ident(char **p, char *id)
{
   _ident(p, id, 0);
}

/* read identifier which may start with a digit */
static void nident(char **p, char *id)
{
   _ident(p, id, 1);
}

/* read identifier and convert to upper case */
static void ident_upcase(char **p, char *id)
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

static value expr(char**);

static value primary(char **p)
{
   value res;
   char id[ID_LEN];
   symbol *sym, *local_sym;

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
   else if (**p == LOCAL_LABEL_LETTER && isalnum(*(*p+1))) {  /* local label*/
      (*p)++;
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
   else if (**p == PROGRAM_COUNTER_LETTER) {
      (*p)++;
      res.v = pc;
      res.t = TYPE_WORD | VALUE_DEFINED;
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
      sym = lookup(id, NULL);
      if (!sym) sym = reserve_label(id, NULL);
      skip_white(p);
      if (**p == LOCAL_LABEL_LETTER) {
         /* qualified identifier: local label or variable */
         (*p)++;
         nident(p, id);
         local_sym = lookup(id, sym->locals);
         if (!local_sym) local_sym = reserve_label(id, sym);
         res = local_sym->value;
      }
      else {
         res = sym->value;         
      }
   }
   else res = number(p);
   return res;
}

static value product(char **p)
{
   value  n2, res;
   char op;

   res = primary(p);

   skip_white(p);
   op = **p;

   while((op == '*') || (op == '/') || (op == AND_LETTER)
         || (**p == '<' && *(*p+1) == '<')
         || (**p == '>' && *(*p+1) == '>')) {
      (*p)++;
      if (**p == '<' || **p == '>') (*p)++;

      n2 = primary(p);

      if (DEFINED(res) && DEFINED(n2)) {
         switch (op) {
            case '*':
               res.v = (u16)(res.v * n2.v); break;
            case '/':
               res.v = (u16)(res.v / n2.v); break;
            case AND_LETTER:
               res.v = (u16)(res.v & n2.v); break;
            case '<':
               res.v = (u16)(res.v << n2.v); break;
            case '>':
               res.v = (u16)(res.v >> n2.v); break;
         }         
      }
      else res.v = 0;

      INFERE_TYPE(res, n2);
      INFERE_DEFINED(res, n2);
      skip_white(p);
      op = **p;
   }

   return res;
}

static value term(char **p)
{
   value n2, res;
   char op;

   skip_white(p);
   if (**p == '-') {
      /* unary minus */
      (*p)++;
      res = product(p);
      res.v = -res.v;
      SET_TYPE(res, TYPE_WORD);
   }
   else {
      /* unary plus */
      if(**p == '+') {
         (*p)++;
      }
      res = product(p);
   }

   skip_white(p);
   op = **p;

   while ((op == '+') || (op == '-') || 
          (op == OR_LETTER) || (op == EOR_LETTER)) {
      (*p)++;
      n2 = product(p);

      if (DEFINED(res) && DEFINED(n2)) {
         switch (op) {
            case '+':
               res.v = res.v + n2.v; break;
            case '-':
               res.v = res.v - n2.v; break;
            case OR_LETTER:
               res.v = res.v | n2.v; break;
            case EOR_LETTER:
               res.v = res.v ^ n2.v; break;
         }         
      }
      else res.v = 0;

      INFERE_TYPE(res, n2);
      INFERE_DEFINED(res, n2);
      skip_white(p);
      op = **p;
   }

   return res;
}

static value comparison(char **p)
{
   value res, n2;
   char op, op2;

   res = term(p); 
    
   skip_white(p);
   while ((**p == '=' && *(*p+1) == '=') ||
          (**p == '!' && *(*p+1) == '=') ||
          (**p == '<' && *(*p+1) == '=') ||
          (**p == '>' && *(*p+1) == '=') ||
          (**p == '<') || (**p == '>')) {
      op = **p;
      op2 = *(*p + 1);
      *p += 1;
      if (**p == '=') *p += 1;

      n2 = term(p);

      if (DEFINED(res) && DEFINED(n2)) {
         switch (op) {
         case '=': res.v = res.v == n2.v; break;
         case '!': res.v = res.v != n2.v; break;
         case '<': res.v = (op2 == '=') ? res.v <= n2.v : res.v < n2.v; break;
         case '>': res.v = (op2 == '=') ? res.v >= n2.v : res.v > n2.v; break;
         }         
      }
      else res.v = 0;

      INFERE_DEFINED(res, n2);
      if (DEFINED(res) && res.v) res.v = 1;
      SET_TYPE(res, TYPE_BYTE);
  }

   return res;
}

static value expr(char **p)
{
   value v;

   skip_white(p);
   if (**p == '>') {
      (*p)++;
      v = comparison(p);
      SET_TYPE(v, TYPE_BYTE);
      v.v = v.v >> 8;
   }
   else if (**p == '<') {
      (*p)++;
      v = comparison(p);
      SET_TYPE(v, TYPE_BYTE);
      v.v = v.v & 0xff;
   }
   else if (starts_with(*p, "[b]")) {
      /* lossless byte conversion */
      *p += 3;
      v = comparison(p);
      if (DEFINED(v) && v.v > 0xff)
         error(ERR_BYTERNG);
      SET_TYPE(v, TYPE_BYTE);
   }
   else if (starts_with(*p, "[w]")) {
      /* lossless word conversion */
      *p += 3;
      v = comparison(p);
      SET_TYPE(v, TYPE_WORD);
   }
   else if (starts_with(*p, ".not")) {
      *p += 4;
      v = comparison(p);
      if (DEFINED(v)) {
         v.v = (v.v) ? 0 : 1;
      }
      SET_TYPE(v, TYPE_BYTE);
   }
   else v = comparison(p);
   return v;
}

static void upcase(char *p)
{
   for (; *p; p++) *p = (char)toupper(*p);
}

static idesc *getidesc(const char *p)
{
   int l = 0, r = sizeof(itbl) / sizeof(idesc), x;
   int cmp;

   while (r >= l) {
      x = l + ((r - l) >> 2);
      cmp = strcmp(p, itbl[x].mn);
      if (cmp == 0) return &itbl[x];
      else if (cmp > 0) l = x + 1;
      else r = x - 1;
   }
   return NULL;
}

static void emit_byte(u8 b, int pass)
{
   if (pass == 2) {
      code[oc] = b;
   }
   oc+=1;
}

static void emit(const char *p, u16 len, int pass)
{
   u16 i=0;

   if (pass == 2) {
      for (i=0; i<len; i++) {
         code[oc+i] = p[i];
      }      
   }
   oc+=len;
}

static void emit_word(u16 w, int pass)
{
   if (pass == 2) {
      code[oc] = w & 0xff;
      code[oc+1] = w >> 8;
   }
   oc+=2;
}

/* emit instruction without argument */
static void emit_instr_0(idesc *instr, int am, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
   }
   oc+=1;
}

/* emit instruction with byte argument */
static void emit_instr_1(idesc *instr, int am, u8 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o;
   }
   oc+=2;
}

/* emit instruction with word argument */
static void emit_instr_2(idesc *instr, int am, u16 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o & 0xff;
      code[oc+2] = o >> 8;
   }
   oc+=3;
}

static int instruction_imp_acc(int pass, idesc *instr)
{
   int am = AM_INV;

   if (instr->op[AM_ACC] != INV) am = AM_ACC;
   else if (instr->op[AM_IMP] != INV) am = AM_IMP;
   else error(ERR_AM);

   emit_instr_0(instr, am, pass);

   return am;
}

static int instruction_imm(char **p, int pass, idesc *instr)
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

static int instruction_rel(int pass, idesc *instr, value v)
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
static int instruction_ind(char **p, int pass, idesc *instr)
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
      if (((am == AM_INX) || am == (AM_INY)) && (TYPE(v) != TYPE_BYTE))
         error(ERR_ILLTYPE);
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
static int instruction_abxy_zpxy(char **p, int pass, idesc *instr, value v)
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
static int instruction_abs_zp(int pass, idesc *instr, value v)
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
static void instruction(char **p, int pass)
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

static int string_lit(char **p, char *buf, int bufsize)
{
   char *start = *p;

   if (**p != '"') error (ERR_STR);
   (*p)++;
   while (**p != '"') {
      if (bufsize && *p - start + 1 >= bufsize) error(ERR_STRLEN);
      if (IS_END(**p)) error(ERR_STREND);
      if (buf) *(buf++) = **p;
      (*p)++;
   }
   if (buf) *buf = '\0';
   (*p)++;
   return (int)(*p - start - 2);
}

static void directive_byte(char **p, int pass)
{
   value v;
   int next, len;
   char *tp;

   do {
      next = 0;
      skip_white(p);

      if (**p == '"') {
         tp = *p + 1;
         len = string_lit(p, NULL, 0);
         pc += (u16)len;
         emit(tp, (u16)len, pass);
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

static void directive_word(char **p, int pass)
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

static FILE * open_file(const char *fn, const char *mode)
{
   FILE *f;

   f = fopen(fn, mode);
   if (f) return f;

   /* TODO: search in different paths for file to open */

   return NULL;
}

static long file_size(FILE *f)
{
   long pos, size;
   
   pos = ftell(f);
   fseek(f, 0, SEEK_END);
   size = ftell(f);
   fseek(f, pos, SEEK_SET);

   return size;
}

static char * str_copy(const char *src)
{
   char *dst = malloc(strlen(src)+1);
   if (!dst) error(ERR_NO_MEM);

   strcpy(dst, src);
   return dst;
}

static asm_file * read_file(const char *fn)
{
   FILE *f;
   asm_file *file;

   long size;
   char *buf;

   for (file = asm_files; file < asm_files + asm_file_count; file++) {
      /* if file is already loaded return it */
      if (!strcmp(file->filename, fn)) {
         return file;
      }
   }
   
   /* too many files ? */
   if (file >= asm_files + MAX_FILES) return NULL;

   /* read file contents */
   f = open_file(fn, "rb");
   if (!f) return NULL;
   size = file_size(f);
   buf = malloc((size_t)size + 1);
   if (!buf) return NULL;
   fread(buf, 1, size, f);
   buf[size] = '\0';
   fclose(f);

   file->filename = str_copy(fn);
   file->text = buf;
   file->size = size;
   asm_file_count++;

   return file;
}

static void free_files(void)
{
   asm_file *file = asm_files;

   for (; file < asm_files + asm_file_count; file++) {
      free(file->filename);
      free(file->text);
   }   
}

static void push_pos_stack(asm_file *f, char *pos, int l)
{
   if (pos_stk_ptr >= MAX_POS_STACK) error(ERR_MAXINC);

   pos_stk[pos_stk_ptr].file = f;
   pos_stk[pos_stk_ptr].pos  = pos;
   pos_stk[pos_stk_ptr].line = l;
   pos_stk[pos_stk_ptr].listing = (char)listing;
   pos_stk[pos_stk_ptr].list_statements = (char)list_statements;
   list_statements = listing;
   pos_stk_ptr++;
}

static void pop_pos_stack(char **p)
{
   pos_stk_ptr--; 
   current_file = pos_stk[pos_stk_ptr].file;
   *p = pos_stk[pos_stk_ptr].pos;
   line = pos_stk[pos_stk_ptr].line;
   listing = pos_stk[pos_stk_ptr].listing;
   list_statements = pos_stk[pos_stk_ptr].list_statements;
}

static void directive_include(char **p, int pass)
{
   asm_file *file;
   (void)pass;

   /* read filename */
   skip_white(p);
   string_lit(p, filename_buf, STR_LEN);
   skip_white_and_comment(p);
   if (!IS_END(**p)) error(ERR_EOL);

   skip_eol(p);


   /* read the include file */
   file = read_file(filename_buf);
   if (!file) error_ext(ERR_OPEN, filename_buf);

   /* push current file and position to stk and set pointers to inc file */
   push_pos_stack(current_file, *p, line + 1);

   current_file = file;
   *p = current_file->text;
   line = 1;
}

static void directive_fill(char **p, int pass)
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
   }
   oc += count.v;
}

static void directive_binary(char **p, int pass)
{
   /* syntax: .binary "file"[,skip[,count]] */
   FILE *file;
   unsigned long size;
   value skip, count = {0};

   /* read filename */
   skip_white(p);
   string_lit(p, filename_buf, STR_LEN);
   skip_white_and_comment(p);

   file = open_file(filename_buf, "rb");
   if (!file) error(ERR_OPEN);

   size = file_size(file);
   count.v = (unsigned short)size;

   skip_white(p);
   if (**p == ',') {
      skip_curr_and_white(p);
      skip = expr(p);
      skip_white(p);
      if (**p == ',') {
         skip_curr_and_white(p);
         count = expr(p);
      }
   }
   else skip.v = 0;

   if (skip.v > size) {
      fclose(file);
      return;
   }
   if (skip.v + count.v > (unsigned short)size) {
      count.v = (unsigned short)size - skip.v;
   }

   if (pass == 2) {
      fseek(file, skip.v, SEEK_SET);
      fread(code+oc, count.v, 1, file);
   }

   pc += count.v;
   oc += count.v;

   fclose(file);
}

static void directive_if(char **p)
{
   value v;

   if (if_stack_count >= IF_STATE_MAX) error(ERR_MAX_IF);

   if_stack[if_stack_count].process_statements = process_statements;

   if (process_statements) {
      v = expr(p);
      if (!DEFINED(v)) error(ERR_UNDEF);
      process_statements = v.v != 0;
      if_stack[if_stack_count].condition_met = process_statements;
   }
   else {
      skip_to_eol(p);
   }

   if_stack_count++;
}

static void directive_else(void)
{
   if (!if_stack_count) error(ERR_MISSING_IF);

   if (if_stack[if_stack_count-1].process_statements)
      process_statements = !if_stack[if_stack_count-1].condition_met;
}

static void directive_endif(void)
{
   if (!if_stack_count) error(ERR_MISSING_IF);

   if_stack_count--;
   process_statements = if_stack[if_stack_count].process_statements;
}

static void directive_echo(char **p, int pass)
{
   value v;
   int next, print_hex;

   if (pass == 1) {
      skip_to_eol(p);
      return;
   }

   do {
      next = 0;
      skip_white(p);

      if ( **p == '"') {
         string_lit(p, filename_buf, STR_LEN);
         printf("%s", filename_buf);
      }
      else {
         if (starts_with(*p, "[$]")) {
            *p += 3;
            print_hex = 1;
         }
         else print_hex = 0;

         v = expr(p);
         if (DEFINED(v)) {
            if (print_hex)
               printf("$%X", (unsigned)v.v);
            else
               printf("%u", (unsigned)v.v);
         }
         else {
            printf("?");
         }
      }

      skip_white(p);
      if (**p == ',') {
         skip_curr_and_white(p);
         next = 1;
      }
   }
   while (next);

   puts("");
}

static int directive(char **p, int pass)
{
   char id[ID_LEN];
   value v;
   int again = 0;

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
      again = 1;
   }
   else if (!strcmp(id, "BINARY")) {
      directive_binary(p, pass);
   }
   else if (!strcmp(id, "ECHO")) {
      directive_echo(p, pass);
   }
   else if (!strcmp(id, "NOLIST")) {
      listing = 0;
   }
   else if (!strcmp(id, "LIST")) {
      listing = list_statements;
      list_skip_one = 1;
   }
   else {
      error(ERR_NODIRECTIVE);
   }

   return again;
}

static int ismnemonic(const char *id)
{
   char idupcase[ID_LEN];
   int l = 0, r = sizeof(itbl) / sizeof(idesc), x;
   int cmp;
   strcpy(idupcase, id);
   upcase(idupcase);

   while (r >= l) {
      x = l + ((r - l) >> 2);
      cmp = strcmp(idupcase, itbl[x].mn);
      if (cmp == 0) return 1;
      else if (cmp > 0) l = x + 1;
      else r = x - 1;
   }
   return 0;
}

/* processes one statement or assembler instruction */
static int statement(char **p, int pass)
{
   char id1[ID_LEN];
   value v1;
   char *pt;
   int again = 0;
   enum { NONE=0, GLOBAL_LABEL=1, LOCAL_LABEL=2 } label = NONE;

   skip_white_and_comment(p);
   if (IS_END(**p)) return 0;
   pt = *p;

   /* first check for variable or label definition */
   if (**p == LOCAL_LABEL_LETTER) {
      (*p)++;
      nident(p, id1);
      skip_white(p);
      label = LOCAL_LABEL;
   }
   else if (isalpha(**p)) {
      ident(p, id1);
      skip_white(p);
      label = GLOBAL_LABEL;
   }

   if (label && **p == '=') {       /* variable definition */
      (*p)++;
      v1 = expr(p);
      if (label == GLOBAL_LABEL)
         define_variable(id1, v1, NULL);
      else {
         if (!current_label) error(ERR_NO_GLOBAL);
         define_variable(id1, v1, current_label);
      }

      return again;
   }
   else if (label && ((**p == ':') || (!ismnemonic(id1)))) {
      if (**p == ':') (*p)++;
      
      if (label == GLOBAL_LABEL)
         current_label = define_label(id1, pc, NULL);
      else {
         if (!current_label) error(ERR_NO_GLOBAL);
         define_label(id1, pc, current_label);
      }

      skip_white_and_comment(p);
      if (IS_END(**p)) return again;
   }
   else *p = pt;

   /* check for directive or instruction */
   if (**p == DIRECTIVE_LETTER) {
      (*p)++;
      again = directive(p, pass);
   }
   else if (isalpha(**p)) {
      instruction(p, pass);
   }
   else error(ERR_STMT);

   return again;
}

static void byte_to_pchar(u8 w, char *p)
{
   u16 v;
   v = (w >> 4) & 0xf;
   p[0] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
   v = w & 0xf;
   p[1] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
}

static void word_to_pchar(u16 w, char *p)
{
   u16 v;
   v = (w >> 12) & 0xf;
   p[0] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
   v = (w >> 8) & 0xf;
   p[1] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
   v = (w >> 4) & 0xf;
   p[2] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
   v = w & 0xf;
   p[3] = v + '0' + ((v > 9) ? 'A' - '9' - 1 : 0);
}

char list_addr_buf[20] = "            ";
char list_code_buf[4] = "   ";

static void list_statement(char *statement_start, unsigned short pc_start,
                           unsigned short oc_start, char *p, int skipped)
{
   int count = 0;

   if (!listing || list_skip_one) return;

   if (oc_start < oc) {
      /* output program counter, but only if we emitted code */
      word_to_pchar(oc_start, list_addr_buf);
      word_to_pchar(pc_start, list_addr_buf + 6);
      /*fprintf(list_file, "%04X  %04X  ", oc_start, pc_start);*/
      fputs(list_addr_buf, list_file);
   }
   else
      fputs("            ", list_file);

   while (oc_start < oc && count < 3) {
      byte_to_pchar(code[oc_start++] & 0xff, list_code_buf);
      /*fprintf(list_file, "%02X ", (int)code[oc_start++] & 0xff);*/
      fputs(list_code_buf, list_file);
      count++;
   }

   if (oc_start + count < oc)
      fputs("...", list_file);
   else {
      while (count < 4) {
         fputs("   ", list_file);
         count++;
      }
   }
   fprintf(list_file, "%6d", line);
   if (skipped)
      fputs("- ", list_file);
   else
      fputs(": ", list_file);
   fwrite(statement_start, 1, (int)(p - statement_start), list_file);

   fputs("\n", list_file);
}

static int sym_cmp_name(const void *a, const void *b)
{
   const symbol *sa, *sb;
   sa = *(const symbol **)a;
   sb = *(const symbol **)b;

   return strcmp(sa->name, sb->name);
}

static int sym_cmp_val(const void *a, const void *b)
{
   const symbol *sa, *sb;
   sa = *(const symbol **)a;
   sb = *(const symbol **)b;

   return sa->value.v - sb->value.v;
}

static symbol **symtbl_to_array(void)
{
   symbol **tsym, *tsym2;
   symbol **sym_array, **sym_p;

   sym_array = sym_p = malloc(sizeof(symbol *) * ((size_t)symbol_count + 1));
   if (!sym_array) return NULL;
   
   for (tsym = symtbl; tsym < symtbl + SYMTBL_SIZE; tsym++ ) {
      if (!*tsym) continue;
      tsym2 = *tsym;
      while (tsym2) {
         *sym_p++ = tsym2;
         tsym2 = tsym2->next;
      }
   }
   *sym_p = NULL;

   return sym_array;   
}

static void list_symbols(void)
{
   symbol *sym, **sym_array, **sym_p;
   int i;

   sym_array = symtbl_to_array();
   if (!sym_array) return;

   for (i = 1; i <= 2; i++) {
      sym_p = sym_array;
   
      if (i == 1) {
         fputs("\n\n-- SYMBOLS BY NAME -------------------\n\n", list_file);
         qsort(sym_array, symbol_count, sizeof(symbol *), sym_cmp_name);
      }
      else {
         fputs("\n\n-- SYMBOLS BY VALUE ------------------\n\n", list_file);
         qsort(sym_array, symbol_count, sizeof(symbol *), sym_cmp_val);
      }
   
      fputs("    HEX    DEC  NAME\n", list_file);
      for (; *sym_p; sym_p++) {
         sym = *sym_p;
         fputc(sym_kind_to_char(sym->kind), list_file);
         fputc(sym_type_to_char(sym->value.t), list_file);
         fputc(' ', list_file);
         if (DEFINED(sym->value)) {
            if (TYPE(sym->value) == TYPE_BYTE)
               fprintf(list_file, "  %02X  %5u",sym->value.v, sym->value.v);
            else
               fprintf(list_file, "%04X  %5u", sym->value.v, sym->value.v);
         }
         else fputs("   ?      ?", list_file);
         fputs("  ", list_file);
         fputs(sym->name, list_file);
         fputs("\n", list_file);
      }
   }

   free(sym_array);
}

static void list_filename(char *fn)
{
   if (listing) {
      fprintf(list_file, "                          FILE: %s\n", fn);
   }
}

static int conditional_statement(char **p)
{
   char id[ID_LEN];
   char *pt = *p;

   skip_white_and_comment(p);

   if (IS_END(**p)) return 0;
   if (**p != DIRECTIVE_LETTER) return 0;
   (*p)++;
   ident_upcase(p, id);

   if (!strcmp(id, "IF")) {
      directive_if(p);
      return 1;
   }
   else if (!strcmp(id, "ELSE")) {
      directive_else();
      return 1;
   }
   else if (!strcmp(id, "ENDIF")) {
      directive_endif();
      return 1;
   }

   *p = pt;
   return 0;
}

static void pass(char **p, int pass)
{
   int err;
   
   char *statement_start;
   asm_file *last_file;
   unsigned short oc_start;
   unsigned short pc_start;
   int conditional;

   pc = 0;  /* initialize program counter to zero */
   oc = 0;  /* initialize output counter to zero */

   last_file = current_file;
   line = 1;
   current_label = NULL;
   listing = list_statements;
   process_statements = 1;
   if_stack_count = 0;

   if (!(err = setjmp(error_jmp))) {
      while (**p || pos_stk_ptr > 0) {
         conditional = 0;

         if (current_file != last_file) {
            /* file changed (start or terminate processing of inc file ) */
            if (pass == 2) list_filename(current_file->filename);
         }

         if (!**p) {
            /* pop position from file stack (return from include) if at EOF */
            pop_pos_stack(p);
            continue;
         }

         statement_start = *p;
         pc_start = pc;
         oc_start = oc;

         if (conditional_statement(p)) {
            conditional = 1;
         }
         else if (process_statements) {
            if (statement(p, pass)) {
               /* statement returns an "again" flag that is set by the include
                  directive. If true we have to start over again with new file,
                  position and line number */
               continue;
            }
         }
         else {
            skip_to_eol(p);
         }

         skip_white_and_comment(p);

         if (!IS_END(**p)) {
            /* every statement ends with a newline. if it is not found here
               it is an error condition */
            error(ERR_EOL);
         }

         if (pass == 2)
            list_statement(statement_start, pc_start, oc_start, *p,
               !conditional && !process_statements);
                 
         skip_eol(p);
         line++;

         list_skip_one = 0;
         last_file = current_file;
      }

      if (if_stack_count) error(ERR_MISSING_ENDIF);
   }
   else {
      if (error_type == ERROR_NORM)
         printf("%s:%d: error: %s\n", current_file->filename, 
            line, err_msg[err]);
      else
         printf("%s:%d: error: %s %s\n", current_file->filename, line,
            err_msg[err], error_hint);
   }
}

static int save_code(const char *fn, const unsigned char *data, int len)
{
   FILE *f = fopen(fn, "wb");
   if (!f) return 0;
   if ((fwrite(data, len, 1, f) == 0) && (oc != 0)) {
      fclose(f);
      return 0;
   }
   fclose(f);
   return 1;
}

static int init_listing(char *fn)
{
   time_t t;
   struct tm *tm;
   char ts[80];

   list_file = fopen(fn, "wb");
   list_statements = list_file != NULL;

   if (!list_file) return 0;

   time(&t);
   tm = localtime(&t);
   strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M", tm);

   fprintf(list_file, "ASM6502 LISTING FOR %s @ %s\n\n",
      current_file->filename, ts);
   fprintf(list_file, "FPos  PC    Code          Line# Assembler text\n");

   return 1;
}

int main(int argc, char *argv[])
{
   char *ttext;

   debug = (getenv("DEBUG") != NULL);

   /* check program arguments */
   if (argc < 3) {
      printf("Usage: asm6502 input output [listing]\n");
      return EXIT_SUCCESS;
   }
   if (!strcmp(argv[1], argv[2])) {
      printf("refuse to overwrite your source ;-)\n");
      return EXIT_FAILURE;
   }
   if (argc == 4  && 
      (!strcmp(argv[1], argv[3]) || !strcmp(argv[2], argv[3]))) {
      printf("refuse to overwrite your files ;-)\n");
      return EXIT_FAILURE;
   }

   if (!(current_file = read_file(argv[1]))) {
      printf("error loading file\n");
      errors = 1;
      goto ret0;
   }

   /* first assembler pass */
   ttext = current_file->text;
   pass(&ttext, 1);
   if (errors) {
      goto ret1;
   }

   if (argc == 4) {
      /*initialize listing */
      if (!init_listing(argv[3])) {
         printf("error opening listing file\n");
         errors = 1;
         goto ret1;
      }
      printf("writing listing to %s\n", argv[3]);
   }

   /* second assembler pass */
   ttext = current_file->text;
   code = malloc(oc);
   pass(&ttext, 2);
   if (errors) {
      goto ret2;
   }

   if (listing)
      list_symbols();

   printf("output size = %d bytes\n", oc);
   fflush(stdout);

   if (!save_code(argv[2], code, oc)) {
      printf("error saving file\n");
      errors = 1;
      goto ret2;
   }

ret2:
   if (list_file) fclose(list_file);
   free(code);
ret1:
   free_files();
ret0:
   free_symtbl();

   if (errors) return EXIT_FAILURE;
   else return EXIT_SUCCESS;
}
