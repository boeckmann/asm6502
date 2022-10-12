#ifdef __BORLANDC__
#pragma warn -sig
#endif

#define ID_LEN	32

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include "asm6502.h"

char *text = NULL;
char *code = NULL;

u16 pc = 0;    /* program counter */
u16 oc = 0;    /* output counter */
int errors = 0;

#define VALUE_DEFINED 0x40
#define DEFINED(x) (((x).t & VALUE_DEFINED) != 0)
#define UNDEFINED(x) (((x).t & VALUE_DEFINED) == 0)
#define KIND_DEFINED(x) ((x) != 0)

#define TYPE_NONE  0
#define TYPE_BYTE  1
#define TYPE_WORD  2
#define TYPE(v) ((v).t & 0x3f)
#define SET_TYPE(v, u) ((v).t = ((v).t & VALUE_DEFINED) | (u))
#define SET_DEFINED(v) ((v).t = ((v).t | VALUE_DEFINED))
#define SET_UNDEFINED(v) ((v).t = (v).t & 0x3f);
#define INFERE_DEFINED(a,b) if (UNDEFINED(a) || UNDEFINED(b)) { SET_UNDEFINED(a); } else { SET_DEFINED(a); }

#define MAXINT(a,b) (((b) >= (a)) ? (b) : (a))
#define NUM_TYPE(x) (((x) < 0x100) ? TYPE_BYTE : TYPE_WORD)
#define INFERE_TYPE(a,b) (((a).v >= 0x100) || ((b).v >= 0x100)) ? TYPE_WORD : MAXINT(TYPE(a),(TYPE(b)))

typedef struct value {
   u16 v;
   u8  t;
} value;

#define KIND_LBL	0x01
#define KIND_VAR	0x02
#define IS_LBL(x) (((x).kind & KIND_LBL) != 0)
#define IS_VAR(x) (((x).kind & KIND_VAR) != 0)

typedef struct symbol {
   char name[ID_LEN];
   value value;
   u8 kind;
   struct symbol *next;
} symbol;

symbol *symbols = NULL;

symbol *lookup(const char *name)
{
   symbol *table = symbols;
   while (table) {
      if (!strcmp(name, table->name)) return table;
      table = table->next;
   }
   return NULL;
}

symbol *aquire(const char *name)
{
   symbol *sym = lookup(name);
   if (!sym) {
      sym = malloc(sizeof(symbol));
      strcpy(sym->name, name);
      sym->next = symbols;
      sym->value.v = 0;
      sym->value.t = 0;
      sym->kind = 0;
      symbols = sym;
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
   for (; sym; sym = sym->next) {
      if (DEFINED(sym->value))
         printf("%c %-16s %4x %c\n", sym_f2c(sym->kind), sym->name, sym->value.v, sym_t2c(sym->value.t));
      else
         printf("%c %-16s    ? %c\n", sym_f2c(sym->kind), sym->name, sym_t2c(sym->value.t));
   }
}

#define ERR_NUM			1
#define ERR_UNBALANCED	2
#define ERR_ID 			3
#define ERR_IDLEN 		4
#define ERR_STMT		   5
#define ERR_EOL			6
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
   "byte value out of range"
};

jmp_buf error_jmp;
void error(int code)
{
   errors++;
   longjmp(error_jmp, code);
}

void deflbl(const char *id, u16 v)
{
   symbol *sym = aquire(id);
   if (IS_VAR(*sym) || (DEFINED(sym->value) && (sym->value.v != v))) error(ERR_REDEF);
   sym->value.v = v;
   sym->value.t = TYPE_WORD | VALUE_DEFINED;
   sym->kind = KIND_LBL;
}

symbol * reservelbl(const char *id)
{
   symbol *sym = aquire(id);
   if (DEFINED(sym->value)) error(ERR_REDEF);
   sym->value.v = 0;
   sym->value.t = TYPE_WORD;
   sym->kind = KIND_LBL;
   return sym;
}

void defvar(const char *id, const value v)
{
   symbol *sym = aquire(id);
   if (IS_LBL(*sym))
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

#define ishexdigit(x) (isdigit((x)) || (((x) >= 'a') && ((x) <= 'f')) || \
                       (((x) >= 'A') && ((x) <= 'F')))

u16 digit(const char *p)
{
   if (*p <= '9') return (u16)(*p - '0');
   if (*p <= 'F') return (u16)(*p + 10 - 'A');
   return (u16)(*p + 10 - 'a');
}

#define iseol(p) (((p) == 0x0a) || ((p) == 0x0d))
#define isend(p) (((!(p)) || (p) == 0x0a) || ((p) == 0x0d))

void skipeol(char **p)
{
   if (**p == 0x0d) (*p)++;
   if (**p == 0x0a) (*p)++;
}

void skipw(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
}

void skipl(char **p)
{
   while (!isend(**p)) (*p)++;
}

void skipwc(char **p)
{
   while ((**p == ' ') || (**p == '\t')) (*p)++;
   if (**p == ';') {
      (*p)++;
      while((**p != 0) && (**p != 0x0d) && (**p != 0x0a)) (*p)++;
   }
}

void skipnext(char **p)
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
      if (!ishexdigit(**p)) error(ERR_NUM);
      do {
         num.v = (num.v << 4) + digit((*p)++);
      }
      while (ishexdigit(**p));
      typ = ((*p-pt)>3) ? TYPE_WORD : NUM_TYPE(num.v);
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

void ident(char **p, char *id)
{
   int i=0;

   if (!isalpha(**p) || (**p == '_')) error(ERR_ID);
   do {
      *id++ = *(*p)++;
      i++;
      if (i >= ID_LEN) error(ERR_IDLEN);
   }
   while (isalnum(**p) || (**p == '_'));

   *id = '\0';
}

void ident_upcase(char **p, char *id)
{
   int i=0;

   if (!isalpha(**p)) error(ERR_ID);
   do {
      *id++ = toupper(*(*p)++);
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

   skipw(p);
   if (**p == '(') {
      (*p)++;
      res = expr(p);
      skipw(p);
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
      res.v = pc;
      SET_TYPE(res, TYPE_WORD);
      SET_DEFINED(res);
   }
   else if (isalpha(**p)) {
      ident(p, id);
      sym = lookup(id);
      if (!sym) sym = reservelbl(id);
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

   skipw(p);
   op = **p;

   while(op == '*') {
      (*p)++;
      n2 = primary(p);
      res.v = res.v * n2.v;
      SET_TYPE(res, INFERE_TYPE(res, n2));
      INFERE_DEFINED(res, n2);
      skipw(p);
      op = **p;
   }

   return res;
}

value term(char **p)
{
   value n2, res;
   char op;

   skipw(p);
   if (**p == '-') {
      (*p)++;
      res = product(p);
      res.v = -res.v;
      if (DEFINED(res)) SET_TYPE(res, NUM_TYPE(res.v));
   }
   else res = product(p);

   skipw(p);
   op = **p;

   while ((op == '+') || (op == '-')) {
      (*p)++;
      n2 = product(p);

      switch (op) {
         case '+':
            res.v = res.v + n2.v;
            break;
         case '-':
            res.v = res.v - n2.v;
            break;
      }
      SET_TYPE(res, INFERE_TYPE(res, n2));
      INFERE_DEFINED(res, n2);
      skipw(p);
      op = **p;
   }

   return res;
}

value expr(char **p)
{
   value v;

   skipw(p);
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
   for (; *p; p++) *p = toupper(*p);
}

idesc *getidesc(const char *p)
{
   int i;
   for (i=0; i < (int)(sizeof(itbl)/sizeof(idesc)); i++) {
      if (!strcmp(p, itbl[i].mn)) return &itbl[i];
   }
   return NULL;
}

void emit_b(u8 b, int pass)
{
   if (pass == 2) {
      code[oc] = b;
   }
   oc+=1;
}

void emit_0(idesc *instr, int am, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
   }
   oc+=1;
}

void emit_1(idesc *instr, int am, u8 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o;
   }
   oc+=2;
}

void emit_2(idesc *instr, int am, u16 o, int pass)
{
   if (pass == 2) {
      code[oc] = instr->op[am];
      code[oc+1] = o & 0xff;
      code[oc+2] = o >> 8;
   }
   oc+=3;
}

int instruction_imp_acc(char **p, int pass, idesc *instr)
{
   int am = AM_INV;

   if (instr->op[AM_ACC] != INV) am = AM_ACC;
   else if (instr->op[AM_IMP] != INV) am = AM_IMP;
   else error(ERR_AM);

   emit_0(instr, am, pass);

   return am;
}

int instruction_imm(char **p, int pass, idesc *instr)
{
   int am = AM_IMM;
   value v;
   am = AM_IMM;
   (*p)++;
   if (instr->op[am] == INV) error(ERR_AM);
   v = expr(p);
   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);
   }
   emit_1(instr, am, to_byte(v).v, pass);
   return am;
}

int instruction_rel(char **p, int pass, idesc *instr, value v)
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
   else off = (~0u) - (pct - v.v - 1u);
   emit_1(instr, am, off & 0xffu, pass);

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
   skipw(p);

   /* indirect X addressing mode? */
   if (**p == ',') {
      skipnext(p);
      ident_upcase(p, id);
      if (strcmp(id, "X")) error(ERR_INX);
      am = AM_INX;
      skipw(p);
      if (**p != ')') error(ERR_CLBR);
   }
   else {
      if (**p != ')') error(ERR_CLBR);
      skipnext(p);
      /* indirect Y addressing mode? */
      if (**p == ',') {
         skipnext(p);
         ident_upcase(p, id);
         if (strcmp(id, "Y")) error(ERR_INY);
         am = AM_INY;
      }
      else {
         am = AM_IND;
         if (instr->op[am] == INV) error(ERR_AM);
      }
   }

   if ((am == AM_INV) || (instr->op[am] == INV)) error(ERR_AM);

   if (pass == 2) {
      if (UNDEFINED(v)) error(ERR_UNDEF);
      if (((am == AM_INX) || am == (AM_INY)) && (TYPE(v) != TYPE_BYTE)) error(ERR_ILLTYPE);
   }

   if (am == AM_IND) {
      emit_2(instr, am, v.v, pass);
   }
   else {
      emit_1(instr, am, v.v, pass);
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
      emit_1(instr, am, v.v, pass);
   }
   else {
      emit_2(instr, am, v.v, pass);
   }


   return am;
}

/* handle absolute and zeropage addressing modes */
int instruction_abs_zp(char **p, int pass, idesc *instr, value v)
{
   int am = AM_INV;

   if ((TYPE(v) == TYPE_BYTE) && AM_VALID(*instr, AM_ZP)) {
      am = AM_ZP;
      if (pass == 2) {
         if (UNDEFINED(v)) error(ERR_UNDEF);
      }
      emit_1(instr, am, v.v, pass);
   }
   else if (AM_VALID(*instr, AM_ABS)) {
      am = AM_ABS;
      if (pass == 2) {
         if (UNDEFINED(v)) error(ERR_UNDEF);
      }
      emit_2(instr, am, v.v, pass);
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
   skipwc(p);
   if (isend(**p)) {
      am = instruction_imp_acc(p, pass, instr);
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
      skipw(p);
      /* relative instruction mode if instruction supports it */
      if (instr->op[AM_REL] != INV) {
         am = instruction_rel(p, pass, instr, v);
      }
      /* else we go through the possible absolute addressing modes */
      else if (**p == ',') {
         skipnext(p);
         am = instruction_abxy_zpxy(p, pass, instr, v);
      }
      /* must be absolute or zeropage addressing */
      else {
         am = instruction_abs_zp(p, pass, instr, v);
      }
   }

   /* update program counter */
   if (am == AM_INV) error(ERR_AM);
   pc += am_size[am];
}

void directive(char **p, int pass)
{
   char id[ID_LEN];
   value v;
   int next;

   ident_upcase(p, id);

   if (!strcmp(id, "ORG")) {
      v = expr(p);
      if (UNDEFINED(v)) error (ERR_UNDEF);
      pc = v.v;
   }
   else if (!strcmp(id, "PUT")) {
      v = expr(p);
      if (UNDEFINED(v)) error (ERR_UNDEF);
   }
   else if (!strcmp(id, "BYTE")) {
      do {
         next = 0;
         skipw(p);

         if (**p == '"') {
            (*p)++;
            while (!isend(**p) && (**p != '"')) {
               emit_b(**p, pass);
               (*p)++;
               pc++;
            }
            if (**p != '"') error(ERR_STREND);
            (*p)++;
         }
         else {
            v = expr(p);

            if (pass == 2) {
               if (UNDEFINED(v)) error (ERR_UNDEF);
               if (TYPE(v) != TYPE_BYTE) error(ERR_ILLTYPE);
            }
            emit_b(v.v, pass);

            pc++;
         }

         skipw(p);
         if (**p == ',') {
            skipnext(p);
            next = 1;
         }
      }
      while (next);
   }
   else {
      error(ERR_NODIRECTIVE);
   }
}

/* processes one statement or assembler instruction in phase 1 */
void statement(char **p, int pass)
{
   char id1[ID_LEN];
   value v1;
   char *pt;

   skipwc(p);
   if (isend(**p)) return;
   pt = *p;

   /* first check for variable or label definition */
   if (isalpha(**p)) {
      ident(p, id1);
      skipw(p);
      if (**p == '=') {	/* variable definition */
         (*p)++;
         v1 = expr(p);
         defvar(id1, v1);
         return;
      }
      else if (**p == ':') {
         (*p)++;
         deflbl(id1, pc);
         skipwc(p);
         if (isend(**p)) return;       
      }
      else *p = pt;
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

static int line;
void pass(char **p, int pass)
{
   int err;
   line = 1;
   pc = 0;
   oc = 0;

   if (!(err = setjmp(error_jmp))) {
      while (**p) {
         statement(p, pass);
         skipwc(p);

         //if (!(**p)) break;

         if (!isend(**p)) error(ERR_EOL);

         skipeol(p);
         line++;
      }
   }
   else {
      printf("error: line %d: %s\n", line, err_msg[err]);
   }
}

char *load(const char *fn)
{
   char *buf = NULL;
   FILE *f = fopen(fn, "rb");
   int size;
   if (!f) return 0;
   fseek(f, 0, SEEK_END);
   size = ftell(f);
   fseek(f, 0, SEEK_SET);
   buf = malloc(size+1);
   if (!buf) return NULL;
   fread(buf, 1, size, f);
   fclose(f);
   buf[size] = '\0';
   return buf;
}

int save(const char *fn, const char *data, int len)
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

int main(int argc, char *argv[])
{
   char *ttext;

   if ((argc != 3) || !strcmp(argv[1], argv[2])) {
      printf("Usage: asm6502 input output\n");
      return EXIT_SUCCESS;
   }

   if (!(ttext = text = load(argv[1]))) {
      printf("error loading file\n");
      goto err0;
   }

   pass(&ttext, 1);
   if (errors) {
      printf("abort: source contains errors\n");
      goto err1;
   }

   ttext = text;
   code = malloc(oc);
   pass(&ttext, 2);
   if (errors) {
      printf("abort: source contains errors\n");
      goto err2;
   }

   printf("output size = %d bytes\n", oc);
   fflush(stdout);

   if (!save(argv[2], code, oc)) {
      printf("error saving file\n");
      goto err2;
   }


   /*dump_symbols();*/
   return EXIT_SUCCESS;

err2:
   free(code);
err1:
   free(text);
err0:
   return EXIT_FAILURE;
}
