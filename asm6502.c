/* ASM6502 - a small but useful assembler for the MOS 6502 microprocessor
 
Copyright (c) 2022-2023 Bernd Boeckmann
 
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* Visual Studio */
#endif

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

static int flag_quiet = 0;

static u8 *code = NULL;  /* holds the emitted code */
static u16 code_size;

static unsigned line;   /* currently processed line number */

/* program counter and output counter may not be in sync */
/* this happens if an .org directive is used, which modifies the */
/* program counter but not the output counter. */

static int pass; /* current assembler pass */

static u16 pc = 0;    /* program counter of currently assembled instruction */
static u16 oc = 0;    /* counter of emitted output bytes */


/* file and position structures */

typedef struct asm_file {
   char *filename;
   char *text;
} asm_file;

typedef struct pos_stack {
   asm_file *file;
   char *pos;
   unsigned line;
   int listing;
   int list_statements;
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
   u8 t;   /* type (none, byte or word) */
   u8 defined;   /* defined or undefined */
} value;

enum {
   TYPE_BYTE = 1,
   TYPE_WORD = 2
};

/* data type for storing symbols (labels and variables) */
typedef struct symbol {
   char name[ID_LEN];
   value value;
   u8 kind;                /* is it a label or a variable? */
   struct symbol *next;
   struct symbol *locals;  /* local sub-definitions */
   char *filename;
   unsigned line;
} symbol;

enum {
   KIND_LBL = 1,
   KIND_VAR = 2
};

#define SYMBOL_TBL_SIZE 1024
symbol *symbol_tbl[SYMBOL_TBL_SIZE];

static int symbol_count = 0;           /* number of global symbols */
static symbol *current_label = NULL;   /* search scope for local labels */

typedef struct if_state {
   u8 process_statements;
   u8 condition_met;         /* 1 = condition was met for if */
} if_state;

#define IF_STATE_MAX 32
if_state if_stack[IF_STATE_MAX];
int if_stack_count = 0;

/* symbol specific preprocessor directives */
#define IS_LBL( x ) ((x).kind == KIND_LBL)
#define IS_VAR( x ) ((x).kind == KIND_VAR)

/* value specific preprocessor directives */
#define DEFINED( x ) (((x).defined) != 0)
#define UNDEFINED( x ) (((x).defined) == 0)
#define SET_DEFINED( v ) ((v).defined = 1)
#define SET_UNDEFINED( v ) ((v).defined = 0)
#define INFER_DEFINED( a, b ) (a).defined = DEFINED(a) || DEFINED(b)

/* type specific preprocessor directives */
#define TYPE( v ) ((v).t)
#define SET_TYPE( v, u ) ((v).t = (u))

#define NUM_TYPE( x ) (((x) < 0x100) ? TYPE_BYTE : TYPE_WORD)

#define INFER_TYPE( a, b ) \
         (((a).v >= 0x100) || ((b).v >= 0x100)) \
            ? SET_TYPE((a), TYPE_WORD) \
            : SET_TYPE((a), MAXINT(TYPE(a),(TYPE(b))))

enum {
   ERR_NUM = 1,
   ERR_UNBALANCED,
   ERR_ID,
   ERR_ID_LEN,
   ERR_STMT,
   ERR_EOL,
   ERR_REDEFINITION,
   ERR_INSTR,
   ERR_AM,
   ERR_CLOSING_PAREN,
   ERR_INX,
   ERR_INY,
   ERR_NO_DIRECTIVE,
   ERR_UNDEF,
   ERR_ILLEGAL_TYPE,
   ERR_RELATIVE_RANGE,
   ERR_STR_END,
   ERR_BYTE_RANGE,
   ERR_NO_GLOBAL,
   ERR_CHR,
   ERR_STRLEN,
   ERR_STR,
   ERR_OPEN,
   ERR_MAX_INC,
   ERR_NO_BYTE,
   ERR_NO_MEM,
   ERR_MISSING_IF,
   ERR_MISSING_ENDIF,
   ERR_MAX_IF,
   ERR_PHASE,
   ERR_PHASE_SIZE,
   ERR_DIV_BY_ZERO
};

static char *err_msg[] = {
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
   "missing closing brace",
   "malformed indirect X addressing",
   "malformed indirect Y addressing",
   "unknown directive",
   "undefined value",
   "illegal type",
   "relative jump target out of range",
   "string not terminated",
   "byte value out of range",
   "no scope for local definition",
   "malformed character constant",
   "string too long",
   "string expected",
   "can not read file",
   "maximum number of files exceeded",
   "byte sized value expected",
   "insufficient memory",
   "missing .IF",
   "missing .ENDIF",
   "too many if nesting levels",
   "symbol value mismatch between pass one and two",
   "pass two code size greater than pass one code size",
   "division by zero"
};

enum {
   ERROR_NORM,
   ERROR_EXT,     /* extended error with additional message */
   ERROR_ABORT    /* .error directive */
};

static char error_hint[128];
static int errors = 0;
static int error_type = 0;

jmp_buf error_jmp;

noreturn static void error( int err ) {
   errors++;
   error_type = ERROR_NORM;
   longjmp( error_jmp, err );
}


noreturn static void error_ext( int err, const char *msg ) {
   errors++;
   error_type = ERROR_EXT;
   strncpy( error_hint, msg, sizeof( error_hint ) - 1 );
   longjmp( error_jmp, err );
}


noreturn static void error_abort( void ) {
   errors++;
   error_type = ERROR_ABORT;
   longjmp( error_jmp, -1 );
}


static unsigned name_hash( const char *name ) {
   unsigned h = 0;
   while ( *name ) {
      h = h * 57 + ( *name++ - 'A' );
   }
   return h;
}


static symbol *lookup( const char *name, symbol *start ) {
   symbol *table = start;
   unsigned h;

   if ( start == NULL) {
      h = name_hash( name ) & ( SYMBOL_TBL_SIZE - 1 );
      table = symbol_tbl[h];
   }

   while ( table ) {
      if ( !strcmp( name, table->name )) return table;
      table = table->next;
   }

   return NULL;
}


static symbol *new_symbol( const char *name ) {
   symbol *sym = malloc( sizeof( symbol ));
   if ( !sym ) error( ERR_NO_MEM );

   strcpy( sym->name, name );
   sym->value.v = 0;
   sym->value.t = 0;
   sym->value.defined = 0;

   sym->kind = 0;
   sym->locals = NULL;
   sym->filename = NULL;
   sym->line = 0;
   return sym;
}


static void free_symbols( symbol *sym ) {
   symbol *curr, *next;
   curr = sym;

   while ( curr ) {
      if ( curr->locals ) free_symbols( curr->locals );
      next = curr->next;
      free( curr );
      curr = next;
   }
}


static void free_symbol_tbl( void ) {
   int i;
   for ( i = 0; i < SYMBOL_TBL_SIZE; i++ ) {
      free_symbols( symbol_tbl[i] );
      symbol_tbl[i] = NULL;
   }
}


static symbol *acquire( const char *name ) {
   unsigned h;

   symbol *sym = lookup( name, NULL);
   if ( !sym ) {
      h = name_hash( name ) & ( SYMBOL_TBL_SIZE - 1 );
      sym = new_symbol( name );
      sym->next = symbol_tbl[h];
      symbol_tbl[h] = sym;
      symbol_count++;
   }
   return sym;
}


static symbol *acquire_local( const char *name, symbol *parent ) {
   symbol *sym;
   if ( !parent ) return NULL;
   sym = lookup( name, parent->locals );
   if ( !sym ) {
      sym = new_symbol( name );
      sym->next = parent->locals;
      parent->locals = sym;
   }
   return sym;
}


static symbol *define_label( const char *id, u16 v, symbol *parent ) {
   symbol *sym;
   if ( parent ) sym = acquire_local( id, parent );
   else sym = acquire( id );

   if ( IS_VAR( *sym ) || (DEFINED( sym->value ) && ( sym->value.v != v ))) {
      if ( pass == 1 ) error( ERR_REDEFINITION );
      else error( ERR_PHASE );
   }

   sym->value.v = v;
   sym->value.t = ((TYPE( sym->value ) == TYPE_WORD ) ? TYPE_WORD : NUM_TYPE( v ));
   sym->value.defined = 1;
   sym->kind = KIND_LBL;
   sym->filename = current_file->filename;
   sym->line = line;
   return sym;
}


static symbol *reserve_label( const char *id, symbol *parent ) {
   symbol *sym;
   if ( parent ) sym = acquire_local( id, parent );
   else sym = acquire( id );

   if ( DEFINED( sym->value )) error( ERR_REDEFINITION );
   sym->value.v = 0;
   sym->value.t = TYPE_WORD;
   sym->kind = KIND_LBL;
   return sym;
}


static void define_variable( const char *id, const value v, symbol *parent ) {
   symbol *sym;
   if ( parent ) sym = acquire_local( id, parent );
   else sym = acquire( id );

   /* if already defined make sure the value did not change */
   if ( DEFINED( sym->value ) && sym->value.v != v.v ) {
      if ( pass == 1 ) error( ERR_REDEFINITION );
      else error( ERR_PHASE );
   }

   sym->value.v = v.v;
   sym->value.defined = v.defined;
   sym->filename = ( current_file ) ? current_file->filename : NULL;
   sym->line = line;

   /* if the type is already set do not change it */
   if ( !TYPE( sym->value )) sym->value.t = v.t;

   /* if previously defined as label make it word sized */
   if ( IS_LBL( *sym )) SET_TYPE( sym->value, TYPE_WORD );
   sym->kind = KIND_VAR;
}


static value to_byte( value v ) {
   if ( DEFINED( v ) && ( v.v > 0xff )) error( ERR_BYTE_RANGE );
   SET_TYPE( v, TYPE_BYTE );
   return v;
}


#define IS_HEX_DIGIT( x ) (isdigit((x)) || (((x) >= 'a') && ((x) <= 'f')) || \
                           (((x) >= 'A') && ((x) <= 'F')))


static u16 digit( const char *p ) {
   if ( *p <= '9' ) return (u16) ( *p - '0' );
   if ( *p <= 'F' ) return (u16) ( *p + 10 - 'A' );
   return (u16) ( *p + 10 - 'a' );
}


#define IS_END( p ) (((!(p)) || (p) == 0x0a) || ((p) == 0x0d))


static void skip_eol( char **p ) {
   if ( **p == 0x0d ) ( *p )++;
   if ( **p == 0x0a ) ( *p )++;
}


static void skip_white( char **p ) {
   while (( **p == ' ' ) || ( **p == '\t' )) ( *p )++;
}


static void skip_white_and_comment( char **p ) {
   while (( **p == ' ' ) || ( **p == '\t' )) ( *p )++;
   if ( **p == ';' ) {
      ( *p )++;
      while ( !IS_END( **p )) ( *p )++;
   }
}


static void skip_curr_and_white( char **p ) {
   ( *p )++;
   while (( **p == ' ' ) || ( **p == '\t' )) {
      ( *p )++;
   }
}


static void skip_to_eol( char **p ) {
   while ( **p != 0 && **p != 0x0a && **p != 0x0d ) ( *p )++;
}


static int starts_with( char *text, char *s ) {
   while ( *s ) {
      if ( toupper( *text++ ) != toupper( *s++ )) return 0;
   }
   return 1;
}


static value number( char **p ) {
   value num = { 0 };
   char *pt = *p;
   u8 typ;

   if ( **p == '$' ) {
      ( *p )++;
      if ( !IS_HEX_DIGIT( **p )) error( ERR_NUM );
      do {
         num.v = ( num.v << 4 ) + digit(( *p )++ );
      } while (IS_HEX_DIGIT( **p ));
      typ = (( *p - pt ) > 3 ) ? TYPE_WORD : NUM_TYPE( num.v );
      SET_TYPE( num, typ );
      SET_DEFINED( num );
   } else if ( **p == '%' ) {
      ( *p )++;
      if (( **p != '0' ) && ( **p != '1' )) error( ERR_NUM );
      do {
         num.v = ( num.v << 1 ) + ( **p - '0' );
         ( *p )++;
      } while (( **p == '0' ) || ( **p == '1' ));
      typ = (( *p - pt ) > 9 ) ? TYPE_WORD : NUM_TYPE( num.v );
      SET_TYPE( num, typ );
      SET_DEFINED( num );
   } else {
      if ( !isdigit( **p )) error( ERR_NUM );
      do {
         num.v = num.v * 10 + digit(( *p )++ );
      } while ( isdigit( **p ));
      SET_TYPE( num, (( *p - pt ) > 3 ) ? TYPE_WORD : NUM_TYPE( num.v ));
      SET_DEFINED( num );
   }

   return num;
}


static void ident_( char **p, char *id, int numeric ) {
   int i = 0;

   if (( !numeric && !isalpha( **p ) && ( **p != '_' ))
       || ( !isalnum( **p ) && ( **p != '_' )))
      error( ERR_ID );

   do {
      *id++ = *( *p )++;
      i++;
      if ( i >= ID_LEN ) error( ERR_ID_LEN );
   } while ( isalnum( **p ) || ( **p == '_' ));

   *id = '\0';
}


/* read identifier which may not start with a digit */
static void ident( char **p, char *id ) {
   ident_( p, id, 0 );
}


/* read identifier which may start with a digit */
static void numbered_ident( char **p, char *id ) {
   ident_( p, id, 1 );
}


/* read identifier and convert to upper case */
static void ident_uppercase( char **p, char *id ) {
   int i = 0;

   if ( !isalpha( **p )) error( ERR_ID );
   do {
      *id++ = (char) toupper( *( *p )++ );
      i++;
      if ( i >= ID_LEN ) error( ERR_ID_LEN );
   } while ( isalnum( **p ));

   *id = '\0';
}


static value expr( char ** );


static value primary( char **p ) {
   value res = { 0 };
   char id[ID_LEN];
   symbol *sym, *local_sym;

   skip_white( p );
   if ( **p == '(' ) {
      ( *p )++;
      res = expr( p );
      skip_white( p );
      if ( **p != ')' ) error( ERR_UNBALANCED );
      ( *p )++;
   } else if ( **p == '.' && *( *p + 1 ) == '?' ) {
      ( *p ) += 2;
   } else if ( **p == LOCAL_LABEL_LETTER && isalnum( *( *p + 1 ))) {  /* local label*/
      ( *p )++;
      numbered_ident( p, id );
      sym = lookup( id, current_label->locals );
      if ( sym ) {
         res = sym->value;
      }
   } else if ( **p == PROGRAM_COUNTER_LETTER ) {
      ( *p )++;
      res.v = pc;
      SET_TYPE( res, TYPE_WORD );
      SET_DEFINED( res );
   } else if ( **p == '\'' ) {
      ( *p )++;
      if ( IS_END( **p ) || ( **p < 0x20 )) error( ERR_CHR );

      res.v = (u8) **p;
      SET_TYPE( res, TYPE_BYTE );
      SET_DEFINED( res );

      ( *p )++;
      if ( **p != '\'' ) error( ERR_CHR );
      ( *p )++;
   } else if ( isalpha( **p )) {
      ident( p, id );
      sym = lookup( id, NULL);
      if ( !sym ) sym = reserve_label( id, NULL);
      skip_white( p );
      if ( **p == LOCAL_LABEL_LETTER ) {
         /* qualified identifier: local label or variable */
         ( *p )++;
         numbered_ident( p, id );
         local_sym = lookup( id, sym->locals );
         if ( !local_sym ) local_sym = reserve_label( id, sym );
         res = local_sym->value;
      } else {
         res = sym->value;
      }
   } else res = number( p );
   return res;
}


static value unary( char **p ) {
   value res;
   char op = 0;

   skip_white( p );
   if ( **p == '~' || **p == '!' || **p == '?' ) {
      op = **p;
      ( *p )++;
      res = unary( p );
   } else res = primary( p );

   if ( op ) {
      switch ( op ) {
         case '?':
            res.v = DEFINED( res ) ? 1 : 0;
            SET_DEFINED( res );
            SET_TYPE( res, TYPE_BYTE );
            break;
         case '~':
            if ( DEFINED( res )) res.v = ~res.v;
            break;
         case '!':
            if ( DEFINED( res )) {
               res.v = !res.v;
               if ( res.v ) res.v = 1;
            }
            break;
         default:
            break;
      }
      if ( TYPE( res ) == TYPE_BYTE ) res.v &= 0xff;
   }
   return res;
}


static value product( char **p ) {
   value n2, res;
   char op;

   res = unary( p );

   skip_white( p );
   op = **p;

   while (( op == '*' ) || ( op == '/' )
          || ( op == AND_LETTER && *( *p + 1 ) != AND_LETTER )
          || ( **p == '<' && *( *p + 1 ) == '<' )
          || ( **p == '>' && *( *p + 1 ) == '>' )) {
      ( *p )++;
      if ( **p == '<' || **p == '>' ) ( *p )++;

      n2 = unary( p );

      if ( DEFINED( res ) && DEFINED( n2 )) {
         switch ( op ) {
            case '*':
               res.v = (u16) ( res.v * n2.v );
               break;
            case '/':
               if ( n2.v == 0 )
                  error( ERR_DIV_BY_ZERO );
               res.v = (u16) ( res.v / n2.v );
               break;
            case AND_LETTER:
               res.v = (u16) ( res.v & n2.v );
               break;
            case '<':
               res.v = (u16) ( res.v << n2.v );
               break;
            case '>':
               res.v = (u16) ( res.v >> n2.v );
               break;
            default:
               break;
         }
      } else res.v = 0;

      INFER_TYPE( res, n2 );
      INFER_DEFINED( res, n2 );
      skip_white( p );
      op = **p;
   }

   return res;
}


static value term( char **p ) {
   value n2, res;
   char op;

   skip_white( p );
   if ( **p == '-' ) {
      /* unary minus */
      ( *p )++;
      res = product( p );
      res.v = -res.v;
      SET_TYPE( res, TYPE_WORD );
   } else {
      /* unary plus */
      if ( **p == '+' ) {
         ( *p )++;
      }
      res = product( p );
   }

   skip_white( p );
   op = **p;

   while (( op == '+' ) || ( op == '-' ) ||
          ( op == OR_LETTER && *( *p + 1 ) != OR_LETTER ) ||
          ( op == EOR_LETTER )) {
      ( *p )++;
      n2 = product( p );

      if ( DEFINED( res ) && DEFINED( n2 )) {
         switch ( op ) {
            case '+':
               res.v = res.v + n2.v;
               break;
            case '-':
               res.v = res.v - n2.v;
               break;
            case OR_LETTER:
               res.v = res.v | n2.v;
               break;
            case EOR_LETTER:
               res.v = res.v ^ n2.v;
               break;
            default:
               break;
         }
      } else res.v = 0;

      INFER_TYPE( res, n2 );
      INFER_DEFINED( res, n2 );
      skip_white( p );
      op = **p;
   }

   return res;
}


static value conversion( char **p ) {
   value v;

   skip_white( p );
   if ( **p == '>' ) {
      ( *p )++;
      v = term( p );
      SET_TYPE( v, TYPE_BYTE );
      v.v = v.v >> 8;
   } else if ( **p == '<' ) {
      ( *p )++;
      v = term( p );
      SET_TYPE( v, TYPE_BYTE );
      v.v = v.v & 0xff;
   } else if ( starts_with( *p, "[b]" )) {
      /* lossless byte conversion */
      *p += 3;
      v = term( p );
      if ( DEFINED( v ) && v.v > 0xff )
         error( ERR_BYTE_RANGE );
      SET_TYPE( v, TYPE_BYTE );
   } else if ( starts_with( *p, "[w]" )) {
      /* lossless word conversion */
      *p += 3;
      v = term( p );
      SET_TYPE( v, TYPE_WORD );
   } else v = term( p );
   return v;
}


static value comparison( char **p ) {
   value res, n2;
   char op, op2;

   res = conversion( p );

   skip_white( p );
   while (( **p == '=' && *( *p + 1 ) == '=' ) ||
          ( **p == '!' && *( *p + 1 ) == '=' ) ||
          ( **p == '<' && *( *p + 1 ) == '=' ) ||
          ( **p == '>' && *( *p + 1 ) == '=' ) ||
          ( **p == '<' ) || ( **p == '>' )) {
      op = **p;
      op2 = *( *p + 1 );
      *p += 1;
      if ( **p == '=' ) *p += 1;

      n2 = conversion( p );

      if ( DEFINED( res ) && DEFINED( n2 )) {
         switch ( op ) {
            case '=':
               res.v = res.v == n2.v;
               break;
            case '!':
               res.v = res.v != n2.v;
               break;
            case '<':
               res.v = ( op2 == '=' ) ? res.v <= n2.v : res.v < n2.v;
               break;
            case '>':
               res.v = ( op2 == '=' ) ? res.v >= n2.v : res.v > n2.v;
               break;
            default:
               break;
         }
         SET_DEFINED( res );
         if ( res.v ) res.v = 1;
      } else {
         res.v = 0;
         SET_UNDEFINED( res );
      }
      SET_TYPE( res, TYPE_BYTE );
   }

   return res;
}


static value logical_and( char **p ) {
   value res, n2;

   res = comparison( p );

   skip_white( p );
   while (( **p == AND_LETTER && *( *p + 1 ) == AND_LETTER )) {
      *p += 2;

      n2 = comparison( p );

      if ( DEFINED( res ) && DEFINED( n2 )) {
         res.v = ( res.v && n2.v ) ? 1 : 0;
         SET_DEFINED( res );
      } else {
         res.v = 0;
         SET_UNDEFINED( res );
      }
      SET_TYPE( res, TYPE_BYTE );
   }
   return res;
}


static value logical_or( char **p ) {
   value res, n2;

   res = logical_and( p );

   skip_white( p );
   while (( **p == OR_LETTER && *( *p + 1 ) == OR_LETTER )) {
      *p += 2;

      n2 = logical_and( p );

      if ( DEFINED( res ) && res.v != 0 ) {
         res.v = 1;
         SET_DEFINED( res );
      } else {
         if ( DEFINED( n2 ) && n2.v != 0 ) {
            res.v = 1;
            SET_DEFINED( res );
         } else {
            res.v = 0;
            SET_UNDEFINED( res );
         }
      }

      SET_TYPE( res, TYPE_BYTE );
   }
   return res;
}


static value defined_or_else( char **p ) {
   value res, n2;

   res = logical_or( p );

   skip_white( p );
   while (( **p == '?' && *( *p + 1 ) == ':' )) {
      *p += 2;
      n2 = logical_or( p );

      if ( !DEFINED( res )) res = n2;
   }

   return res;
}


static value expr( char **p ) {
   return defined_or_else( p );
}


static void to_uppercase( char *p ) {
   for ( ; *p; p++ ) *p = (char) toupper( *p );
}


static instruction_desc *get_instruction_descr( const char *p ) {
   int l = 0, r = sizeof( instruction_tbl ) / sizeof( instruction_desc ), x;
   int cmp;

   while ( r >= l ) {
      x = l + (( r - l ) >> 2 );
      cmp = strcmp( p, instruction_tbl[x].mn );
      if ( cmp == 0 ) return &instruction_tbl[x];
      else if ( cmp > 0 ) l = x + 1;
      else r = x - 1;
   }
   return NULL;
}


static void emit_byte( u8 b ) {
   if ( pass == 2 ) {
      if ( oc < code_size ) code[oc] = b;
      else error( ERR_PHASE_SIZE );
   }

   oc += 1;
}


static void emit( const char *p, u16 len ) {
   u16 i = 0;

   if ( pass == 2 ) {
      if ( oc - 1 < code_size - len ) {
         for ( i = 0; i < len; i++ ) {
            code[oc + i] = p[i];
         }
      } else error( ERR_PHASE_SIZE );
   }
   oc += len;
}


static void emit_word( u16 w ) {
   if ( pass == 2 ) {
      if ( oc < code_size - 1 ) {
         code[oc] = w & 0xff;
         code[oc + 1] = w >> 8;
      } else error( ERR_PHASE_SIZE );
   }
   oc += 2;
}


/* emit instruction without argument */
static void emit_instr_0( instruction_desc *instr, int am ) {
   if ( pass == 2 ) {
      if ( oc < code_size ) code[oc] = instr->op[am];
      else error( ERR_PHASE_SIZE );
   }
   oc += 1;
}


/* emit instruction with byte argument */
static void emit_instr_1( instruction_desc *instr, int am, u8 o ) {
   if ( pass == 2 ) {
      if ( oc < code_size - 1 ) {
         code[oc] = instr->op[am];
         code[oc + 1] = o;
      } else error( ERR_PHASE_SIZE );
   }
   oc += 2;
}


/* emit instruction with word argument */
static void emit_instr_2( instruction_desc *instr, int am, u16 o ) {
   if ( pass == 2 ) {
      if ( oc < code_size - 2 ) {
         code[oc] = instr->op[am];
         code[oc + 1] = o & 0xff;
         code[oc + 2] = o >> 8;
      } else error( ERR_PHASE_SIZE );
   }
   oc += 3;
}


static int instruction_imp_acc( instruction_desc *instr ) {
   int am = AM_INV;

   if ( instr->op[AM_ACC] != INV ) am = AM_ACC;
   else if ( instr->op[AM_IMP] != INV ) am = AM_IMP;
   else error( ERR_AM );

   emit_instr_0( instr, am );

   return am;
}


static int instruction_imm( char **p, instruction_desc *instr ) {
   int am = AM_IMM;
   value v;

   ( *p )++;
   if ( instr->op[am] == INV ) error( ERR_AM );
   v = expr( p );
   if ( pass == 2 ) {
      if ( UNDEFINED( v )) error( ERR_UNDEF );
   }
   emit_instr_1( instr, am, (u8) to_byte( v ).v );
   return am;
}


static int instruction_rel( instruction_desc *instr, value v ) {
   int am = AM_REL;
   u16 pct = pc + 2u;
   u16 off;

   /* relative branch offsets are in 2-complement */
   /* have to calculate it by hand avoiding implementation defined behaviour */
   /* using unsigned int because int may not be in 2-complement */
   if ( pass == 2 ) {
      if ( UNDEFINED( v )) error( ERR_UNDEF );

      if (( v.v >= pct ) && ((u16) ( v.v - pct ) > 127u )) error( ERR_RELATIVE_RANGE );
      else {
         if (( pct > v.v ) && ((u16) ( pct - v.v ) > 128u )) error( ERR_RELATIVE_RANGE );
      }
   }
   if ( v.v >= pct ) off = v.v - pct;
   else off = (u16) (( ~0u ) - ( pct - v.v - 1u ));
   emit_instr_1( instr, am, off & 0xffu );

   return am;
}


/* handle indirect addressing modes */
static int instruction_ind( char **p, instruction_desc *instr ) {
   char id[ID_LEN];
   int am = AM_INV;
   value v;

   ( *p )++;
   v = expr( p );
   skip_white( p );

   /* indirect X addressing mode? */
   if ( **p == ',' ) {
      skip_curr_and_white( p );
      ident_uppercase( p, id );
      if ( strcmp( id, "X" ) != 0 ) error( ERR_INX );
      am = AM_INX;
      skip_white( p );
      if ( **p != ')' ) error( ERR_CLOSING_PAREN );
      skip_curr_and_white( p );
   } else {
      if ( **p != ')' ) error( ERR_CLOSING_PAREN );
      skip_curr_and_white( p );
      /* indirect Y addressing mode? */
      if ( **p == ',' ) {
         skip_curr_and_white( p );
         ident_uppercase( p, id );
         if ( strcmp( id, "Y" ) != 0 ) error( ERR_INY );
         am = AM_INY;
      } else {
         am = AM_IND;
      }
   }

   if (( instr->op[am] ) == INV ) error( ERR_AM );

   if ( pass == 2 ) {
      if ( UNDEFINED( v )) error( ERR_UNDEF );
      if ((( am == AM_INX ) || am == ( AM_INY )) && (TYPE( v ) != TYPE_BYTE ))
         error( ERR_ILLEGAL_TYPE );
   }

   if ( am == AM_IND ) {
      emit_instr_2( instr, am, v.v );
   } else {
      emit_instr_1( instr, am, (u8) v.v );
   }

   return am;
}


/* handle absolute x and y, zero-page x and y addressing modes */
static int instruction_abxy_zpxy( char **p, instruction_desc *instr, value v ) {
   char id[ID_LEN];
   int am = AM_INV;

   ident_uppercase( p, id );
   /* test for absolute and zero-page X addressing */
   if ( !strcmp( id, "X" )) {
      if ((TYPE( v ) == TYPE_BYTE ) && AM_VALID( *instr, AM_ZPX )) am = AM_ZPX;
      else if ( AM_VALID( *instr, AM_ABX )) am = AM_ABX;
      else error( ERR_AM );
   }
      /* test for absolute and zero-page Y addressing */
   else if ( !strcmp( id, "Y" )) {
      if ((TYPE( v ) == TYPE_BYTE ) && AM_VALID( *instr, AM_ZPY )) am = AM_ZPY;
      else if ( AM_VALID( *instr, AM_ABY )) am = AM_ABY;
      else error( ERR_AM );
   } else error( ERR_AM );

   if ( pass == 2 ) {
      if ( UNDEFINED( v )) error( ERR_UNDEF );
   }

   if (( am == AM_ZPX ) || ( am == AM_ZPY )) {
      emit_instr_1( instr, am, (u8) v.v );
   } else {
      emit_instr_2( instr, am, v.v );
   }

   return am;
}


/* handle absolute and zero-page addressing modes */
static int instruction_abs_zp( instruction_desc *instr, value v ) {
   int am = AM_INV;

   if ((TYPE( v ) == TYPE_BYTE ) && AM_VALID( *instr, AM_ZP )) {
      am = AM_ZP;
      if ( pass == 2 ) {
         if ( UNDEFINED( v )) error( ERR_UNDEF );
      }
      emit_instr_1( instr, am, (u8) v.v );
   } else if ( AM_VALID( *instr, AM_ABS )) {
      am = AM_ABS;
      if ( pass == 2 ) {
         if ( UNDEFINED( v )) error( ERR_UNDEF );
      }
      emit_instr_2( instr, am, v.v );
   } else error( ERR_AM );
   return am;
}


/* process one instruction */
static void instruction( char **p ) {
   char id[ID_LEN];
   instruction_desc *instr;
   int am = AM_INV;
   value v;

   /* first get instruction for given mnemonic */
   ident_uppercase( p, id );
   instr = get_instruction_descr( id );
   if ( !instr ) error( ERR_INSTR );

   /* if found get addressing mode */
   skip_white_and_comment( p );
   if ( IS_END( **p )) {
      am = instruction_imp_acc( instr );
   } else if ( **p == '#' ) {
      am = instruction_imm( p, instr );
   }

      /* handle indirect addressing modes */
   else if ( **p == '(' ) {
      am = instruction_ind( p, instr );
   }

      /* relative and absolute addressing modes */
   else {
      v = expr( p );
      skip_white( p );
      /* relative instruction mode if instruction supports it */
      if ( instr->op[AM_REL] != INV ) {
         am = instruction_rel( instr, v );
      }
         /* else we go through the possible absolute addressing modes */
      else if ( **p == ',' ) {
         skip_curr_and_white( p );
         am = instruction_abxy_zpxy( p, instr, v );
      }
         /* must be absolute or zero-page addressing */
      else {
         am = instruction_abs_zp( instr, v );
      }
   }

   /* update program counter */
   if ( am == AM_INV ) error( ERR_AM );

   pc += am_size[am];
}


static int string_lit( char **p, char *buf, int buf_size ) {
   char *start = *p;

   if ( **p != '"' ) error( ERR_STR );
   ( *p )++;
   while ( **p != '"' ) {
      if ( buf_size && *p - start + 1 >= buf_size ) error( ERR_STRLEN );
      if ( IS_END( **p )) error( ERR_STR_END );
      if ( buf ) *( buf++ ) = **p;
      ( *p )++;
   }
   if ( buf ) *buf = '\0';
   ( *p )++;
   return (int) ( *p - start - 2 );
}


static void directive_byte( char **p ) {
   value v;
   int next, len;
   char *tp;

   do {
      next = 0;
      skip_white( p );

      if ( **p == '"' ) {
         tp = *p + 1;
         len = string_lit( p, NULL, 0 );
         pc += (u16) len;
         emit( tp, (u16) len );
      } else {
         v = expr( p );

         if ( pass == 2 ) {
            if ( UNDEFINED( v )) error( ERR_UNDEF );
            if ( NUM_TYPE( v.v ) != TYPE_BYTE ) error( ERR_NO_BYTE );
         }
         emit_byte((u8) to_byte( v ).v );

         pc++;
      }

      skip_white( p );
      if ( **p == ',' ) {
         skip_curr_and_white( p );
         next = 1;
      }
   } while ( next );
}


static void directive_word( char **p ) {
   value v;
   int next;

   do {
      next = 0;
      skip_white( p );

      v = expr( p );

      if ( pass == 2 ) {
         if ( UNDEFINED( v )) error( ERR_UNDEF );
      }
      emit_word( v.v );

      pc += 2;
      skip_white( p );
      if ( **p == ',' ) {
         skip_curr_and_white( p );
         next = 1;
      }
   } while ( next );
}


static FILE *open_file( const char *fn, const char *mode ) {
   FILE *f;

   f = fopen( fn, mode );
   if ( f ) return f;

   /* TODO: search in different paths for file to open */

   return NULL;
}


static long file_size( FILE *f ) {
   long pos, size;

   pos = ftell( f );
   fseek( f, 0, SEEK_END );
   size = ftell( f );
   fseek( f, pos, SEEK_SET );

   return size;
}


static char *str_copy( const char *src ) {
   char *dst = malloc( strlen( src ) + 1 );
   if ( !dst ) error( ERR_NO_MEM );

   strcpy( dst, src );
   return dst;
}


static asm_file *read_file( const char *fn ) {
   FILE *f;
   asm_file *file;

   long size;
   char *buf;

   for ( file = asm_files; file < asm_files + asm_file_count; file++ ) {
      /* if file is already loaded return it */
      if ( !strcmp( file->filename, fn )) {
         return file;
      }
   }

   /* too many files ? */
   if ( file >= asm_files + MAX_FILES ) return NULL;

   /* read file contents */
   f = open_file( fn, "rb" );
   if ( !f ) return NULL;
   size = file_size( f );
   buf = malloc((size_t) size + 1 );
   if ( !buf ) return NULL;
   fread( buf, 1, size, f );
   buf[size] = '\0';
   fclose( f );

   file->filename = str_copy( fn );
   file->text = buf;
   asm_file_count++;

   return file;
}


static void free_files( void ) {
   asm_file *file = asm_files;

   for ( ; file < asm_files + asm_file_count; file++ ) {
      free( file->filename );
      free( file->text );
   }
}


static void push_pos_stack( asm_file *f, char *pos, unsigned l ) {
   pos_stack *stk;

   if ( pos_stk_ptr >= MAX_POS_STACK ) error( ERR_MAX_INC );
   stk = &pos_stk[pos_stk_ptr];

   stk->file = f;
   stk->pos = pos;
   stk->line = l;
   stk->listing = listing;
   stk->list_statements = list_statements;
   list_statements = listing;
   pos_stk_ptr++;
}


static void pop_pos_stack( char **p ) {
   pos_stack *stk;

   pos_stk_ptr--;
   stk = &pos_stk[pos_stk_ptr];

   current_file = stk->file;
   *p = stk->pos;
   line = stk->line;
   listing = stk->listing;
   list_statements = stk->list_statements;
}


static void directive_include( char **p ) {
   asm_file *file;

   /* read filename */
   skip_white( p );
   string_lit( p, filename_buf, STR_LEN );
   skip_white_and_comment( p );
   if ( !IS_END( **p )) error( ERR_EOL );

   skip_eol( p );

   /* read the include file */
   file = read_file( filename_buf );
   if ( !file ) error_ext( ERR_OPEN, filename_buf );

   /* push current file and position to stk and set pointers to inc file */
   push_pos_stack( current_file, *p, line + 1 );

   current_file = file;
   *p = current_file->text;
   line = 1;
}


static void directive_fill( char **p ) {
   value count, filler;

   count = expr( p );
   if ( UNDEFINED( count )) error( ERR_UNDEF );

   pc += count.v;

   skip_white( p );
   if ( **p == ',' ) {
      /* check for filler value, otherwise fill with zero */
      skip_curr_and_white( p );
      filler = expr( p );
      if ( UNDEFINED( filler )) error( ERR_UNDEF );
      if ( NUM_TYPE( filler.v ) != TYPE_BYTE ) error( ERR_NO_BYTE );
   } else {
      filler.v = 0;
   }

   if ( pass == 2 ) {
      memset( code + oc, filler.v, count.v );
   }
   oc += count.v;
}


static void directive_binary( char **p ) {
   /* syntax: .binary "file"[,skip[,count]] */
   FILE *file;
   unsigned long size;
   value skip, count = { 0 };

   /* read filename */
   skip_white( p );
   string_lit( p, filename_buf, STR_LEN );
   skip_white_and_comment( p );

   file = open_file( filename_buf, "rb" );
   if ( !file ) error( ERR_OPEN );

   size = file_size( file );
   count.v = (u16) size;

   skip_white( p );
   if ( **p == ',' ) {
      skip_curr_and_white( p );
      skip = expr( p );
      skip_white( p );
      if ( **p == ',' ) {
         skip_curr_and_white( p );
         count = expr( p );
      }
   } else skip.v = 0;

   if ( skip.v > size ) {
      fclose( file );
      return;
   }
   if ( skip.v + count.v > (u16) size ) {
      count.v = (u16) size - skip.v;
   }

   if ( pass == 2 ) {
      fseek( file, skip.v, SEEK_SET );
      fread( code + oc, count.v, 1, file );
   }

   pc += count.v;
   oc += count.v;

   fclose( file );
}


static void directive_if( char **p, int positive_logic, int check_defined ) {
   value v;

   if ( if_stack_count >= IF_STATE_MAX ) error( ERR_MAX_IF );

   if_stack[if_stack_count].process_statements = process_statements;

   if ( process_statements ) {
      v = expr( p );
      if ( check_defined ) process_statements = DEFINED( v );
      else process_statements = DEFINED( v ) && v.v != 0;

      if ( !positive_logic ) process_statements = !process_statements;
      if_stack[if_stack_count].condition_met = process_statements;
   } else {
      skip_to_eol( p );
   }

   if_stack_count++;
}


static void directive_else( void ) {
   if ( !if_stack_count ) error( ERR_MISSING_IF );

   if ( if_stack[if_stack_count - 1].process_statements )
      process_statements = !if_stack[if_stack_count - 1].condition_met;
}


static void directive_endif( void ) {
   if ( !if_stack_count ) error( ERR_MISSING_IF );

   if_stack_count--;
   process_statements = if_stack[if_stack_count].process_statements;
}


static void echo( char **p ) {
   value v;
   int next, print_hex;

   do {
      next = 0;
      skip_white( p );

      if ( **p == '"' ) {
         string_lit( p, filename_buf, STR_LEN );
         printf( "%s", filename_buf );
      } else {
         if ( starts_with( *p, "[$]" )) {
            *p += 3;
            print_hex = 1;
         } else print_hex = 0;

         v = expr( p );
         if ( DEFINED( v )) {
            if ( print_hex )
               printf( "$%X", (unsigned) v.v );
            else
               printf( "%u", (unsigned) v.v );
         } else {
            printf( "?" );
         }
      }

      skip_white( p );
      if ( **p == ',' ) {
         skip_curr_and_white( p );
         next = 1;
      }
   } while ( next );

   puts( "" );
}


static void directive_echo( char **p, int on_pass ) {
   /* echo on second pass */
   if ( pass != on_pass ) {
      skip_to_eol( p );
      return;
   }
   echo( p );
}


static void directive_diagnostic( char **p, int level ) {
   /* warnings and errors are processed at pass 1 */
   if ( pass != 1 ) {
      skip_to_eol( p );
      return;
   }

   switch ( level ) {
      case 1: /* warning */
         printf( "%s:%d: warning: ", current_file->filename, line );
         echo( p );
         break;
      case 2: /* error */
         printf( "%s:%d: error: ", current_file->filename, line );
         echo( p );
         error_abort();
         break;
      default:
         break;
   }
}


static void directive_assert( char **p, int on_pass ) {
   value res;

   if ( pass != on_pass ) {
      skip_to_eol( p );
      return;
   }

   res = expr( p );

   if ( UNDEFINED( res ) || res.v == 0 ) {
      printf( "%s:%d: assertion failed: ", current_file->filename, line );
      skip_white( p );
      if ( **p == ',' ) {
         ( *p )++;
         echo( p );
      } else {
         puts( "" );
      }
      error_abort();
   } else {
      skip_to_eol( p );
   }
}


static int directive( char **p ) {
   char id[ID_LEN];
   value v;
   int again = 0;

   ident_uppercase( p, id );

   if ( !strcmp( id, "ORG" )) {
      v = expr( p );
      if ( UNDEFINED( v )) error( ERR_UNDEF );
      pc = v.v;
   } else if ( !strcmp( id, "BYTE" )) {
      directive_byte( p );
   } else if ( !strcmp( id, "WORD" )) {
      directive_word( p );
   } else if ( !strcmp( id, "FILL" )) {
      directive_fill( p );
   } else if ( !strcmp( id, "INCLUDE" )) {
      directive_include( p );
      again = 1;
   } else if ( !strcmp( id, "BINARY" )) {
      directive_binary( p );
   } else if ( !strcmp( id, "ECHO" )) {
      directive_echo( p, 2 );
   } else if ( !strcmp( id, "ECHO1" )) {
      directive_echo( p, 1 );
   } else if ( !strcmp( id, "ASSERT" )) {
      directive_assert( p, 1 );
   } else if ( !strcmp( id, "ASSERT1" )) {
      directive_assert( p, 2 );
   } else if ( !strcmp( id, "ERROR" )) {
      directive_diagnostic( p, 2 );
   } else if ( !strcmp( id, "WARNING" )) {
      directive_diagnostic( p, 1 );
   } else if ( !strcmp( id, "NOLIST" )) {
      listing = 0;
   } else if ( !strcmp( id, "LIST" )) {
      listing = list_statements;
      list_skip_one = 1;
   } else {
      error( ERR_NO_DIRECTIVE );
   }

   return again;
}


static int is_mnemonic( const char *id ) {
   char id_uppercase[ID_LEN];
   int l = 0, r = sizeof( instruction_tbl ) / sizeof( instruction_desc ), x;
   int cmp;
   strcpy( id_uppercase, id );
   to_uppercase( id_uppercase );

   while ( r >= l ) {
      x = l + (( r - l ) >> 2 );
      cmp = strcmp( id_uppercase, instruction_tbl[x].mn );
      if ( cmp == 0 ) return 1;
      else if ( cmp > 0 ) l = x + 1;
      else r = x - 1;
   }
   return 0;
}


/* processes one statement or assembler instruction */
static int statement( char **p ) {
   char id1[ID_LEN];
   value v1;
   char *pt;
   int again = 0;
   enum {
      NONE = 0, GLOBAL_LABEL = 1, LOCAL_LABEL = 2
   } label = NONE;

   skip_white_and_comment( p );
   if ( IS_END( **p )) return 0;
   pt = *p;

   /* first check for variable or label definition */
   if ( **p == LOCAL_LABEL_LETTER ) {
      ( *p )++;
      numbered_ident( p, id1 );
      skip_white( p );
      label = LOCAL_LABEL;
   } else if ( isalpha( **p )) {
      ident( p, id1 );
      skip_white( p );
      label = GLOBAL_LABEL;
   }

   if ( label && **p == '=' ) {       /* variable definition */
      ( *p )++;
      v1 = expr( p );
      if ( label == GLOBAL_LABEL )
         define_variable( id1, v1, NULL);
      else {
         if ( !current_label ) error( ERR_NO_GLOBAL );
         define_variable( id1, v1, current_label );
      }

      return again;
   } else if ( label && (( **p == ':' ) || ( !is_mnemonic( id1 )))) {
      if ( **p == ':' ) ( *p )++;

      if ( label == GLOBAL_LABEL )
         current_label = define_label( id1, pc, NULL);
      else {
         if ( !current_label ) error( ERR_NO_GLOBAL );
         define_label( id1, pc, current_label );
      }

      skip_white_and_comment( p );
      if ( IS_END( **p )) return again;
   } else *p = pt;

   /* check for directive or instruction */
   if ( **p == DIRECTIVE_LETTER ) {
      ( *p )++;
      again = directive( p );
   } else if ( isalpha( **p )) {
      instruction( p );
   } else error( ERR_STMT );

   return again;
}


static void byte_to_pchar( u8 w, char *p ) {
   u16 v;
   v = ( w >> 4 ) & 0xf;
   p[0] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
   v = w & 0xf;
   p[1] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
}


static void word_to_pchar( u16 w, char *p ) {
   u16 v;
   v = ( w >> 12 ) & 0xf;
   p[0] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
   v = ( w >> 8 ) & 0xf;
   p[1] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
   v = ( w >> 4 ) & 0xf;
   p[2] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
   v = w & 0xf;
   p[3] = (char) ( v + '0' + (( v > 9 ) ? 'A' - '9' - 1 : 0 ));
}


static void list_statement( char *statement_start, u16 pc_start,
                            u16 oc_start, char *p, int skipped ) {
   static char list_addr_buf[20] = "           ";
   static char list_code_buf[4] = "   ";
   int count = 0;

   if ( !listing || list_skip_one ) return;

   fprintf( list_file, "%5d", line );
   if ( skipped )
      fputs( "- ", list_file );
   else
      fputs( ": ", list_file );

   if ( oc_start < oc ) {
      /* output program counter, but only if we emitted code */
      word_to_pchar( oc_start, list_addr_buf );
      word_to_pchar( pc_start, list_addr_buf + 5 );
      fputs( list_addr_buf, list_file );
   } else
      fputs( "           ", list_file );

   while ( oc_start < oc && count < 3 ) {
      byte_to_pchar( code[oc_start++] & 0xff, list_code_buf );
      fputs( list_code_buf, list_file );
      count++;
   }

   if ( oc_start + count < oc )
      fputs( "...", list_file );
   else {
      while ( count < 4 ) {
         fputs( "   ", list_file );
         count++;
      }
   }
   fputs( "  ", list_file );
   fwrite( statement_start, 1, (int) ( p - statement_start ), list_file );

   fputs( "\n", list_file );
}


static int sym_cmp_name( const void *a, const void *b ) {
   const symbol *sa, *sb;
   sa = *(const symbol **) a;
   sb = *(const symbol **) b;

   return strcmp( sa->name, sb->name );
}


static int sym_cmp_val( const void *a, const void *b ) {
   const symbol *sa, *sb;
   int res;

   sa = *(const symbol **) a;
   sb = *(const symbol **) b;

   if ( DEFINED( sa->value ) && DEFINED( sb->value )) {
      res = sa->value.v - sb->value.v;
   } else if ( UNDEFINED( sa->value ) && DEFINED( sb->value )) {
      res = -1;
   } else if ( DEFINED( sa->value ) && UNDEFINED( sb->value )) {
      res = 1;
   } else {
      res = 0;
   }

   return ( res != 0 ) ? res : strcmp( sa->name, sb->name );
}


static symbol **symbol_tbl_to_array( void ) {
   symbol **tsym, *tsym2;
   symbol **sym_array, **sym_p;

   sym_array = sym_p = malloc( sizeof( symbol * ) * ((size_t) symbol_count + 1 ));
   if ( !sym_array ) return NULL;

   for ( tsym = symbol_tbl; tsym < symbol_tbl + SYMBOL_TBL_SIZE; tsym++ ) {
      if ( !*tsym ) continue;
      tsym2 = *tsym;
      while ( tsym2 ) {
         *sym_p++ = tsym2;
         tsym2 = tsym2->next;
      }
   }
   *sym_p = NULL;

   return sym_array;
}


static void fill_dots( char *s, int len ) {
   int i;
   for ( i = 0; i < len; i++ ) {
      s[i] = ( i & 1 ) ? '.' : ' ';
   }
}


static void list_symbols( void ) {
   symbol *sym, **sym_array, **sym_p;
   char name_buf[ID_LEN + 1];
   int i;

   name_buf[ID_LEN] = '\0';

   sym_array = symbol_tbl_to_array();
   if ( !sym_array ) return;

   for ( i = 1; i <= 2; i++ ) {
      sym_p = sym_array;

      if ( i == 1 ) {
         fputs( "\n\nS Y M B O L S   B Y   N A M E\n\n", list_file );
         qsort( sym_array, symbol_count, sizeof( symbol * ), sym_cmp_name );
      } else {
         fputs( "\n\nS Y M B O L S   B Y   V A L U E\n\n", list_file );
         qsort( sym_array, symbol_count, sizeof( symbol * ), sym_cmp_val );
      }

      fputs( "NAME                              HEX    DEC  "
             "SYM TYPE  WHERE\n", list_file );
      for ( ; *sym_p; sym_p++ ) {
         sym = *sym_p;

         fill_dots( name_buf, ID_LEN );
         memcpy( name_buf, sym->name, strlen( sym->name ));
         fputs( name_buf, list_file );

         if ( DEFINED( sym->value )) {
            if ( TYPE( sym->value ) == TYPE_BYTE )
               fprintf( list_file, "   %02X  %5u  ", sym->value.v, sym->value.v );
            else
               fprintf( list_file, " %04X  %5u  ", sym->value.v, sym->value.v );
         } else fputs( "    ?      ?  ", list_file );

         if ( sym->kind == KIND_LBL ) fputs( "LBL ", list_file );
         else fputs( "VAR ", list_file );

         if ( sym->value.t == TYPE_BYTE ) fputs( "BYTE  ", list_file );
         else if ( sym->value.t == TYPE_WORD ) fputs( "WORD  ", list_file );
         else fputs( "?     ", list_file );

         if ( sym->filename ) {
            fprintf( list_file, "%s:%d", sym->filename, sym->line );
         }
         fputs( "\n", list_file );
      }
   }

   free( sym_array );
}


static void list_filename( char *fn ) {
   if ( listing ) {
      fprintf( list_file, " FILE: %s\n", fn );
   }
}


static int conditional_statement( char **p ) {
   char id[ID_LEN];
   char *pt = *p;

   skip_white_and_comment( p );

   if ( IS_END( **p )) return 0;
   if ( **p != DIRECTIVE_LETTER ) return 0;
   ( *p )++;
   ident_uppercase( p, id );

   if ( !strcmp( id, "IF" )) {
      directive_if( p, 1, 0 );
      return 1;
   } else if ( !strcmp( id, "IFN" )) {
      directive_if( p, 0, 0 );
      return 1;
   } else if ( !strcmp( id, "IFDEF" )) {
      directive_if( p, 1, 1 );
      return 1;

   } else if ( !strcmp( id, "IFNDEF" )) {
      directive_if( p, 0, 1 );
      return 1;
   } else if ( !strcmp( id, "ELSE" )) {
      directive_else();
      return 1;
   } else if ( !strcmp( id, "ENDIF" )) {
      directive_endif();
      return 1;
   }

   *p = pt;
   return 0;
}


static void do_pass( char **p ) {
   int err;

   char *statement_start;
   asm_file *last_file;
   u16 oc_start;
   u16 pc_start;
   int conditional;

   pc = 0;  /* initialize program counter to zero */
   oc = 0;  /* initialize output counter to zero */

   last_file = current_file;
   line = 1;
   current_label = NULL;
   listing = list_statements;
   process_statements = 1;
   if_stack_count = 0;

   if ( !( err = setjmp( error_jmp ))) {
      while ( **p || pos_stk_ptr > 0 ) {
         conditional = 0;

         if ( current_file != last_file ) {
            /* file changed (start or terminate processing of inc file ) */
            if ( pass == 2 ) list_filename( current_file->filename );
         }

         if ( !**p ) {
            /* pop position from file stack (return from include) if at EOF */
            pop_pos_stack( p );
            continue;
         }

         statement_start = *p;
         pc_start = pc;
         oc_start = oc;

         if ( conditional_statement( p )) {
            conditional = 1;
         } else if ( process_statements ) {
            if ( statement( p )) {
               /* statement returns an "again" flag that is set by the include
                  directive. If true we have to start over again with new file,
                  position and line number */
               continue;
            }
         } else {
            skip_to_eol( p );
         }

         skip_white_and_comment( p );

         if ( !IS_END( **p )) {
            /* every statement ends with a newline. if it is not found here
               it is an error condition */
            error( ERR_EOL );
         }

         if ( pass == 2 )
            list_statement( statement_start, pc_start, oc_start, *p,
                            !conditional && !process_statements );

         skip_eol( p );
         line++;

         list_skip_one = 0;
         last_file = current_file;
      }

      if ( if_stack_count ) error( ERR_MISSING_ENDIF );
   } else {
      if ( error_type == ERROR_NORM )
         printf( "%s:%d: error: %s\n", current_file->filename,
                 line, err_msg[err] );
      else if ( error_type == ERROR_EXT )
         printf( "%s:%d: error: %s %s\n", current_file->filename, line,
                 err_msg[err], error_hint );
   }
}


static int save_code( const char *fn, const u8 *data, int len ) {
   FILE *f = fopen( fn, "wb" );
   if ( !f ) return 0;
   if (( fwrite( data, len, 1, f ) == 0 ) && ( oc != 0 )) {
      fclose( f );
      return 0;
   }
   fclose( f );
   return 1;
}


static int init_listing( char *fn ) {
   time_t t;
   struct tm *tm;
   char ts[80];

   list_file = fopen( fn, "wb" );
   list_statements = list_file != NULL;

   if ( !list_file ) return 0;

   time( &t );
   tm = localtime( &t );
   strftime( ts, sizeof( ts ), "%Y-%m-%d %H:%M", tm );

   fprintf( list_file, "ASM6502        PROGRAM LISTING AND SYMBOL TABLE        "
                       "DATE: %s\n\n"
                       "MAIN INPUT FILE: %s\n\n",
            ts, current_file->filename );
   fprintf( list_file, " Line# Pos  Addr  Code           Source\n" );

   return 1;
}


static char *source_filename = NULL;
static char *listing_filename = NULL;
static char *output_filename = NULL;


static int parse_args( char *argv[] ) {
   char *p;
   value v;

   argv++;
   while ( *argv ) {
      if ( **argv == '/' || **argv == '-' ) {
         if ( !strcmp( *argv + 1, "q" )) flag_quiet++;
         else if ( !strcmp( *argv + 1, "o" )) {
            argv++;
            if ( !*argv ) return 0;
            output_filename = *argv;
         } else if ( !strcmp( *argv + 1, "l" )) {
            argv++;
            if ( !*argv ) return 0;
            listing_filename = *argv;
         } else return 0;
      } else if (( p = strchr( *argv, '=' )) != NULL) {
         /* variable definition */
         *p++ = 0;
         if ( !setjmp( error_jmp )) {
            v = number( &p );
            define_variable( *argv, v, NULL);
         } else return 0;
      } else {
         /* source filename */
         if ( source_filename ) return 0;
         source_filename = *argv;
      }

      argv++;
   }
   return source_filename != NULL && output_filename != NULL;
}


void print_usage( void ) {
   printf(
      "Usage: asm6502 [-q] input -o output [-l listing] [VAR=number]...\n\n"
      "  -q             be quiet, unless an error occurred\n"
      "  -o output      set output file name\n"
      "  -l listing     set optional listing file name\n\n"
      "Variables defined from command line are are known to the assembler\n"
      "when assembling files. The numbers are parsed like number literals\n"
      "in the source code.\n\n"
   );
}


int main( int argc, char *argv[] ) {
   char *source;

   if ( !parse_args( argv )) {
      print_usage();
      return ( argc > 1 ) ? EXIT_FAILURE : EXIT_SUCCESS;
   }

   if ( !strcmp( source_filename, output_filename )) {
      printf( "refuse to overwrite your source ;-)\n" );
      return EXIT_FAILURE;
   }
   if ( listing_filename && ( !strcmp( source_filename, listing_filename ) ||
                              !strcmp( output_filename, listing_filename ))) {
      printf( "refuse to overwrite your files ;-)\n" );
      return EXIT_FAILURE;
   }

   if ( !( current_file = read_file( source_filename ))) {
      printf( "error loading file\n" );
      errors = 1;
      goto ret0;
   }

   /* first assembler pass */
   pass = 1;
   source = current_file->text;
   do_pass( &source );
   code_size = oc;
   if ( errors ) {
      goto ret1;
   }

   if ( listing_filename ) {
      /*initialize listing */
      if ( !init_listing( listing_filename )) {
         printf( "error opening listing file\n" );
         errors = 1;
         goto ret1;
      }
   }

   /* second assembler pass */
   pass = 2;
   source = current_file->text;
   code = malloc( code_size );
   do_pass( &source );

   if ( oc != code_size && !errors ) {
      printf( "error: pass two code size less than pass one code size\n" );
      errors++;
   }

   if ( errors ) {
      goto ret2;
   }

   if ( listing )
      list_symbols();

   if ( !flag_quiet ) {
      printf( "output file %s, %d bytes written\n", output_filename, oc );
      if ( listing_filename )
         printf( "listing written to %s\n", listing_filename );
   }

   if ( !save_code( output_filename, code, oc )) {
      printf( "error saving file\n" );
      errors = 1;
      goto ret2;
   }

   ret2:
   if ( list_file ) fclose( list_file );
   free( code );

   ret1:
   free_files();

   ret0:
   free_symbol_tbl();

   if ( errors ) return EXIT_FAILURE;
   else return EXIT_SUCCESS;
}
