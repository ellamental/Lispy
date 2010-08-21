/******************************************************************************
** scheme
** 
** A simple implementation of Scheme in C
** 
** Based on Bootstrap Scheme by Peter Michaux:
** http://michaux.ca/articles/scheme-from-scratch-introduction
******************************************************************************/

/*
* Bootstrap Scheme - a quick and very dirty Scheme interpreter.
* Copyright (C) 2010 Peter Michaux (http://peter.michaux.ca/)
*
* This program is free software: you can redistribute it and/or
* modify it under the terms of the GNU Affero General Public
* License version 3 as published by the Free Software Foundation.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License version 3 for more details.
*
* You should have received a copy of the GNU Affero General Public
* License version 3 along with this program. If not, see
* <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// Report Error and Terminate
void REPL(void);
#define error(args...) fprintf(stderr, args); printf("\n"); REPL()


/** ***************************************************************************
**                                  Types
******************************************************************************/

typedef enum {THE_EMPTY_LIST, FIXNUM, BOOLEAN, VOID,
              CHARACTER, STRING, SYMBOL, PAIR, 
              PRIMITIVE_PROCEDURE, COMPOUND_PROCEDURE, MACRO} object_type;

typedef struct object {
  object_type type;
  union {
    struct {                                  // FIXNUM
      long value;
    } fixnum;
    struct {                                  // BOOLEAN
      char value;
    } boolean;
    struct {                                  // CHARACTER
      char value;
    } character;
    struct {                                  // STRING
      char *value;
    } string;
    struct {                                  // PAIR
      struct object *car;
      struct object *cdr;
    } pair;
    struct {                                  // SYMBOL
      char *value;
    } symbol;
    struct {                                  // PRIMITIVE_PROCEDURE
      struct object *(*fn) (struct object *arguments);
    } primitive_procedure;
    struct {                                  // COMPOUND_PROCEDURE
      struct object *parameters;
      struct object *body;
      struct object *env;
    } compound_procedure;
    struct {                                  // MACRO
      struct object *transformer;
    } macro;
  } data;
} object;


/* Initialize Variables
**************************************/

object *the_empty_list;
object *False;
object *True;
object *Void;

object *symbol_table;

object *quote_symbol;
object *define_symbol;
object *set_symbol;
object *if_symbol;
object *cond_symbol;
object *else_symbol;
object *lambda_symbol;
object *begin_symbol;
object *let_symbol;
object *define_macro_symbol;
object *test_symbol;

object *the_global_environment;

object *cons(object *car, object *cdr);
object *car(object *pair);
object *cdr(object *pair);

void write(object *obj);

/* Object Allocation
**************************************/

/* no GC so truely "unlimited extent" */
object *alloc_object(void) {
  object *obj;

  obj = malloc(sizeof(object));
  if (obj == NULL) {
    error("out of memory\n");
  }
  return obj;
}


/** ***************************************************************************
**                          Data Type Constructors
******************************************************************************/


/* The Empty List
**************************************/

char is_the_empty_list(object *obj) {
  return obj == the_empty_list;
}


/* BOOLEANs
**************************************/
char is_boolean(object *obj) {
  return obj->type == BOOLEAN;
}

char is_false(object *obj) {
  return obj == False;
}

char is_true(object *obj) {
  return !is_false(obj);
}


/* FIXNUMs
**************************************/

object *make_fixnum(long value) {
  object *obj;

  obj = alloc_object();
  obj->type = FIXNUM;
  obj->data.fixnum.value = value;
  return obj;
}

char is_fixnum(object *obj) {
  return obj->type == FIXNUM;
}


/* CHARACTERs
**************************************/

object *make_character(char value) {
  object *obj;
  
  obj = alloc_object();
  obj->type = CHARACTER;
  obj->data.character.value = value;
  return obj;
}

char is_character(object *obj) {
  return obj->type == CHARACTER;
}


/* STRINGs
**************************************/

object *make_string(char *value) {
  object *obj;
  
  obj = alloc_object();
  obj->type = STRING;
  obj->data.string.value = malloc(strlen(value) + 1);
  if (obj->data.string.value == NULL) {
    error("out of memory\n");
  }
  strcpy(obj->data.string.value, value);
  return obj;
}

char is_string(object *obj) {
  return obj->type == STRING;
}


/* SYMBOLs
**************************************/

object *make_symbol(char *value) {
  object *obj;
  object *element;
  
  // search for the symbol in symbol_table
  element = symbol_table;
  while (!is_the_empty_list(element)) {
    if (strcmp(car(element)->data.symbol.value, value) == 0) {
      return car(element);
    }
    element = cdr(element);
  }
  
  // create a symbol and add it to symbol_table
  obj = alloc_object();
  obj->type = SYMBOL;
  obj->data.symbol.value = malloc(strlen(value) + 1);
  if (obj->data.symbol.value == NULL) {
    error("out of memory\n");
  }
  strcpy(obj->data.symbol.value, value);
  symbol_table = cons(obj, symbol_table);
  return obj;
}

char is_symbol(object *obj) {
  return obj->type == SYMBOL;
}


/* PAIRs
**************************************/

object *cons(object *car, object *cdr) {
  object *obj;
  
  obj = alloc_object();
  obj->type = PAIR;
  obj->data.pair.car = car;
  obj->data.pair.cdr = cdr;
  return obj;
}

char is_pair(object *obj) {
  return obj->type == PAIR;
}

object *car(object *pair) {
  return pair->data.pair.car;
}

object *cdr(object *pair) {
  return pair->data.pair.cdr;
}

object *set_car(object *obj, object* value) {
  obj->data.pair.car = value;
}

object *set_cdr(object *obj, object* value) {
  obj->data.pair.cdr = value;
}

#define caar(obj) car(car(obj))
#define cadr(obj) car(cdr(obj))
#define cdar(obj) cdr(car(obj))
#define cddr(obj) cdr(cdr(obj))
#define caaar(obj) car(car(car(obj)))
#define caadr(obj) car(car(cdr(obj)))
#define cadar(obj) car(cdr(car(obj)))
#define caddr(obj) car(cdr(cdr(obj)))
#define cdaar(obj) cdr(car(car(obj)))
#define cdadr(obj) cdr(car(cdr(obj)))
#define cddar(obj) cdr(cdr(car(obj)))
#define cdddr(obj) cdr(cdr(cdr(obj)))
#define caaaar(obj) car(car(car(car(obj))))
#define caaadr(obj) car(car(car(cdr(obj))))
#define caadar(obj) car(car(cdr(car(obj))))
#define caaddr(obj) car(car(cdr(cdr(obj))))
#define cadaar(obj) car(cdr(car(car(obj))))
#define cadadr(obj) car(cdr(car(cdr(obj))))
#define caddar(obj) car(cdr(cdr(car(obj))))
#define cadddr(obj) car(cdr(cdr(cdr(obj))))
#define cdaaar(obj) cdr(car(car(car(obj))))
#define cdaadr(obj) cdr(car(car(cdr(obj))))
#define cdadar(obj) cdr(car(cdr(car(obj))))
#define cdaddr(obj) cdr(car(cdr(cdr(obj))))
#define cddaar(obj) cdr(cdr(car(car(obj))))
#define cddadr(obj) cdr(cdr(car(cdr(obj))))
#define cdddar(obj) cdr(cdr(cdr(car(obj))))
#define cddddr(obj) cdr(cdr(cdr(cdr(obj))))


/* PRIMITIVE_PROCEDUREs
**************************************/

object *make_primitive_procedure(object *(*fn) (struct object *arguments)) {
  object *obj;
  
  obj = alloc_object();
  obj->type = PRIMITIVE_PROCEDURE;
  obj->data.primitive_procedure.fn = fn;
  return obj;
}

char is_primitive_procedure(object *obj) {
  return obj->type == PRIMITIVE_PROCEDURE;
}


/* COMPOUND_PROCEDUREs
**************************************/

object *make_compound_procedure(object *parameters, object *arguments,
                                object* env) {
  object *obj;
  obj = alloc_object();
  obj->type = COMPOUND_PROCEDURE;
  obj->data.compound_procedure.parameters = parameters;
  obj->data.compound_procedure.body = arguments;
  obj->data.compound_procedure.env = env;
  return obj;
}

char is_compound_procedure(object *obj) {
  return obj->type == COMPOUND_PROCEDURE;
}


/* MACROs
**************************************/

object *make_macro(object *transformer) {
  object *obj;
  obj = alloc_object();
  obj->type = MACRO;
  obj->data.macro.transformer = transformer;
  return obj;
}

char is_macro(object *obj) {
  return obj->type == MACRO;
}

/** ***************************************************************************
**                             ENVIRONMENTs
******************************************************************************/

/* Frames
**************************************/

object *make_frame(object *variables, object *values) {
  return cons(variables, values);
}

object *frame_variables(object *frame) {
  return car(frame);
}

object *frame_values(object *frame) {
  return cdr(frame);
}

void add_binding_to_frame(object *var, object *val, object *frame) {
  set_car(frame, cons(var, car(frame)));
  set_cdr(frame, cons(val, cdr(frame)));
}


/* Environments
**************************************/

char is_the_empty_environment(object *env) {
  return env == the_empty_list;
}

object *current_environment(object *env) {
  return car(env);
}

object *enclosing_environment(object *env) {
  return cdr(env);
}

object *extend_environment(object *vars, object *vals, object *base_env) {
  return cons(make_frame(vars, vals), base_env);
}


/* Variables
**************************************/

object *lookup_variable_value(object *var, object *env) {
  object *frame;
  object *vars;
  object *vals;
  while (!is_the_empty_environment(env)) {
    frame = current_environment(env);
    vars = frame_variables(frame);
    vals = frame_values(frame);
    while (!is_the_empty_list(vars)) {
      if (var == car(vars)) {
        return car(vals);
      }
      vars = cdr(vars);
      vals = cdr(vals);
    }
    env = enclosing_environment(env);
  }
  error("Unbound Variable: %s", var->data.symbol.value);
}

void set_variable_value(object *var, object *val, object *env) {
  object *frame;
  object *vars;
  object *vals;
  
  while (!is_the_empty_environment(env)) {
    frame = current_environment(env);
    vars = frame_variables(frame);
    vals = frame_values(frame);
    while (!is_the_empty_list(vars)) {
      if (var == car(vars)) {
        set_car(vals, val);
        return;
      }
      vars = cdr(vars);
      vals = cdr(vals);
    }
    env = enclosing_environment(env);
  }
  error("Can not set unbound variable: %s\n", var->data.symbol.value);
}

void define_variable(object *var, object *val, object *env) {
  object *frame;
  object *vars;
  object *vals;
  
  frame = current_environment(env);
  vars = frame_variables(frame);
  vals = frame_values(frame);
  
  while (!is_the_empty_list(vars)) {
    if (var == car(vars)) {
      set_car(vals, val);
      return;
    }
    vars = cdr(vars);
    vals = cdr(vals);
  }
  add_binding_to_frame(var, val, frame);
}



/** ***************************************************************************
**                                   Read
******************************************************************************/

char is_delimiter(int c) {
  return isspace(c) || c == EOF ||
     c == '(' || c == ')' ||
     c == '"' || c == ';';
}

char is_initial(int c) {
  return isalpha(c) || c == '*' || c == '/' || c == '>' ||
         c == '<'   || c == '=' || c == '?' || c == '!';
}


/* Peek at Next Character
**************************************/

int peek(FILE *in) {
  int c;

  c = getc(in);
  ungetc(c, in);
  return c;
}

void peek_expected_delimiter(FILE *in) {
  if (!is_delimiter(peek(in))) {
    error("Character not followed by delimiter");
  }
}


/* Remove Whitespace
**************************************/

void remove_whitespace(FILE *in) {
  int c;
    
  while ((c = getc(in)) != EOF) {
    if (isspace(c)) {
      continue;
    }
    else if (c == ';') { /* comments are whitespace also */
      while (((c = getc(in)) != EOF) && (c != '\n'));
      continue;
    }
    ungetc(c, in);
    break;
  }
}


/* Read Characters
**************************************/

/* Read #\newline and #\space characters */
void read_expected_string(FILE *in, char *str) {
  int c;
  
  while (*str != '\0') {
    c = getc(in);
    if (c != *str) {
      error("unexpected character '%c'\n", c);
    }
    str++;
  }
}

object *read_character(FILE *in) {
  int c;
  
  c = getc(in);
  
  switch (c) {
    case EOF:
      error("incomplete character literal");
    /* #\space */
    case 's':
      if (peek(in) == 'p') {
        read_expected_string(in, "pace");
        peek_expected_delimiter(in);
        return make_character(' ');
      }
      break;
    /* #\newline */
    case 'n':
      if (peek(in) == 'e') {
        read_expected_string(in, "ewline");
        peek_expected_delimiter(in);
        return make_character('\n');
      }
      break;
  }
  peek_expected_delimiter(in);
  return make_character(c);
}


/* Read
**************************************/

object *read(FILE *in);

object *read_pair(FILE *in) {
  int c;
  object *car_obj;
  object *cdr_obj;
  
  remove_whitespace(in);
  
  c = getc(in);
  if (c == ')') {
    return the_empty_list;
  }
  ungetc(c, in);
  
  car_obj = read(in);
  
  remove_whitespace(in);
  
  c = getc(in);
  if (c == '.') {  /* read improper list */
    c = peek(in);
    if (!is_delimiter(c)) {
      error("dot not followed by delimiter");
    }
    cdr_obj = read(in);
    remove_whitespace(in);
    c = getc(in);
    if (c != ')') {
      error("Missing trailing right paren");
    }
    return cons(car_obj, cdr_obj);
  }
  else {  /* read list */
    ungetc(c, in);
    cdr_obj = read_pair(in);
    return cons(car_obj, cdr_obj);
  }
}


object *read(FILE *in) {
  int c;                         /* Character from input               */
  short sign = 1;                /* Sign of numerical input            */
  long num = 0;                  /* Accumulator for numerical input    */
  int i;                         /* Counter for strings                */
  #define BUFFER_MAX 1000        /* Maximum length for string/symbols  */
  char buffer[BUFFER_MAX + 1];   /* Buffer to hold strings/symbols     */

  remove_whitespace(in);

  c = getc(in);

  /* FIXNUMs */
  if (isdigit(c) || (c == '-' && (isdigit(peek(in))))) {
    
    /* Handle Negative Numbers */
    if (c == '-') {
      sign = -1;
    }
    else {
      ungetc(c, in);
    }
    
    /* Build a FIXNUM from input */
    while (isdigit(c = getc(in))) {
      num = (num * 10) + (c - '0');
    }
    
    /* Apply the sign of the number */
    num *= sign;
    
    /* Check that the number is followed by a delimiter and return FIXNUM */
    if (is_delimiter(c)) {
      ungetc(c, in);
      return make_fixnum(num);
    }
    else {
      error("number not followed by delimiter\n");
    }
  }
  
  /* BOOLEANs and CHARACTERs */
  else if (c == '#') {
    c = getc(in);

    switch (c) {
      case 't':                  // Transform #t to True
        return True;
      case 'f':                  // Transform #f to False
        return False;
      case '\\':
        return read_character(in);
      /*case '\n':
        error("Newline not allowed immediately following #\\") */
      default:
        error("Unrecognized boolean or character literal");
    }
  }

  /* SYMBOLs */
  else if (is_initial(c) || 
           ((c == '+' || c == '-') && is_delimiter(peek(in)))) {
    i = 0;
    while (is_initial(c) || isdigit(c) || c == '+' || c == '-') {
      if (i < BUFFER_MAX) {
        buffer[i++] = c;
      }
      else {
        error("Symbol too long.  Maximum length is %d\n", BUFFER_MAX);
      }
      c = getc(in);
    }
    if (is_delimiter(c)) {
      buffer[i] = '\0';
      ungetc(c, in);
      return make_symbol(buffer);
    }
    else {
      error("Symbol not followed by delimiter.");
    }
  }
  
  /* STRINGs */
  else if (c == '"') {
    i = 0;
    while ((c = getc(in)) != '"') {
      /* Newline Escape Character */
      if (c == '\\') {
        c = getc(in);
        if (c == 'n') {c = '\n';}
      }
      if (c == EOF) {
        error("Non-terminated string");
      }
      if (i < BUFFER_MAX) {
        buffer[i++] = c;
      }
      else {
        error("String too long.  Maximum length is %i", BUFFER_MAX);
      }
    }
    buffer[i] = '\0';
    return make_string(buffer);
  }
      
  /* Pairs */
  else if (c == '(') {
    return read_pair(in);
  }
  
  /* Quote */
  else if (c == '\'') {
    return cons(quote_symbol, cons(read(in), the_empty_list));
  }

  /* EOF */
  else if (c == EOF) {
    return NULL;
  }

  /* UNRECOGNIZED INPUT */
  else {
    error("bad input. Unexpected '%c'\n", c);
  }
  error("read illegal state\n");
}



/** ***************************************************************************
**                               Evaluate
******************************************************************************/

/* Self-Evaluating
**************************************/

char is_self_evaluating(object *exp) {
  return is_boolean(exp)   ||
         is_fixnum(exp)    ||
         is_character(exp) ||
         is_string(exp)    ||
         exp->type == VOID;
}


/* Primitive Syntax?
**************************************/

char is_primitive_syntax(object *expression, object *identifier) {
  object *the_car;
  
  if (is_pair(expression)) {
    the_car = car(expression);
    return is_symbol(the_car) && (the_car == identifier);
  }
  return 0;
}


/* quote
**************************************/

char is_quoted(object *expression) {
  return is_primitive_syntax(expression, quote_symbol);
}

object *text_of_quotation(object *exp) {
  return cadr(exp);
}

object *quote_macro_arguments(object *exp) {
  return cons(quote_symbol, 
              cons(exp, 
                   the_empty_list));
}


/* set!
**************************************/

char is_assignment(object *exp) {
  return is_primitive_syntax(exp, set_symbol);
}

object *assignment_variable(object *exp) {
  return car(cdr(exp));
}

object *assignment_value(object *exp) {
  return car(cdr(cdr(exp)));
}


/* lambda
**************************************/

object *make_lambda(object *parameters, object *body) {
  return cons(lambda_symbol, cons(parameters, body));
}

char is_lambda(object *exp) {
  return is_primitive_syntax(exp, lambda_symbol);
}

object *lambda_parameters(object *exp) {
  return cadr(exp);
}

object *lambda_body(object *exp) {
  return cddr(exp);
}

char is_last_exp(object *seq) {
  return is_the_empty_list(cdr(seq));
}


/* define
**************************************/


char is_definition(object *exp) {
  return is_primitive_syntax(exp, define_symbol);
}

object *definition_variable(object *exp) {
  if (is_symbol(cadr(exp))) {
    return cadr(exp);
  }
  else {
    return caadr(exp);
  }
}

object *definition_value(object *exp) {
  if (is_symbol(cadr(exp))) {
    return caddr(exp);
  }
  else {
    return make_lambda(cdadr(exp), cddr(exp));
  }
}


/* if
**************************************/

char is_if(object *expression) {
  return is_primitive_syntax(expression, if_symbol);
}

object *make_if(object *test, object *consequent, object *alternative) {
  return cons(if_symbol,
              cons(test,
                   cons(consequent,
                        cons(alternative,
                             the_empty_list))));
}


/* cond
**************************************/

object *make_cond(object *clauses) {
  object *first = car(clauses);
  object *rest = cdr(clauses);
  
  if (clauses == the_empty_list) {
    return False;
  }
  else if (car(first) == else_symbol) {
    return cadr(first);            // Does not check that else is last clause
  }
  else {
    return make_if(car(first), cadr(first), make_cond(rest));
  }
}


/* let
**************************************/

object *make_let(object *exp) {
  object *bindings = car(exp);
  object *first_binding = car(bindings);
  object *body = cdr(exp);
  object *params = the_empty_list;
  object *values = the_empty_list;
  
  while (bindings != the_empty_list) {
    params = cons(car(first_binding), params);
    values = cons(cadr(first_binding), values);
    bindings = cdr(bindings);
    first_binding = car(bindings);
  }
  
  return cons(make_lambda(params, body), values);
}


/* test
**************************************/
object *h_equalp(object *obj_1, object *obj_2);
object *eval(object *exp, object *env);

object *test(object *exp) {
  object *test_case;
  object *expected;
  object *result;
  object *env = extend_environment(the_empty_list,
                                   the_empty_list,
                                   the_global_environment);

  while (exp != the_empty_list) {
    test_case = car(exp);
    expected = caddr(exp);

    result = h_equalp(eval(test_case, env), eval(expected, env));
    
    if (result == False) {
      write(test_case);
      printf("\n!= ");
      write(expected); printf("\n");
    }
    exp = cdddr(exp);
  }
  return Void;
}

/* Application of Primitive Procedures
**************************************/

char is_application(object *exp) {
  return is_pair(exp);
}


/* eval arguments
**************************************/


object *list_of_values(object *exps, object *env) {
  if (is_the_empty_list(exps)) {
    return the_empty_list;
  }
  else {
    return cons(eval(car(exps), env),
                list_of_values(cdr(exps), env));
  }
}

object *h_length(object *obj);    // h_length is used to check length of arguments


/* eval
**************************************/

object *eval(object *exp, object *env) {
  object *procedure;
  object *arguments;

tailcall:

  /**  Self-Evaluating  **/
  if (is_self_evaluating(exp)) {
    return exp;
  }

  /**  Symbol  **/
  else if (is_symbol(exp)) {
    return lookup_variable_value(exp, env);
  }

  /**  quote  **/
  else if (is_quoted(exp)) {
    return text_of_quotation(exp);
  }

  /**  set!  **/
  else if (is_assignment(exp)) {
    set_variable_value(assignment_variable(exp),
                       eval(assignment_value(exp), env),
                       env);
    return Void;
  }

  /**  define  **/
  else if (is_definition(exp)) {
    define_variable(definition_variable(exp),
                    eval(definition_value(exp), env),
                    env);
    return Void;
  }

  /**  if  **/
  else if (is_if(exp)) {
    exp = is_true(eval(cadr(exp), env)) ?
            caddr(exp) :
            cadddr(exp);
    goto tailcall;
  }

  /**  cond  **/
  else if (is_primitive_syntax(exp, cond_symbol)) {
    exp = make_cond(cdr(exp));
    goto tailcall;
  }

  /**  lambda  **/
  else if (is_lambda(exp)) {
    return make_compound_procedure(cadr(exp), cddr(exp), env);
  }

  /**  begin  **/
  else if (is_primitive_syntax(exp, begin_symbol)) {
    exp = cdr(exp);
    while (!is_last_exp(exp)) {
      eval(car(exp), env);
      exp = cdr(exp);
    }
    exp = car(exp);
    goto tailcall;
  }
  
  /**  let  **/
  else if (is_primitive_syntax(exp, let_symbol)) {
    exp = make_let(cdr(exp));
    goto tailcall;
  }
  
  /**  define-macro  **/
  else if (is_primitive_syntax(exp, define_macro_symbol)) {
    define_variable(cadr(exp), make_macro(caddr(exp)), env);
    return Void;
  }

  /**  test  **/
  else if (is_primitive_syntax(exp, test_symbol)) {
    return test(cdr(exp));
  }

  /**  Application  **/
  else if (is_application(exp)) {
    procedure = eval(car(exp), env);
    //arguments = list_of_values(cdr(exp), env);
    
    /**  Primitive  **/
    if (is_primitive_procedure(procedure)) {
      return (procedure->data.primitive_procedure.fn)(list_of_values(cdr(exp), env));
    }
    
    /**  Compound  **/
    else if (is_compound_procedure(procedure)) {
      // Check argument length (< = error) (> = extra arguments are not used)
      if (h_length(cdr(exp))->data.fixnum.value < h_length(procedure->data.compound_procedure.parameters)->data.fixnum.value) {
        error("Not enough arguments");}
      
      env = extend_environment(procedure->data.compound_procedure.parameters,
                               list_of_values(cdr(exp), env),
                               procedure->data.compound_procedure.env);
      
      // Transform lambda body into begin form
      exp = cons(begin_symbol, procedure->data.compound_procedure.body);
      goto tailcall;
    }
    
    /**  Macro  **/ 
    else if (is_macro(procedure)) {
      // Expand macro by passing the arguments to the transformer unevaluated
      exp =  eval(cons(procedure->data.macro.transformer, 
                       cons(quote_macro_arguments(cdr(exp)),
                            the_empty_list)),
                  env);
      goto tailcall;
    }
    
    else {
      error("Unknown procedure type\n");
    }
  }
  
  else {
    error("cannot eval unknown expression");
    exit(1);
  }
  
  error("eval illegal state\n");
}


/** ***************************************************************************
**                                 Print
******************************************************************************/


void write_pair(object *pair) {
  object *car_obj = car(pair);
  object *cdr_obj = cdr(pair);

  write(car_obj);
  if (is_pair(cdr_obj)) {
    printf(" ");
    write_pair(cdr_obj);
  }
  else if (is_the_empty_list(cdr_obj)) {
    return;
  }
  else {
    printf(" . ");
    write(cdr_obj);
  }
}
  
  
void write(object *obj) {
  char c;
  char *str;
  
  switch (obj->type) {
    case FIXNUM:                                      // FIXNUM
      printf("%ld", obj->data.fixnum.value);
      break;
      
    case BOOLEAN:                                     // BOOLEAN
      printf("%s", is_false(obj) ? "False" : "True");
      break;
      
    case CHARACTER:                                   // CHARACTER
      c = obj->data.character.value;
      printf("#\\");
      switch (c) {
        case '\n':
          printf("newline");
          break;
        case ' ':
          printf("space");
          break;
        default:
          putchar(c);
      }
      break;
      
    case STRING:                                      // STRING
      str = obj->data.string.value;
      putchar('"');
      while (*str != '\0') {
        switch (*str) {
          case '\n':
            printf("\\n");
            break;
          case '\\':
            printf("\\\\");
            break;
          case '"':
            printf("\\\"");
            break;
          default:
            putchar(*str);
        }
        str++;
      }
      putchar('"');
      break;

    case THE_EMPTY_LIST:                              // THE_EMPTY_LIST
      printf("()");
      break;
      
    case SYMBOL:                                      // SYMBOL
      printf("%s", obj->data.symbol.value);
      break;
      
    case PAIR:                                        // PAIR
      printf("(");
      write_pair(obj);
      printf(")");
      break;

    case PRIMITIVE_PROCEDURE:                         // PRIMITIVE_PROCEDURE
      printf("#<primitive>");
      break;
      
    case COMPOUND_PROCEDURE:                          // COMPOUND_PROCEDURE
      printf("#<procedure> ");
      write(obj->data.compound_procedure.parameters); printf("  ");
      write(obj->data.compound_procedure.body);
      break;
    
    case MACRO:                                       // MACRO
      printf("#<macro> ");
      write(obj->data.macro.transformer);
      break;
      
    case VOID:                                        // VOID
      break;

    default:
      error("cannot write unknown type\n");
    }
}

/** ***************************************************************************
**                           Primitive Procedures
*******************************************************************************
** Primitive procedures are prefixed with p_
** Helper procedures are prefixed with h_ and are for internal use.
** Predicates (like equal? or number?) end with a p (p_equalp, p_numberp)
**/

/*  I/O
*************************************************/

//  print

object *p_print(object *arguments) {
  while (!is_the_empty_list(arguments)) {
    object *obj;
    
    obj = car(arguments);
    switch (obj->type) {
      case STRING:
        printf("%s", obj->data.string.value);
        break;
      case CHARACTER:
        printf("%c", obj->data.character.value);
        break;
      default:
        write(obj);
    }
    arguments = cdr(arguments);
  }
  return Void;
}

//  load

object *p_load(object *arguments) {
  char *filename;
  FILE *in;
  object *exp;
  object *result;
  
  filename = car(arguments)->data.string.value;
  in = fopen(filename, "r");
  if (in == NULL) {
    error("could not load file \"%s\"", filename);
  }
  while ((exp = read(in)) != NULL) {
    result = eval(exp, the_global_environment);
  }
  fclose(in);
  return result;
}

/*  List Procedures
************************************************/

//  list

object *p_list(object *arguments) {
  return arguments;
}

//  null?

object *p_nullp(object *arguments) {
  if (is_the_empty_list(car(arguments))) {
    return True;}
  else {return False;}
}

//  cons

object *p_cons(object *arguments) {
  return cons(car(arguments), cadr(arguments));
}

//  car

object *p_car(object *arguments) {
  return car(car(arguments));
}

//  cdr

object *p_cdr(object *arguments) {
  return cdr(car(arguments));
}

//  length

object *h_length(object *lst) {
  int count = 1;
  
  if (lst == the_empty_list) {
    return make_fixnum(0);
  }
  while (cdr(lst) != the_empty_list) {
    count += 1;
    lst = cdr(lst);
  }
  return make_fixnum(count);
}

object *p_length(object *arguments) {
  h_length(car(arguments));
}


/*  Equality Procedures
************************************************/

//  eqv?

object *p_eqvp(object *arguments) {
  object *obj_1;
  object *obj_2;
  
  obj_1 = car(arguments);
  obj_2 = cadr(arguments);
  
  if (obj_1->type != obj_2->type) {
    return False;
  }
  
  switch (obj_1->type) {
    
    case VOID:
    case THE_EMPTY_LIST:
      return True;
      break;
    
    case FIXNUM:
      return (obj_1->data.fixnum.value == 
              obj_2->data.fixnum.value) ? 
              True : False;
      break;
      
    case CHARACTER:
      return (obj_1->data.character.value ==
              obj_2->data.character.value) ?
              True : False;
      break;
    
    case SYMBOL:
      return (obj_1->data.symbol.value ==
              obj_2->data.symbol.value) ?
              True : False;
      break;
    
    case PRIMITIVE_PROCEDURE:
    case COMPOUND_PROCEDURE:
    case BOOLEAN: 
    case STRING:
    case PAIR:
      return (obj_1 == obj_2) ? True : False;
      break;
  }
}


//  equal?

object *h_equalp(object *obj_1, object *obj_2) {
  object *result;
  object *temp_1;
  object *temp_2;
  
  if (obj_1->type != obj_2->type) {
    return False;
  }

  switch (obj_1->type) {
    case VOID:
    case THE_EMPTY_LIST:
      return True;
      break;
    
    case FIXNUM:
      return (obj_1->data.fixnum.value == 
              obj_2->data.fixnum.value) ? 
              True : False;
      break;
      
    case CHARACTER:
      return (obj_1->data.character.value ==
              obj_2->data.character.value) ?
              True : False;
      break;
    
    case SYMBOL:
      return (obj_1->data.symbol.value ==
              obj_2->data.symbol.value) ?
              True : False;
      break;
    
    case PRIMITIVE_PROCEDURE:
    case COMPOUND_PROCEDURE:
    case BOOLEAN:
      return (obj_1 == obj_2) ? True : False;
    
    case STRING:
      return !strcmp(obj_1->data.string.value, obj_2->data.string.value) ? 
             True : False;
      break;
    
    case PAIR:
      if (h_length(obj_1)->data.fixnum.value != 
          h_length(obj_2)->data.fixnum.value) {
        return False;}
      while (obj_1 != the_empty_list) {
        if (h_equalp(car(obj_1), car(obj_2)) == True) {
          obj_1 = cdr(obj_1);
          obj_2 = cdr(obj_2);
        }
        else {
          return False;
        }
      }
      return True;
          
    default:
      error("Unsupported types for equal?");
  }
}

object *p_equalp(object *arguments) {
  h_equalp(car(arguments), cadr(arguments));
}


//  not

object *p_not(object *exp) {
  return (car(exp) == False) ? True : False;
}


/*  Numeric Procedures
************************************************/

//  =

object *p_numeric_equal(object *arguments) {
  if (car(arguments)->data.fixnum.value == cadr(arguments)->data.fixnum.value) {
    return True;
  }
  else {
    return False;
  }
}


//  +

object *p_add(object *arguments) {
  long result = 0;
  
  while (!is_the_empty_list(arguments)) {
    result += (car(arguments))->data.fixnum.value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}


//  -

object *p_sub(object *arguments) {
  long result = (car(arguments))->data.fixnum.value;
  arguments = cdr(arguments);
  while (!is_the_empty_list(arguments)) {
    result -= (car(arguments))->data.fixnum.value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}


//  *

object *p_mul(object *arguments) {
  long result = 1;

  while (!is_the_empty_list(arguments)) {
    result *= (car(arguments))->data.fixnum.value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}


//  /

object *p_div(object *arguments) {
  long result = (car(arguments))->data.fixnum.value;
  arguments = cdr(arguments);
  
  while (!is_the_empty_list(arguments)) {
    result /= (car(arguments))->data.fixnum.value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}


//  >

object *p_greater_than(object *arguments) {
  if (car(arguments)->data.fixnum.value > cadr(arguments)->data.fixnum.value) {
    return True;}
  else {return False;}
}


//  <

object *p_less_than(object *arguments) {
  if (car(arguments)->data.fixnum.value < cadr(arguments)->data.fixnum.value) {
    return True;}
  else {return False;}
}


//  >=

object *p_greater_than_or_eq(object *arguments) {
  if (car(arguments)->data.fixnum.value >= cadr(arguments)->data.fixnum.value) {
    return True;}
  else {return False;}
}


//  <=

object *p_less_than_or_eq(object *arguments) {
  if (car(arguments)->data.fixnum.value <= cadr(arguments)->data.fixnum.value) {
    return True;}
  else {return False;}
}


/*  Type Procedures
************************************************/

//  type

object *h_type(object *obj) {
  
  switch (obj->type) {
    case THE_EMPTY_LIST:
      return cons(make_string("'()"), the_empty_list);
    
    case BOOLEAN:
      return cons(make_string("boolean"), the_empty_list);
      
    case CHARACTER:
      return cons(make_string("character"), the_empty_list);
      
    case SYMBOL:
      return cons(make_string("symbol"), the_empty_list);
      
    case FIXNUM:
      return cons(make_string("number"), cons(make_string("integer"), the_empty_list));
      
    case PRIMITIVE_PROCEDURE:
      return cons(make_string("procedure"), cons(make_string("primitive"), the_empty_list));
    
    case COMPOUND_PROCEDURE:
      return cons(make_string("procedure"), cons(make_string("compound"), the_empty_list));
 
    case STRING:
      return cons(make_string("sequence"), cons(make_string("string"), the_empty_list));
    
    case PAIR:
      return cons(make_string("sequence"), cons(make_string("pair"), the_empty_list));
  }
}

object *p_type(object *arguments) {
  h_type(car(arguments));
}

//  type?

object *p_typep(object *arguments) {
  h_equalp(h_type(car(arguments)), h_type(cadr(arguments)));
}

// number->string

object *p_number_to_string(object *arguments) {
  object *number;
  char buf[15];                  // is this a large enough buffer?  auto-calc size?
  number = car(arguments);

  if (cdr(arguments) == the_empty_list) {
    sprintf(buf, "%ld", number->data.fixnum.value);
    return make_string(buf);
  }
  else {
    error("Bases other than 10 not supported yet");
    //itoa(number->data.fixnum.value, buf, (cadr(arguments))->data.fixnum.value);
    //return make_string(buf);
  }
}

//  string->number

object *p_string_to_number(object *arguments) {
  return make_fixnum(atol(car(arguments)->data.string.value));
}

//  symbol->string

object *p_symbol_to_string(object *arguments) {
  return make_string(car(arguments)->data.symbol.value);
}

//  string->symbol

object *p_string_to_symbol(object *arguments) {
  return make_symbol(car(arguments)->data.string.value);
}

//  char->integer

object *p_char_to_integer(object *arguments) {
  return make_fixnum(car(arguments)->data.character.value);
}

//  integer->char

object *p_integer_to_char(object *arguments) {
  return make_character(car(arguments)->data.fixnum.value);
}



/** ***************************************************************************
**                                   REPL
******************************************************************************/


void init(void) {
  the_empty_list = alloc_object();
  the_empty_list->type = THE_EMPTY_LIST;

  the_global_environment = extend_environment(the_empty_list,
                                              the_empty_list,
                                              the_empty_list);

  False = alloc_object();
  False->type = BOOLEAN;
  False->data.boolean.value = 0;
  
  True = alloc_object();
  True->type = BOOLEAN;
  True->data.boolean.value = 1;
  
  Void = alloc_object();
  Void->type = VOID;

}

void populate_global_environment(void) {
  
  symbol_table = the_empty_list;
 
  /* Self-evaluating Symbols
  **********************************/
  define_variable(make_symbol("False"), False, the_global_environment);
  define_variable(make_symbol("True"), True, the_global_environment);
  define_variable(make_symbol("void"), Void, the_global_environment);

  
  /* Primitive Forms
  **********************************/
  quote_symbol = make_symbol("quote");
  set_symbol = make_symbol("set!");
  define_symbol = make_symbol("define");
  if_symbol = make_symbol("if");
  cond_symbol = make_symbol("cond");
  else_symbol = make_symbol("else");
  lambda_symbol = make_symbol("lambda");
  begin_symbol = make_symbol("begin");
  let_symbol = make_symbol("let");
  define_macro_symbol = make_symbol("define-macro");
  test_symbol = make_symbol("test");

  
  /* Primitive Procedures
  **********************************/
  #define add_procedure(scheme_name, c_name)       \
    define_variable(make_symbol(scheme_name),      \
                    make_primitive_procedure(c_name),   \
                    the_global_environment);

  // I/O Procedures
  add_procedure("print", p_print);
  add_procedure("load",  p_load);
  
  
  // List Procedures
  add_procedure("list",   p_list);
  add_procedure("null?",  p_nullp);
  add_procedure("cons",   p_cons);
  add_procedure("car",    p_car);
  add_procedure("cdr",    p_cdr);
  add_procedure("length", p_length);

  
  // Equality Procedures
  add_procedure("eqv?",   p_eqvp);
  add_procedure("equal?", p_equalp);
  add_procedure("not",    p_not);
  
  
  // Numeric Procedures
  add_procedure("=", p_numeric_equal);
  add_procedure("+", p_add);
  add_procedure("-", p_sub);
  add_procedure("*", p_mul);
  add_procedure("/", p_div);

  add_procedure(">", p_greater_than);
  add_procedure("<", p_less_than);
  add_procedure(">=", p_greater_than_or_eq);
  add_procedure("<=", p_less_than_or_eq);

  
  // Type Procedures
  add_procedure("type",           p_type);
  add_procedure("type?",          p_typep);

  add_procedure("number->string", p_number_to_string);
  add_procedure("string->number", p_string_to_number);
  add_procedure("symbol->string", p_symbol_to_string);
  add_procedure("string->symbol", p_string_to_symbol);
  
  add_procedure("char->integer", p_char_to_integer);
  add_procedure("integer->char", p_integer_to_char);
  
  // Character Procedures
  
}


/* REPL
**************************************/

void REPL(void) {
  object *input;
  object *output;
  
  while (1) {
    printf("> ");
    input = read(stdin);
    output = eval(input, the_global_environment);
    if (output != Void) {
      write(output);
      printf("\n");
    }
  }
}

int main(void) {

  printf("****************************************\n"
         "**           Basic Scheme             **\n"
         "**           Version 0.01             **\n"
         "**                                    **\n"
         "** Use ctrl-c to exit                 **\n"
         "****************************************\n");

  init();
  populate_global_environment();
  
  // Load and run unit tests
  p_load(cons(make_string("/home/trades/Documents/scheme/unit_test.scm"),
              the_empty_list));
  
  REPL();
  
  return 0;
}

