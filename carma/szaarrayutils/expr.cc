#include <string.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"

#include "carma/szautil/Exception.h"

/*
 * Declare the context of a push-variable-onto-stack instruction.
 */
typedef struct {
  Variable *var;      /* The variable to be pushed onto the stack */
} LoadVarOper;

/*
 * Declare the context of an evaluate-function instruction.
 */
typedef struct {
  Function *func;     /* The declaration of the function */
  Variable *result;   /* The return value of the function */
  Variable *data;     /* NULL, or the state data of a loop-state function */
} FuncOper;

/*
 * Declare the contents of a list-assembly instruction.
 */
typedef struct {
  unsigned n;         /* The number of variables to place in the list */
  Variable *var;      /* An output list variable of the appropriate type */
                      /*  An empty list is allocated to var->data.list */
                      /*  when the instruction is created */
} ListOper;

/*
 * Declare the contents of a group-assembly instruction.
 */
typedef struct {
  unsigned n;         /* The number of variables to place in the group */
  Variable *var;      /* An output group variable of the appropriate type */
                      /*  An empty list is allocated to var->list */
                      /*  when the instruction is created */
} GroupOper;

/*
 * Declare the contents of a load-field instruction.
 */
typedef struct {
  unsigned field;     /* The sequential index of the field in the group */
} FieldOper;

/*
 * Declare the content of a clone-variable instruction.
 * The clone scoperator is used where needed to prevent aliasing problems.
 */
typedef struct {
  Variable *var;      /* The result of the clone scoperator */
} CloneOper;

/*
 * Declare the content of an scoperator instruction.
 */
typedef struct {
  ScOperator *oper;    /* The scoperator to be evaluated */
  Variable *result;  /* The return value of the scoperator */
} OpFnOper;

/*
 * Declare the context of a pop-variable-and-copy instruction.
 */
typedef struct {
  Variable *var;      /* The destination variable of the copy instruction */
} StoreVarOper;

/*
 * Declare the context of a unset-variable instruction.
 */
typedef struct {
  Variable *var;      /* The variable whose VAR_IS_NUL flag is to be set */
} UnsetVarOper;

/*
 * Declare the context of a set-boolvar-modifier instruction.
 */
typedef struct {
  Variable *var;      /* The boolean variable to be set */
  int state;          /* The boolean value to give the variable */
} SetBoolOper;

/*
 * Declare the context of a set-string-modifier instruction.
 */
typedef struct {
  Variable *var;      /* The boolean variable to be set */
  char *string;         /* The string value to give the variable */
} SetStringOper;

/*
 * Expressions are recorded as a list of instructions that operate
 * on the stack of the containing expression. Most of these instructions
 * are implemented by functions that pop one or more values from the
 * top of the stack, compute something with them and push a single result
 * onto the stack.
 */
typedef enum {
  OPER_LOAD,          /* Push a given variable onto the stack */
  OPER_FUNC,          /* Execute the given function using whatever values */
                      /*  are currently in func->args. Push the return value */
                      /*  of the function onto the stack. Note that */
                      /*  parse_procedure_arguments() arranges for the */
                      /*  function argument values to be assigned to */
                      /*  func->args before it emits this scoperator. */
  OPER_LIST,          /* Create a list variable from the top n variables */
                      /*  on the stack */
  OPER_GROUP,         /* Create a group variable from the appropriate */
                      /*  number of variables at the top of the stack */
  OPER_FIELD,         /* Replace the group variable at the top */
                      /*  of the stack with the value of one of its fields. */
  OPER_CLONE,         /* Replace the scalar variable at the top of the stack */
                      /*  with a new variable of the same type and value */
  OPER_OPER,          /* Pop a given number of variables from the stack, */
                      /*  arrange them into a list and pass the list to a */
                      /*  given scoperator function. Push the return value of */
                      /*  the scoperator onto the stack. */
  OPER_SKIP,          /* If the boolvar variable at the top of the stack is true */
                      /*  skip the remaining instructions */
  OPER_STORE,         /* Pop one variable from the stack and copy its value */
                      /*  to a specified variable. */
  OPER_UNSET,         /* Set the VAR_IS_NUL flag of a specified variable. */
  OPER_SET_BOOL,      /* Set the value of a given boolean variable */
  OPER_SET_STRING     /* Set the value of a given string variable */
} OperType;

struct ExprOper {            /* This is typedef'd to ExprOper in script.h */
  ScriptObj header;          /* The generic script-object header */
  OperType type;             /* The type of instruction */
  union {
    LoadVarOper load;        /* type = OPER_LOAD */
    FuncOper func;           /* type = OPER_FUNC */
    ListOper list;           /* type = OPER_LIST */
    GroupOper group;         /* type = OPER_GROUP */
    FieldOper field;         /* type = OPER_FIELD */
    CloneOper clone;         /* type = OPER_CLONE */
    OpFnOper oper;           /* type = OPER_OPER */
    StoreVarOper store;      /* type = OPER_STORE */
    UnsetVarOper unset;      /* type = OPER_UNSET */
    SetBoolOper set_boolvar;    /* type = OPER_SET_BOOL */
    SetStringOper set_string;/* type = OPER_SET_STRING */
  } context;
};

/*
 * The following type is used to contain compiled expressions and
 * support their execution.
 */
struct Expr {          /* This is typedef'd to Expr in script.h */
  ScriptObj header;    /* The generic script-object header */
  VariableList *stack; /* The execution stack of the expression */
  VariableList *args;  /* This is where pop_ExprStackArgs(...,narg) */
                       /*  assembles an argument list of the top narg */
                       /*  variables from the stack */
  ExprOperList *ops;   /* The instructions that implement the expression */
};

static ListNode *push_ExprStack(Script *sc, Expr *expr, Variable *var);

static TypeSpec *parse_variable_ref(Script *sc, DataType *dt, int is_list,
			      TypeSpec *type, InputStream *stream, Expr *e);
static TypeSpec *parse_function_call(Script *sc, DataType *dt, int is_list,
			       Function *func, InputStream *stream, Expr *e);
static TypeSpec *parse_cast_expr(Script *sc, DataType *dt, int is_list,
				 DataType *cast, InputStream *stream, Expr *e);
static int parse_list_expr(Script *sc, DataType *dt, InputStream *stream,
			   Expr *e);
static int parse_list_const(Script *sc, DataType *dt, InputStream *stream,
			    Expr *e);
static Variable *top_ExprStack(Script *sc, Expr *expr);

static int exe_FuncOper(Script *sc, Expr *expr, FuncOper *op);
static int exe_OpFnOper(Script *sc, Expr *expr, OpFnOper *op);
static int exe_ListOper(Script *sc, Expr *expr, ListOper *op);
static int exe_GroupOper(Script *sc, Expr *expr, GroupOper *op);
static int exe_FieldOper(Script *sc, Expr *expr, FieldOper *op);
static int exe_CloneOper(Script *sc, Expr *expr, CloneOper *op);

/*.......................................................................
 * Parse an single argument expression.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  target    TypeSpec *  The target argument type.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int parse_argument(Script *sc, TypeSpec *target, InputStream *stream, Expr *e)
{
/*
 * Find the start of the expression.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
/*
 * Lists aren't a datatype in their own right, so we need special
 * code here to parse them. Delegate parsing atom expressions with
 * the method functions of the associated datatype.
 */
  return target->is_list ?
    parse_list_expr(sc, target->dt, stream, e) :
      target->dt->parse_fn(sc, target->dt, stream, e);
}

/*.......................................................................
 * Parse an operand of a given datatype from an input stream. This
 * is intended for use by the parse_fn() methods of datatypes.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype.
 *  is_list        int    True if a list of 'dt' is expected.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int parse_operand(Script *sc, DataType *dt, int is_list,
		  InputStream *stream, Expr *e)
{
/*
 * Locate the start of the operand.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
/*
 * Determine what form of operand to parse.
 */
  switch(stream->nextc) {
/*
 * A parenthesized sub-expression.
 */
  case '(':
/*
 * Skip the open parenthesis and parse the sub-expression.
 */
    if(input_skip_white(stream, 1, 1))
      return 1;
    if(is_list ?
       parse_list_expr(sc, dt, stream, e) :
       dt->parse_fn(sc, dt, stream, e))
      return 1;
/*
 * Check for and skip the close parenthesis.
 */
    if(input_skip_white(stream, 0, 0) ||
       stream->nextc != ')' ||
       input_skip_space(stream, 0, 1)) {
      input_error(stream, 1, "Unclosed parenthesis.\n");
      return 1;
    };
    break;
/*
 * A reference to a variable, or a function call.
 */
  case '$':
    if(parse_dollar_expr(sc, dt, is_list, stream, e) == NULL)
      return 1;
    break;
/*
 * Attempt to parse a datatype constant.
 */
  default:
    return is_list ?
      parse_list_const(sc, dt, stream, e) :
      dt->const_fn(sc, dt, stream, e);
    break;
  };
  return 0;
}

/*.......................................................................
 * Parse a variable or function call after encountering a $ scoperator.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype (or NULL if any will do).
 *  is_list        int    True if a list of 'dt' is expected. This is
 *                        ignored if 'dt' is NULL.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return    TypeSpec *  The type of the expression. This will be
 *                        consistent with dt and is_list unless dt==NULL,
 *                        or an error occurs. Otherwise, if dt==NULL
 *                        it is up to the caller to check that the
 *                        returned type matches its needs.
 */
TypeSpec *parse_dollar_expr(Script *sc, DataType *dt, int is_list,
			    InputStream *stream, Expr *e)
{
  Symbol *symbol;  /* A script symbol */
/*
 * Skip the $ and locate the start of the variable or function name.
 */
  if(input_skip_white(stream, 1, 1))
    return NULL;
/*
 * Read and attempt to identify the following symbol.
 */
  if(input_keyword(stream, 0, 0)) {
    input_error(stream, 1, "An unexpected symbol follows the $ scoperator.\n");
    return NULL;
  };
  symbol = find_ScriptSymbol(sc, stream, stream->work);
  if(!symbol)
    return NULL;
/*
 * Find out what type of symbol we have found.
 */
  switch(symbol->code) {
/*
 * If the symbol is a variable and has the correct type, push it onto
 * the instruction stack.
 */
  case SYM_VARIABLE:
/*
 * Arrange for the referenced variable to be loaded onto the stack.
 */
    {
      Variable *var = (Variable* )symbol->data;
      if(!add_LoadOper(sc, e, var))
	return NULL;
/*
 * Check for group-field references and verify that the resulting
 * variable has the correct type.
 */
      return parse_variable_ref(sc, dt, is_list, var->type, stream, e);
    };
    break;
/*
 * Evaluate a function?
 */
  case SYM_FUNCTION:
    return parse_function_call(sc, dt, is_list, (Function *)symbol->data,
			       stream, e);
    break;
  case SYM_DATATYPE:
    return parse_cast_expr(sc, dt, is_list, (DataType *)symbol->data,
			   stream, e);
    break;
  default:
    input_error(stream, 1, "An unknown symbol (%s) follows the $ scoperator.\n",
		symbol->name);
    return NULL;
    break;
  };
}

/*.......................................................................
 * Parse a variable expression. This includes following group fields
 * where necessary. It is assumed that the caller has just parsed
 * an expression that will evaluate to a variable of declaration 'type'.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype (NULL if unimportant).
 *  is_list        int    True if a list of 'dt' is expected (ignored
 *                        if dt==NULL).
 *  type      TypeSpec *  The type of the variable being referenced.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return    TypeSpec *  The type of the expression. This will be
 *                        consistent with dt and is_list unless dt==NULL,
 *                        or an error occurs. Otherwise, if dt==NULL
 *                        it is up to the caller to check that the
 *                        returned type matches its needs.
 */
static TypeSpec *parse_variable_ref(Script *sc, DataType *dt, int is_list,
			      TypeSpec *type, InputStream *stream, Expr *e)
{
/*
 * Find the next token.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
/*
 * If the variable is a group and is followed by the field scoperator,
 * parse the fields.
 */
  while(stream->nextc == '.' && type->dt->dataclass==DT_GROUP && !type->is_list) {
/*
 * Get the list of group field declarations.
 */
    GroupType *gt = (GroupType* )type->dt->context;
    ListNode *node = gt->fields->head;
    int member = 0;
/*
 * Skip the field scoperator.
 */
    if(input_skip_white(stream, 1, 1))
      return NULL;
/*
 * Read the next field name.
 */
    if(input_keyword(stream, 0, 0)) {
      input_error(stream, 1, "An invalid field name follows a . scoperator.\n");
      return NULL;
    };
/*
 * Locate the group field that matches the field-name.
 */
    for( ; node; node=node->next,member++) {
      TypeSpec *t = (TypeSpec* )node->data;
      if(strcmp(t->name, stream->work) == 0) {
	type = t;
	break;
      };
    };
/*
 * Field not found?
 */
    if(!node) {
      input_error(stream, 1,
		  "The group datatype '%s' doesn't have a field named '%s'\n",
		  type->dt->name, stream->work);
      return NULL;
    };
/*
 * Add a field-reference scoperator to the instruction list.
 */
    if(!add_FieldOper(sc, e, member))
      return NULL;
/*
 * Prepare to check for another field reference.
 */
    if(input_skip_space(stream, 1, 0))
      return NULL;
  };
/*
 * Now verify that the declaration of the final field matches the
 * required declaration.
 */
  if(dt) {
    if((type->dt->id != DT_WILDCARD && type->dt != dt) || 
       !type->is_list != !is_list || 
       (type->dt->id != DT_UNK && type->dt->id != dt->id)) {

      COUT(dt->id << " " << type->dt->id);
      input_error(stream, 1, "The entered $ expression doesn't yield a value of the required datatype.\n");
      return NULL;
    };
  };
  return type;
}

/*.......................................................................
 * Parse a function invokation expression. It is assumed that the caller
 * has just read $function_name from the input stream and that 'func'
 * contains the function corresponding to function_name.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype (NULL if unimportant).
 *  is_list        int    True if a list of 'dt' is expected (ignored
 *                        if dt==NULL).
 *  func      Function *  The function to be invoked.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return    TypeSpec *  The type of the expression. This will be
 *                        consistent with dt and is_list unless dt==NULL,
 *                        or an error occurs. Otherwise, if dt==NULL
 *                        it is up to the caller to check that the
 *                        returned type matches its needs.
 */
static TypeSpec *parse_function_call(Script *sc, DataType *dt, int is_list,
			       Function *func, InputStream *stream, Expr *e)
{
  ListNode *arg;       /* A node in the list of function arguments */
  Variable *data;      /* The loop-specific state variable of the function */
/*
 * If the function is a loop-state function, get its loop-state data
 * container associated with the innermost enclosing loop.
 */
  if(func->is_builtin && func->body.builtin.loop_fn) {
    data = get_loop_data(sc, stream, func);
    if(!data)
      return NULL;
  } else {
    data = NULL;
  };
/*
 * Find the next non-space character that follows the function name.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
/*
 * Has the argument list been omitted?
 */
  if(stream->nextc != '(') {
/*
 * The argument list and its enclosing parentheses can be omitted if
 * the function doesn't require any mandatory arguments.
 */
    if(func->args->head != func->opt) {
      input_error(stream, 1, "Missing '(' after function name.\n");
      return NULL;
    };
/*
 * Arrange to set all of the optional arguments to NULL.
 */
    for(arg=func->opt; arg; arg=arg->next) {
      TypeSpec *type = ((Variable *)arg->data)->type;
      DataType *dt = type->dt;
      if(add_LoadOper(sc, e, type->is_list ? dt->null_list : dt->null_atom))
	return NULL;
    };
/*
 * Parse the parenthesis-enclosed argument list of the function.
 */
  } else {
/*
 * Move over the open parenthesis to locate the first argument.
 */
    if(read_InputStream(stream, 1))
      return NULL;
/*
 * Parse the arguments.
 */
    if(parse_procedure_arguments(sc, func->return_type->name, 1,
				 func->args, NULL, func->opt, stream, e))
      return NULL;
  };
/*
 * Arrange for the function to be evaluated.
 */
  if(!add_FuncOper(sc, e, func, data))
    return NULL;
/*
 * Check for field references and the verify that the resulting type
 * matches the target type.
 */
  return parse_variable_ref(sc, dt, is_list, func->return_type, stream, e);
}

/*.......................................................................
 * Parse the argument expressions of a function or command.
 *
 * Input:
 *  sc           Script *  The parent script.
 *  name           char *  The name of the procedure (for error reporting).
 *  is_func         int    True if the procedure is function. False if it
 *                         is a command.
 *  args           List *  The list of argument variables.
 *  mods   ModifierList *  The list of command modifiers, or NULL if the
 *                         procedure doesn't have any modifiers.
 *  opts       ListNode *  If 'args' contains a trailing list of optional
 *                         arguments, then 'opts' be the list-node in 'args'
 *                         that contains the first optional argument.
 *                         Otherwise it must be NULL.
 *  stream  InputStream *  The stream to read the argument expressions from.
 * Input/Output:
 *  e              Expr *  The expression to append the argument evaluation
 *                         instructions to.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int parse_procedure_arguments(Script *sc, char *name, int is_func, List *args,
			      ModifierList *mods, ListNode *opts,
			      InputStream *stream, Expr *e)
{
  ListNode *arg;       /* A node in the list of procedure arguments */
  ListNode *mod;       /* A node in the list of modifiers */
  int iarg;            /* The number of the argument being parsed */
  int nopt;            /* The potential number of optional arguments */
  int narg=0;          /* The number of arguments seen so far */
/*
 * Name the type of procedure for use in error reports.
 */
  char *type = is_func ? (char* )"function" : (char* )"command";
/*
 * The following macro returns true when a terminator character of
 * the current procedure type is seen.
 */
#define IS_TERMINATOR(c) (is_func ? (c) == ')' : ((c) == '\n' || (c) == EOF))
/*
 * Mark all modifier arguments as NULL.
 */
  for(arg=args->head; arg; arg=arg->next) {
    Variable *var = (Variable* )arg->data;
    if(var->flags & VAR_IS_MODIFIER)
      var->flags |= VAR_IS_NUL;
    else
      break;
  };
/*
 * Parse modifier arguments.
 */
  while(stream->nextc == '/') {
    Modifier *modifier; /* The specified modifier */
    Variable *var;      /* The variable associated with the modifier */
/*
 * Does this command have any modifiers?
 */
    if(!mods) {
      input_error(stream, 1, "Unknown modifier to the '%s' %s.\n", name, type);
      return 1;
    };
/*
 * Skip the '/' and read the modifier name.
 */
    if(read_InputStream(stream, 0) ||
       input_keyword(stream, 0, 1)) {
      input_error(stream, 1,
		  "Missing modifier name after / in the call to %s.\n", name);
      return 1;
    };
/*
 * Compare the keyword against the declared modifiers of the command.
 */
    modifier = NULL;
    for(mod=mods->head; mod && !modifier; mod=mod->next) {
      Modifier *m = (Modifier* )mod->data;
      if(strcmp(m->name, stream->work) == 0)
	modifier = m;
    };
/*
 * Is the modifier unknown?
 */
    if(!modifier) {
      input_error(stream, 1, "Unknown modifier /%s of the '%s' %s.\n",
		  stream->work, name, type);
      return 1;
    };
/*
 * Get the procedure argument that records the modifier.
 */
    var = modifier->arg;
/*
 * Is it a boolean modifier?
 */
    if(var->type->dt == sc->builtin.boolvar_dt) {
      if(add_SetBoolOper(sc, e, var, 1)==NULL)
	return 1;
/*
 * No, so it must be a string modifier.
 */
    } else {
/*
 * Complain if the user has already specified a value for this modifier.
 */
      if(!(var->flags & VAR_IS_NUL)) {
	input_error(stream, 1,
		    "The '%s' and '%s' modifiers of %s can't be mixed.\n",
		    stream->work, STRING_VARIABLE(var)->string, name);
	return 1;
      };
/*
 * Arrange for the modifier name to be assigned to the modifier argument.
 */
      if(add_SetStringOper(sc, e, var, modifier->name)==NULL)
	return 1;
/*
 * Temporarily record the string value in the argument variable so that
 * we can use it above for reporting modifier clashes.
 */
      STRING_VARIABLE(var)->string = modifier->name;
    };
/*
 * Mark the option as having a value.
 */
    var->flags &= ~VAR_IS_NUL;
  };
/*
 * For each modifier modifier that hasn't been set, arrange for
 * it to be given its default value. In the case of string-valued
 * modifier arguments this is the first modifier name registered
 * to that argument.
 */
  if(mods) {
    for(mod=mods->head; mod; mod=mod->next) {
      Modifier *m = (Modifier* )mod->data;
      Variable *var = m->arg;
      if(var->flags & VAR_IS_NUL) {
	if(var->type->dt == sc->builtin.boolvar_dt) {
	  if(add_SetBoolOper(sc, e, var, 0)==NULL)
	    return 1;
	} else {
	  if(add_SetStringOper(sc, e, var, m->name)==NULL)
	    return 1;
	};
/*
 * Mark the option as now having a value.
 */
	var->flags &= ~VAR_IS_NUL;
      };
    };
  };
/*
 * Find the start of the first normal argument.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * Parse each of the mandatory arguments.
 */
  for(iarg=1; arg != opts; arg=arg->next, iarg++) {
    Variable *var = (Variable* )arg->data;
/*
 * Parse the argument expression and arrange for its value to be assigned
 * to the dummy procedure argument variable.
 */
    if(parse_argument(sc, var->type, stream, e) ||
       input_skip_space(stream, 1, 0) ||
       add_StoreOper(sc, e, var)==NULL)
      return 1;
/*
 * Keep a tally of the number of arguments parsed so far.
 */
    narg++;
/*
 * Skip the inter-argument comma unless we have reached the end
 * of the mandatory arguments.
 */
    if(arg->next != opts) {
      if(stream->nextc != ',') {
	if(IS_TERMINATOR(stream->nextc)) {
	  input_error(stream, 1,
		      "Too few arguments have been presented to the '%s' %s.\n",
		      name, type);
	} else {
	  input_error(stream, 1, "Argument %d of the '%s' %s is garbled.\n",
		      iarg, name, type);
	};
	return 1;
      };
      if(input_skip_white(stream, 1, 1))
	return 1;
    };
  };
/*
 * Parse any optional arguments.
 */
  if(opts) {
/*
 * Mark the optional arguments as not having values, so that subsequently
 * we will be able to tell which have been given values.
 */
    for(arg=opts,nopt=0; arg; arg=arg->next, nopt++) {
      Variable *var = (Variable* )arg->data;
      var->flags |= VAR_IS_NUL;
    };
/*
 * Parse optional arguments separated by commas.
 */
    while(narg ? stream->nextc==',' : !IS_TERMINATOR(stream->nextc)) {
      Variable *var;     /* The optional argument being processed */
/*
 * Skip interargument commas.
 */
      if(narg && input_skip_white(stream, 1, 1))
	return 1;
/*
 * Read the name of the option.
 */
      if(input_keyword(stream, 0, 0)) {
	input_error(stream, 1,
	      "Missing argument name for name=value optional argument.\n");
	return 1;
      };
/*
 * Search for the named optional argument.
 */
      for(arg=opts; arg; arg=arg->next) {
	if(strcmp(stream->work, ((Variable *)arg->data)->type->name) == 0)
	  break;
      };
/*
 * Not found?
 */
      if(!arg) {
	input_error(stream, 1, "Unknown named optional argument: %s\n",
		    stream->work);
	return 1;
      };
/*
 * Get the corresponding dummy-argument variable into which the value
 * will be copied.
 */
      var = (Variable* )arg->data;
/*
 * Skip the following '=' scoperator.
 */
      if(stream->nextc != '=') {
	input_error(stream, 1, "No value given for optional argument: %s\n",
		    stream->work);
	return 1;
      };
      if(input_skip_white(stream, 1, 1))
	return 1;
/*
 * Has the specified argument already been given a value?
 */
      if(~var->flags & VAR_IS_NUL) {
	input_error(stream, 1,
		    "Redundant assignment of optional argument: %s\n",
		    stream->work);
	return 1;
      };
/* 
 * Parse the trailing expression according to the datatype of the specified
 * argument and arrange for its value to be stored in the dummy argument
 * variable of the procedure.
 */
      if(parse_argument(sc, ((Variable *) arg->data)->type, stream, e) ||
	 input_skip_space(stream, 1, 0) ||
	 add_StoreOper(sc, e, var)==NULL)
	return 1;
/*
 * Mark the argument as having been given a value.
 */
      var->flags &= ~VAR_IS_NUL;
/*
 * Keep a tally of the number of arguments parsed so far.
 */
      narg++;
    };
/*
 * For each optional argument that hasn't been assigned, arrange for
 * it to be marked as null.
 */
    for(arg=opts; arg; arg=arg->next) {
      Variable *var = (Variable* )arg->data;
      if(var->flags & VAR_IS_NUL)
	add_UnsetOper(sc, e, var);
    };
  };
/*
 * Check for the terminator character.
 */
  if(!IS_TERMINATOR(stream->nextc)) {
    if(stream->nextc == ',') {
      input_error(stream, 1,
		  "Too many arguments have been presented to the '%s' %s.\n",
		  name, type);
    } else {
      input_error(stream, 1, "The last argument of the '%s' %s is garbled.\n",
		  name, type);
    };
    return 1;
  };
/*
 * Skip the argument list terminator if need be.
 */
  if(is_func && input_skip_space(stream, 1, 1))
    return 1;
  return 0;
#undef IS_TERMINATOR
}

/*.......................................................................
 * Parse a cast expression. It is assumed that the caller
 * has just read $datatype_name from the input stream and that 'cast_dt'
 * contains the datatype corresponding to datatype_name.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype (NULL if unimportant).
 *  is_list        int    True if a list of 'dt' is expected (ignored
 *                        if dt==NULL).
 *  cast      DataType *  The cast datatype.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return    TypeSpec *  The type of the expression. This will be
 *                        consistent with dt and is_list unless dt==NULL,
 *                        or an error occurs. Otherwise, if dt==NULL
 *                        it is up to the caller to check that the
 *                        returned type matches its needs.
 */
static TypeSpec *parse_cast_expr(Script *sc, DataType *dt, int is_list,
				 DataType *cast, InputStream *stream, Expr *e)
{
/*
 * The following expression must be enclosed in parenthesis.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc != '(') {
    input_error(stream, 1, "Missing '(' after datatype name '%s'.\n",
		cast->name);
    return NULL;
  };
  if(input_skip_white(stream, 1, 1))
    return NULL;
/*
 * Parse an expression of the specified type.
 */
  if(parse_argument(sc, cast->atom_reg, stream, e))
    return NULL;
/*
 * Check for and skip the terminating parenthesis.
 */
  if(input_skip_white(stream, 1, 0))
    return NULL;
  if(stream->nextc != ')') {
    input_error(stream, 1, "Unclosed parenthesis in %s() expression.\n",
		cast->name);
    return NULL;
  };
  if(input_skip_space(stream, 1, 1))
    return NULL;
/*
 * Check for field references and the verify that the resulting type
 * matches the target type.
 */
  return parse_variable_ref(sc, dt, is_list, cast->atom_reg, stream, e);
}

/*.......................................................................
 * Parse a list expression. This is the equivalent of a datatype
 * DT_PARSE() method function.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int parse_list_expr(Script *sc, DataType *dt, InputStream *stream,
			   Expr *e)
{
  return parse_operand(sc, dt, 1, stream, e);
}

/*.......................................................................
 * Parse a list-constant expression. This is the equivalent of a
 * datatype DT_CONST() method funcion.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  dt        DataType *  The target datatype.
 *  stream InputStream *  The stream to parse from.
 * Input/Output:
 *  e             Expr *  The expression output.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int parse_list_const(Script *sc, DataType *dt, InputStream *stream,
			    Expr *e)
{
  int nmember = 0;   /* The number of list elements */
/*
 * List constants are delimited by braces.
 */
  if(stream->nextc != '{') {
    input_error(stream, 1, "Missing 'listof %s' value.\n", dt->name);
    return 1;
  };
/*
 * Parse the members of the list.
 */
  do {
/*
 * Keep a record of the current end of the list of expression instructions.
 */
    ListNode *old_end = get_ExprEnd(e);
/*
 * Skip the '{' before the first argument and the comma preceding
 * subsequent arguments.
 */
    if(input_skip_white(stream, 1, 1))
      return 1;
/*
 * Parse the next list member.
 */
    if(dt->parse_fn(sc, dt, stream, e))
      return 1;
    nmember++;
/*
 * If the parsed member is an alias of a user variable, arrange for it
 * to be replaced with a copy. If this weren't done and the user
 * subsequently changed the aliased variable, the list member would
 * also change.
 */
    if(remove_alias(sc, e, old_end))
      return 1;
/*
 * If there are any further arguments then the next token should
 * be a comma.
 */
    if(input_skip_white(stream, 1, 0))
      return 1;
  } while(stream->nextc == ',');
/*
 * Make sure that the list is correctly terminated.
 */
  if(stream->nextc != '}') {
    input_error(stream, 1, "Missing , or } after a 'listof %s' member.\n",
		dt->name);
    return 1;
  };
/*
 * Skip the terminator.
 */
  if(input_skip_space(stream, 1, 1))
    return 1;
/*
 * Add a list-construction scoperator to the instruction list.
 */
  if(!add_ListOper(sc, e, dt, nmember))
    return 1;
  return 0;
}

/*.......................................................................
 * Create an expression object.
 *
 * Input:
 *  sc      Script *  The host script environment.
 * Output:
 *  expr      Expr *  The new object, or NULL on error.
 */
Expr *new_Expr(Script *sc)
{
  Expr *e;   /* The object to be returned */
/*
 * Allocate the container.
 */
  e = (Expr* )new_ScriptObject(sc, sc->memory.expr, 0);
  if(!e)
    return NULL;
/*
 * Clear the container.
 */
  e->stack = NULL;
  e->args = NULL;
  e->ops = NULL;
/*
 * Allocate the list that will be used as the execution-time stack
 */
  e->stack = new_VariableList(sc);
  if(!e->stack)
    return NULL;
/*
 * Allocate a list for assembling argument-lists from stack variables.
 */
  e->args = new_VariableList(sc);
  if(!e->args)
    return NULL;
/*
 * Allocate the list that will contain the list of instructions needed
 * to evaluate the expression.
 */
  e->ops = new_ScriptList(sc);
  if(!e->ops)
    return NULL;
  return e;
}

/*.......................................................................
 * Create and add an empty expression operation to a given expression.
 * The caller is responsible for filling in the instruction-specific
 * fields of the returned object.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  type   OperType     The type of instruction to be appended.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
static ExprOper *append_ExprOper(Script *sc, Expr *e, OperType type)
{
/*
 * Allocate the instruction object.
 */
  ExprOper *op = (ExprOper* )new_ScriptObject(sc, sc->memory.exproper, 0);
  if(!op)
    return NULL;
/*
 * Initialize the instruction according to its type.
 */
  op->type = type;
  switch(type) {
  case OPER_LOAD:
    op->context.load.var = NULL;
    break;
  case OPER_FUNC:
    op->context.func.func = NULL;
    op->context.func.result = NULL;
    op->context.func.data = NULL;
    break;
  case OPER_LIST:
    op->context.list.var = NULL;
    break;
  case OPER_GROUP:
    op->context.group.var = NULL;
    break;
  case OPER_FIELD:
    op->context.field.field = 0;
    break;
  case OPER_CLONE:
    op->context.clone.var = NULL;
    break;
  case OPER_OPER:
    op->context.oper.oper = NULL;
    op->context.oper.result = NULL;
    break;
  case OPER_SKIP:
    break;
  case OPER_STORE:
    op->context.store.var = NULL;
    break;
  case OPER_UNSET:
    op->context.unset.var = NULL;
    break;
  case OPER_SET_BOOL:
    op->context.set_boolvar.var = NULL;
    op->context.set_boolvar.state = 0;
    break;
  case OPER_SET_STRING:
    op->context.set_string.var = NULL;
    op->context.set_string.string = "";
    break;
  default:
    lprintf(stderr, "append_ExprOper: Unknown instruction type.\n");
    return NULL;
    break;
  };
/*
 * Append the object to the instruction list.
 */
  if(append_ListNode(e->ops, op) == NULL)
    return NULL;
  return op;
}

/*.......................................................................
 * Append a load-variable instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  var    Variable *   The variable to be loaded.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_LoadOper(Script *sc, Expr *e, Variable *var)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_LOAD);
  if(!op)
    return NULL;
  op->context.load.var = var;
  return op;
}

/*.......................................................................
 * Append an evaluate-function instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  func   Function *   The function to be called.
 *  data   Variable *   If func is a loop-state object, then this
 *                      should be the associated loop-state value
 *                      container. Otherwise it should be NULL.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_FuncOper(Script *sc, Expr *e, Function *func, Variable *data)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_FUNC);
  if(!op)
    return NULL;
  op->context.func.func = func;
  op->context.func.data = data;
/*
 * Allocate the variable that will contain the return value of the
 * function.
 */
  op->context.func.result = new_Variable(sc, func->return_type);
  if(!op->context.func.result)
    return NULL;
  return op;
}

/*.......................................................................
 * Append an assemble-list instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  dt     DataType *   The datatype of the list elements.
 *  n      unsigned     The number of stack elements to add to the list.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_ListOper(Script *sc, Expr *e, DataType *dt, unsigned n)
{
  ExprOper *op;     /* The instruction object to be returned */
  VariableList *vl; /* The list container of the target list variable */
/*
 * Allocate and append the instruction object.
 */
  op = append_ExprOper(sc, e, OPER_LIST);
  if(!op)
    return NULL;
/*
 * Create the variable that will contain the assembled list.
 */
  op->context.list.n = n;
  op->context.list.var = new_Variable(sc, dt->list_reg);
/*
 * Create the list container of the variable.
 */
  vl = LIST_VARIABLE(op->context.list.var)->list = new_VariableList(sc);
  if(!vl)
    return NULL;
  return op;
}

/*.......................................................................
 * Append an assemble-group instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  dt     DataType *   The datatype of the group.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_GroupOper(Script *sc, Expr *e, DataType *dt)
{
  ExprOper *op;     /* The instruction object to be returned */
  VariableList *vl; /* The list container of the target group variable */
  GroupType *gt;    /* The distinguishing attributes of the group datatype */
/*
 * Get the group attributes.
 */
  gt = (GroupType* )dt->context;
/*
 * Allocate and append the instruction object.
 */
  op = append_ExprOper(sc, e, OPER_GROUP);
  if(!op)
    return NULL;
/*
 * Record the number of group fields.
 */
  op->context.group.n = gt->nfield;
/*
 * Create the variable that will contain the assembled group.
 */
  op->context.group.var = new_Variable(sc, dt->atom_reg);
/*
 * Create the list container of the variable.
 */
  vl = LIST_VARIABLE(op->context.group.var)->list = new_VariableList(sc);
  if(!vl)
    return NULL;
  return op;
}

/*.......................................................................
 * Append an access group-field instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  field  unsigned     The sequential index of the group field.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_FieldOper(Script *sc, Expr *e, unsigned field)
{
  ExprOper *op;     /* The instruction object to be returned */
/*
 * Allocate and append the instruction object.
 */
  op = append_ExprOper(sc, e, OPER_FIELD);
  if(!op)
    return NULL;
  op->context.field.field = field;
  return op;
}

/*.......................................................................
 * Append a "make unnamed copy of variable" instruction to the instruction
 * list of a given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  type   TypeSpec *   The declaration of the variable to copy.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_CloneOper(Script *sc, Expr *e, TypeSpec *type)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_CLONE);
  if(!op)
    return NULL;
/*
 * Create the output variable of the copy operation. Note that
 * we don't use *type directly, because it contains the name of the
 * original variable. We want to create an unnamed variable.
 */
  op->context.clone.var = new_Variable(sc,
               type->is_list ? type->dt->list_reg : type->dt->atom_reg);
  if(!op->context.clone.var)
    return NULL;
  return op;
}

/*.......................................................................
 * Append an evaluate-scoperator instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  oper   ScOperator *   The scoperator to invoke.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_OpFnOper(Script *sc, Expr *e, ScOperator *oper)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_OPER);
  if(!op)
    return NULL;
  op->context.oper.oper = oper;
/*
 * Allocate the variable that will contain the return value of the
 * scoperator.
 */
  op->context.oper.result = new_Variable(sc, oper->return_type);
  if(!op->context.oper.result)
    return NULL;
  return op;
}

/*.......................................................................
 * Append a "skip if true" instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_SkipOper(Script *sc, Expr *e)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_SKIP);
  if(!op)
    return NULL;
  return op;
}

/*.......................................................................
 * Append a store instruction to the instruction list of a given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  var    Variable *   The variable into which to store the value at the
 *                      top of the stack.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_StoreOper(Script *sc, Expr *e, Variable *var)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_STORE);
  if(!op)
    return NULL;
  op->context.store.var = var;
  return op;
}

/*.......................................................................
 * Append an unset-variable instruction to the instruction list of a
 * given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  var    Variable *   The variable that is to be made null.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_UnsetOper(Script *sc, Expr *e, Variable *var)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_UNSET);
  if(!op)
    return NULL;
  op->context.unset.var = var;
  return op;
}

/*.......................................................................
 * Append a set-boolean-modifier variable instruction to the instruction
 * list of a given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  var    Variable *   The variable that is to be made null.
 *  state       int     The target value of the boolean variable.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_SetBoolOper(Script *sc, Expr *e, Variable *var, int state)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_SET_BOOL);
  if(!op)
    return NULL;
  op->context.set_boolvar.var = var;
  op->context.set_boolvar.state = state;
  return op;
}

/*.......................................................................
 * Append a set-string-modifier variable instruction to the instruction
 * list of a given expression.
 *
 * Input:
 *  sc       Script *   The host script environment.
 *  e          Expr *   The expression to add the instruction to.
 *  var    Variable *   The variable that is to be made null.
 *  string     char *   The target value of the variable. This string
 *                      must have a storage duration that lasts at least
 *                      as long as the current script.
 * Output:
 *  return ExprOper *   The new instruction node, or NULL on error.
 */
ExprOper *add_SetStringOper(Script *sc, Expr *e, Variable *var, char *string)
{
  ExprOper *op = append_ExprOper(sc, e, OPER_SET_STRING);
  if(!op)
    return NULL;
  op->context.set_string.var = var;
  op->context.set_string.string = string;
  return op;
}

/*.......................................................................
 * Create a free-list from which to allocate Expr objects.
 */
ExprMem *new_ExprMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(Expr));
}

/*.......................................................................
 * Create a free-list from which to allocate ExprOper objects.
 */
ExprOperMem *new_ExprOperMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(ExprOper));
}

/*.......................................................................
 * Pop the top 'narg' arguments from an expression stack and reassemble
 * them into an argument list in expr->args. The caller should apply
 * clr_List() to the returned list when it is no longer required.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  expr            Expr *  The expression that contains the stack.
 *  narg             int    The number of arguments to assemble, or -1
 *                          to pop all the remaining arguments.
 * Output:
 *  return  VariableList *  The assembled list of arguments, or NULL
 *                          on error.
 */
VariableList *pop_ExprStackArgs(Script *sc, Expr *expr, int narg)
{
  int i;
/*
 * Discard legacy arguments.
 */
  clr_List(expr->args);
/*
 * Pop all stacked values?
 */
  if(narg < 0)
    narg = expr->stack->nnode;
/*
 * Assemble the new argument list from the top narg variables off
 * the stack.
 */
  for(i=0; i<narg; i++) {
/*
 * Get the next argument from the stack.
 */
    Variable *arg = pop_ExprStack(sc, expr);
    if(!arg) {
      clr_List(expr->args);
      return NULL;
    };
/*
 * Push the variable onto the list of function arguments.
 */
    if(prepend_ListNode(expr->args, arg)==NULL) {
      clr_List(expr->args);
      return NULL;
    };
  };
  return expr->args;
}

/*.......................................................................
 * Pop the top variable from the expression stack.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  expr        Expr *  The expression to take the values from.
 * Output:
 *  return  Variable *  The variable that was removed from the top of
 *                      the stack, or NULL on error.
 */
Variable *pop_ExprStack(Script *sc, Expr *expr)
{
  if(!expr->stack->head) {
    lprintf(stderr, "pop_ExprStack: Stack empty!\n");
    return NULL;
  };
  return (Variable* )del_ListNode(expr->stack, expr->stack->head, NULL);
}

/*.......................................................................
 * Return an alias of the variable at the top of the expression stack.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  expr        Expr *  The expression to take the values from.
 * Output:
 *  return  Variable *  The variable at the top of the stack, or NULL on
 *                      error.
 */
static Variable *top_ExprStack(Script *sc, Expr *expr)
{
  if(!expr->stack->head) {
    lprintf(stderr, "top_ExprStack: Stack empty!\n");
    return NULL;
  };
  return (Variable* )expr->stack->head->data;
}

/*.......................................................................
 * Push a variable onto the expression-evaluation stack.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  expr        Expr *  The host expression.
 *  var     Variable *  The variable to push onto the stack.
 * Output:
 *  return  ListNode *  The list node that contains the specified variable,
 *                      or NULL on error.
 */
static ListNode *push_ExprStack(Script *sc, Expr *expr, Variable *var)
{
  return prepend_ListNode(expr->stack, var);
}

/*.......................................................................
 * Evaluate an expression. The result(s) of the expression will be left in
 * the expression stack.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression to be evaluated.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int exe_Expr(Script *sc, Expr *expr)
{
  ListNode *node;  /* The list container of the operation being executed */
/*
 * Make sure that the expression stack doesn't contain any legacy values.
 */
  if(expr->stack->head) {
    lprintf(stderr, "exe_Expr: Stale values found on stack.\n");
    return 1;
  };
/*
 * Evaluate each of the instructions of the operations list.
 */
  for(node=expr->ops->head; node; node=node->next) {
    ExprOper *eop = (ExprOper* )node->data;
/*
 * Execute the latest instruction according to its type.
 */
    switch(eop->type) {
    case OPER_LOAD:
      if(!push_ExprStack(sc, expr, eop->context.load.var))
	return 1;
      break;
    case OPER_FUNC:
      if(exe_FuncOper(sc, expr, &eop->context.func))
	return 1;
      break;
    case OPER_LIST:
      if(exe_ListOper(sc, expr, &eop->context.list))
	return 1;
      break;
    case OPER_GROUP:
      if(exe_GroupOper(sc, expr, &eop->context.group))
	return 1;
      break;
    case OPER_FIELD:
      if(exe_FieldOper(sc, expr, &eop->context.field))
	return 1;
      break;
    case OPER_CLONE:
      if(exe_CloneOper(sc, expr, &eop->context.clone))
	return 1;
      break;
    case OPER_OPER:
      if(exe_OpFnOper(sc, expr, &eop->context.oper))
	return 1;
      break;
    case OPER_SKIP:
      {
	Variable *var = top_ExprStack(sc, expr);
	if(!var)
	  return 1;
	if(BOOL_VARIABLE(var)->boolvar)
	  return 0;
      };
      break;
    case OPER_STORE:
      if(copy_Variable(eop->context.store.var, pop_ExprStack(sc, expr))==NULL)
	return 1;
      break;
    case OPER_UNSET:
      if(copy_Variable(eop->context.unset.var, NULL)==NULL)
	return 1;
      break;
    case OPER_SET_BOOL:
      BOOL_VARIABLE(eop->context.set_boolvar.var)->boolvar =
	eop->context.set_boolvar.state;
      break;
    case OPER_SET_STRING:
      STRING_VARIABLE(eop->context.set_string.var)->string =
	eop->context.set_string.string;
      break;
    default:
      lprintf(stderr, "exe_Expr: Unknown instruction.\n");
      return 1;
    };
  };
  return 0;
}

/*.......................................................................
 * Evaluate a function-call scoperator.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op     FuncOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_FuncOper(Script *sc, Expr *expr, FuncOper *op)
{
  Function *func = op->func;     /* The function to be evaluated */
/*
 * Evaluate the function.
 */
  if(func->is_builtin) {
    if(func->body.builtin.func_fn(sc, func->args, op->result, op->data))
      return 1;
  } else {
    Variable *result;  /* The return value of the function */
/*
 * Evaluate the expression that constitutes the body of the function.
 */
    if(exe_Expr(sc, func->body.user.expr))
      return 1;
/*
 * Get the result of the function expression.
 */
    result = pop_ExprStack(sc, func->body.user.expr);
    if(!result)
      return 1;
/*
 * Copy the result value into the pre-allocated function return
 * variable.
 */
    copy_Variable(op->result, result);
  };
/*
 * Push the result onto the stack.
 */
  if(!push_ExprStack(sc, expr, op->result))
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate an scoperator instruction.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op     OpFnOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_OpFnOper(Script *sc, Expr *expr, OpFnOper *op)
{
  ScOperator *oper;     /* The scoperator to be evaluated */
  VariableList *args; /* The argument list of the scoperator */
/*
 * Assemble the arguments of the scoperator.
 */
  oper = op->oper;
  args = pop_ExprStackArgs(sc, expr, oper->narg);
  if(!args)
    return 1;
/*
 * Evaluate the scoperator.
 */
  if(oper->oper_fn(sc, args, op->result)) {
    clr_List(args);
    return 1;
  };
/*
 * Discard the argument list.
 */
  clr_List(args);
/*
 * Push the result onto the stack.
 */
  if(!push_ExprStack(sc, expr, op->result))
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate a list-assembly instruction.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op     ListOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_ListOper(Script *sc, Expr *expr, ListOper *op)
{
  int i;
/*
 * Discard any previous list contents.
 */
  clr_List(LIST_VARIABLE(op->var)->list);
/*
 * Copy the top list->n stack variables into the output list
 * variable.
 */
  for(i=0; i < (int)op->n; i++) {
    Variable *var = (Variable* )pop_ExprStack(sc, expr);
    if(!var || prepend_ListNode(LIST_VARIABLE(op->var)->list, var) == NULL)
      return 1;
  };
/*
 * Push the resulting list variable onto the stack.
 */
  if(!push_ExprStack(sc, expr, op->var))
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate a group-assembly instruction.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op     GroupOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_GroupOper(Script *sc, Expr *expr, GroupOper *op)
{
  int i;
/*
 * Discard any previous group contents.
 */
  clr_List(LIST_VARIABLE(op->var)->list);
/*
 * Copy the top list->n stack variables into the output list
 * variable.
 */
  for(i=0; i < (int)op->n; i++) {
    Variable *var = (Variable* )pop_ExprStack(sc, expr);
    if(!var || prepend_ListNode(LIST_VARIABLE(op->var)->list, var) == NULL)
      return 1;
  };
/*
 * Push the resulting list variable onto the stack.
 */
  if(!push_ExprStack(sc, expr, op->var))
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate a field-reference instruction.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op    FieldOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_FieldOper(Script *sc, Expr *expr, FieldOper *op)
{
  Variable *group_var;  /* The group variable whose field is to be extracted */
  ListNode *node;       /* A node of the group variable list of fields */
  int i;
/*
 * Pop the source group variable from the top of the stack.
 */
  group_var = pop_ExprStack(sc, expr);
  if(!group_var)
    return 1;
/*
 * Find the field'th field.
 */
  node = LIST_VARIABLE(group_var)->list->head;
  for(i=0; i < (int)op->field; i++)
    node = node->next;
/*
 * Push the field variable onto the stack.
 */
  if(!push_ExprStack(sc, expr, (Variable* )node->data))
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate a make-unnamed-clone-of-variable instruction.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 *  expr       Expr *   The expression being evaluated.
 *  op    CloneOper *   The context of the instruction.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int exe_CloneOper(Script *sc, Expr *expr, CloneOper *op)
{
  Variable *var;   /* The variable to be copied */
/*
 * Pop the source variable from the stack.
 */
  var = pop_ExprStack(sc, expr);
  if(!var)
    return 1;
/*
 * Make a copy of the variable.
 */
  if(copy_Variable(op->var, var) == NULL)
    return 1;
/*
 * Push the copy of the variable onto the stack.
 */
  if(!push_ExprStack(sc, expr, op->var))
    return 1;
  return 0;
}

/*.......................................................................
 * Return the tail instruction list-node of a given expression. This is
 * intended to be used to record the node that precedes a new
 * sub-expression, before the new sub-expression is parsed. It can be
 * used in conjunction with remove_alias().
 *
 * Input:
 *  e           Expr *   An expression.
 * Output:
 *  return  ListNode *   The end of the instruction list of *e.
 */
ListNode *get_ExprEnd(Expr *e)
{
  return (e && e->ops) ? e->ops->tail : NULL;
}

/*.......................................................................
 * If the sub-expression that follows 'prev' is an alias of a variable in
 * the symbol table, add an instruction to replace it with a copy of the
 * variable.
 *
 * Input:
 *  sc        Script *  The parent script.
 *  e           Expr *  The expression that contains the sub-expression
 *                      to be examined.
 *  old_end ListNode *  The value returned by get_ExprEnd(e) before the
 *                      sub-expression was parsed.
 * Output:
 *  return       int    0 - Not a lone loadvar instruction.
 *                      1 - The list contains a lone loadvar instruction.
 */
int remove_alias(Script *sc, Expr *e, ListNode *old_end)
{
/*
 * Get the list node that contains the start of the sub-expression.
 */
  ListNode *start = old_end ? old_end->next : e->ops->head;
/*
 * Get the first instruction of the sub-expression.
 */
  ExprOper *op = (ExprOper* )start->data;
/*
 * If the first instruction is a load-variable instruction,
 * and there are no further instructions in the sub-expression, then
 * the sub-expression may be an alias to a variable in the symbol table.
 */
  if(op->type == OPER_LOAD && start->next == NULL) {
    Variable *var = op->context.load.var;
/*
 * If the lone variable has a name then it is an alias to a variable
 * in the symbol table. To remove the unwanted alias, arrange for an
 * unnamed copy to be used in its place.
 */
    if(var->type->name && add_CloneOper(sc, e, var->type) == NULL)
      return 1;
  };
  return 0;
}
