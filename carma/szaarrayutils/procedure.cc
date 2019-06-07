#include <ctype.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"

/*
 * Specify the freelist blocking factors.
 */
#define SC_NCOMMAND 20
#define SC_NFUNCTION 20
#define SC_NOPERATOR 20

static VariableList *parse_ArgumentList(Script *sc, InputStream *stream,
                                        char *name, int doexport,
                                        ModifierList **mods, ListNode **opt, ScriptCmd* sCmd=0);
static Function *new_Function(Script *sc, InputStream *stream, int is_builtin, ScriptCmd* sFn=0);
static Command *new_Command(Script *sc, InputStream *stream, int is_builtin, ScriptCmd* sCmd=0);
static ModifierList *parse_Modifiers(Script *sc, InputStream *stream,
				     char *name, int doexport,
				     VariableList *args);
static Modifier *add_Modifier(Script *sc, ModifierList *mods, char *name,
			      Variable *arg);

/*.......................................................................
 * Add a new builtin function to a script environment.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  declaration   char *  The string that contains the function declaration.
 *  func_fn    FUNC_FN(*) The C function that implements the body of the
 *                        script function.
 * Output:
 *  return    Function *  The new function, or NULL on error.
 */
Function *add_BuiltinFunction(Script *sc, char *declaration, FUNC_FN(*func_fn))
{
  return add_BuiltinFunction(sc, declaration, func_fn, " ");
}

Function *add_BuiltinFunction(Script *sc, char *declaration, FUNC_FN(*func_fn), std::string description)
{
  Function *func;   /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || !declaration || !func_fn) {
    lprintf(stderr, "add_BuiltinFunction: NULL argument(s).\n");
    return NULL;
  };
/*
 * Place an input-stream wrapper around the declaration string.
 */
  if(open_StringInputStream(sc->input, 0, declaration))
    return NULL;
/*
 * Parse the function declaration.
 */
  ScriptCmd sFn;
  sFn.declaration_ = declaration;
  sFn.description_ = description;
  sFn.type_        = SCR_FUNCTION;

  func = new_Function(sc, sc->input, 1, &sFn);

/*
 * Discard the stream wrapper.
 */
  close_InputStream(sc->input);
  if(!func)
    return NULL;
/*
 * Supply the implementation of the function.
 */
  func->body.builtin.func_fn = func_fn;
  func->body.builtin.loop_fn = 0;
  func->body.builtin.loop_type = NULL;
/*
 * Add the function to the builtin symbol table.
 */
  if(!add_ScriptSymbol(sc, func->return_type->name, SYM_FUNCTION, func))
    return NULL;

  sc->functions_->insert(sc->functions_->end(), sFn);

  return func;
}

/*.......................................................................
 * Add a new loop-savy builtin function to a script environment.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  declaration   char *  The string that contains the function declaration.
 *  func_fn    FUNC_FN(*) The C function that implements the body of the
 *                        script function.
 *  loop_fn    LOOP_FN(*) On entry to a loop that calls the function,
 *                        loop_fn() will be called with first=1 and
 *                        a state variable in which to record the
 *                        starting state of the loop. On subsequent
 *                        iterations of the loop, loop_fn() will be
 *                        called with first=0 and loop_fn() should
 *                        update 'state' if necessary.
 *  datatype      char *  The name of the datatype of the loop-state
 *                        variable.
 * Output:
 *  return    Function *  The new function, or NULL on error.
 */
Function *add_LoopStateFunction(Script *sc, char *declaration,
				FUNC_FN(*func_fn), LOOP_FN(*loop_fn),
				char *datatype)
{
  Function *func;      /* The object to be returned */
  TypeSpec *loop_type; /* The type of the associated loop-state variable */
  Symbol *symbol;      /* A symbol-table entry */
/*
 * Check arguments.
 */
  if(!sc || !declaration || !func_fn || !loop_fn || !datatype) {
    lprintf(stderr, "add_LoopStateFunction: NULL argument(s).\n");
    return NULL;
  };
/*
 * Lookup the datatype of the loop-state variable.
 */
  symbol = find_ScriptSymbol(sc, NULL, datatype);
  if(!symbol || symbol->code != SYM_DATATYPE) {
    lprintf(stderr, "add_LoopStateFunction: Unknown datatype name: %s.\n",
	    datatype);
    return NULL;
  };
  loop_type = ((DataType *)symbol->data)->atom_reg;
/*
 * Place an input-stream wrapper around the declaration string.
 */
  if(open_StringInputStream(sc->input, 0, declaration))
    return NULL;
/*
 * Parse the function declaration.
 */
  func = new_Function(sc, sc->input, 1);
  if(!func)
    return NULL;
/*
 * Discard the stream wrapper.
 */
  close_InputStream(sc->input);
  if(!func)
    return NULL;
/*
 * Supply the implementation of the function.
 */
  func->body.builtin.func_fn = func_fn;
  func->body.builtin.loop_fn = loop_fn;
  func->body.builtin.loop_type = loop_type;
/*
 * Add the function to the builtin symbol table.
 */
  if(!add_ScriptSymbol(sc, func->return_type->name, SYM_FUNCTION, func))
    return NULL;
  return func;
}

/*.......................................................................
 * Add a new user-defined function to the local scope.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The stream from which to parse the function
 *                         definition.
 * Output:
 *  return     Function *  The new function, or NULL on error.
 */
Function *add_UserFunction(Script *sc, InputStream *stream)
{
  Function *func;   /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "add_UserFunction: NULL argument(s).\n");
    return NULL;
  };
/*
 * Create a new scope for the function arguments and body.
 */
  if(push_Scope(sc))
    return NULL;
/*
 * Parse the function declaration.
 */
  func = new_Function(sc, stream, 0);
  if(!func)
    return NULL;
/*
 * Allocate the container that will contain the expression that
 * the function evaluates.
 */
  func->body.user.expr = new_Expr(sc);
  if(!func->body.user.expr)
    return NULL;
/*
 * The function body must be enclosed in braces.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc != '{') {
    input_error(stream, 1, "Missing { before function expression.\n");
    return NULL;
  };
  if(input_skip_white(stream, 1, 1))
    return NULL;
/*
 * Parse the expression that the function evaluates.
 */
  if(parse_argument(sc, func->return_type, stream, func->body.user.expr))
    return NULL;
/*
 * Check for and skip the terminating '}'.
 */
  if(input_skip_white(stream, 1, 0))
    return NULL;
  if(stream->nextc != '}') {
    input_error(stream, 1, "Missing } after function expression.\n");
    return NULL;
  };
  if(input_skip_space(stream, 1, 1))
    return NULL;
/*
 * Discard the function scope.
 */
  pop_Scope(sc);
/*
 * Add the function to the user symbol table.
 */
  if(!add_ScriptSymbol(sc, func->return_type->name, SYM_FUNCTION, func))
    return NULL;
  return func;
}

/*.......................................................................
 * Add a new builtin command to a script environment.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  declaration   char *  The string that contains the command declaration.
 *  cmd_fn      CMD_FN(*) The C function that implements the body of the
 *                        script command.
 * Output:
 *  return    Command *  The new command, or NULL on error.
 */
Command *add_BuiltinCommand(Script *sc, char *declaration, CMD_FN(*cmd_fn))
{
  return add_BuiltinCommand(sc, declaration, cmd_fn, " ");
}

Command *add_BuiltinCommand(Script *sc, char *declaration, CMD_FN(*cmd_fn), std::string description)
{
  Command *cmd;   /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || !declaration || !cmd_fn) {
    lprintf(stderr, "add_BuiltinCommand: NULL argument(s).\n");
    return NULL;
  };
/*
 * Place an input-stream wrapper around the declaration string.
 */
  if(open_StringInputStream(sc->input, 0, declaration))
    return NULL;
/*
 * Parse the command declaration.
 */
  ScriptCmd sCmd;
  sCmd.declaration_ = declaration;
  sCmd.description_ = description;
  sCmd.type_        = SCR_COMMAND;

  cmd = new_Command(sc, sc->input, 1, &sCmd);

/*
 * Discard the stream wrapper.
 */
  close_InputStream(sc->input);
  if(!cmd)
    return NULL;
/*
 * Supply the implementation of the command.
 */
  cmd->body.builtin.cmd_fn = cmd_fn;
/*
 * Add the command to the builtin symbol table.
 */
  if(!add_ScriptSymbol(sc, cmd->name, SYM_COMMAND, cmd))
    return NULL;

  sc->commands_->insert(sc->commands_->end(), sCmd);

  return cmd;
}

/*.......................................................................
 * Add a new user-defined command to the local scope.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The stream from which to parse the command
 *                         definition.
 * Output:
 *  return     Command *  The new command, or NULL on error.
 */
Command *add_UserCommand(Script *sc, InputStream *stream)
{
  Command *cmd;   /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "add_UserCommand: NULL argument(s).\n");
    return NULL;
  };
/*
 * Create a new scope for the command arguments and body.
 */
  if(push_Scope(sc))
    return NULL;
/*
 * Parse the command declaration.
 */
  cmd = new_Command(sc, stream, 0);
  if(!cmd)
    return NULL;
/*
 * Parse the statement-list that the command evaluates.
 */
  cmd->body.user.stmts = parse_StatementList(sc, stream, 1);
  if(!cmd->body.user.stmts)
    return NULL;
/*
 * Discard the symbol table of the completed scope.
 */
  pop_Scope(sc);
/*
 * Add the command to the user symbol table.
 */
  if(!add_ScriptSymbol(sc, cmd->name, SYM_COMMAND, cmd))
    return NULL;
  return cmd;
}

/*.......................................................................
 * Parse and return the declaration of a function.
 * On return the caller is expected to fill in the body of the function
 * according to its type, and add the function to the symbol table.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The stream to parse from.
 *  is_builtin      int    True if the procedure is to be implemented by
 *                         a C function.
 * Output:
 *  return     Function *  The new function declaration.
 */
static Function *new_Function(Script *sc, InputStream *stream, int is_builtin, ScriptCmd* sFn)
{
  Function *func;   /* The object to be returned */
/*
 * Builtin functions can only be defined during initialization of
 * the script environment. User functions can only be defined after
 * initialization.
 */
  if(is_builtin && sc->script.state != SCRIPT_EMPTY) {
    lprintf(stderr,
	    "new_Function: Builtin's can't be added after initialization.\n");
    return NULL;
  } else if(!is_builtin && sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr,
	    "new_Function: Add user functions after initialization.\n");
    return NULL;
  };
/*
 * Allocate the function container.
 */
  func = (Function* )new_ScriptObject(sc, sc->memory.function, 0);
  if(!func)
    return NULL;
/*
 * Initialize the container.
 */
  func->return_type = NULL;
  func->args = NULL;
  func->narg = 0;
  func->opt = NULL;
  func->is_builtin = is_builtin;
  if(is_builtin) {
    BuiltinFn *body = &func->body.builtin;
    body->func_fn = 0;
    body->loop_fn = 0;
    body->loop_type = 0;
  } else {
    UserFn *body = &func->body.user;
    body->expr = NULL;
  };
/*
 * Parse the return type of the function.
 */
  func->return_type = parse_TypeSpec(sc, stream, NULL);
  if(!func->return_type)
    return NULL;
/*
 * Parse the argument list.
 */
  if(sFn != 0) {
    sFn->name_                  = func->return_type->name;
    sFn->retType_.dataTypeName_ = func->return_type->dt->name;
  }

  func->args = parse_ArgumentList(sc, stream, func->return_type->name,
				  !is_builtin, NULL,
				  is_builtin ? &func->opt : NULL, sFn);

  if(!func->args)
    return NULL;
  func->narg = func->args->nnode;
  return func;
}

/*.......................................................................
 * Parse and return the declaration of a command.
 * On return the caller is expected to fill in the body of the command
 * according to its type, and add the command to the symbol table.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The stream to parse from.
 *  is_builtin      int    True if the command is to be implemented by
 *                         a C function.
 * Output:
 *  return      Command *  The new command declaration.
 */
static Command *new_Command(Script *sc, InputStream *stream, int is_builtin, ScriptCmd* sCmd)
{
  Command *cmd;   /* The object to be returned */
/*
 * Builtin commands can only be defined during initialization of
 * the script environment. User commands can only be defined after
 * initialization.
 */
  if(is_builtin && sc->script.state != SCRIPT_EMPTY) {
    lprintf(stderr,
	    "new_Command: Builtin's can't be added after initialization.\n");
    return NULL;
  } else if(!is_builtin && sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr,
	    "new_Command: Add user commands after initialization.\n");
    return NULL;
  };
/*
 * Allocate the command container.
 */
  cmd = (Command* )new_ScriptObject(sc, sc->memory.command, 0);
  if(!cmd)
    return NULL;
/*
 * Initialize the container.
 */
  cmd->name = NULL;
  cmd->mods = NULL;
  cmd->args = NULL;
  cmd->narg = 0;
  cmd->opt = NULL;
  cmd->is_builtin = is_builtin;
  if(is_builtin) {
    BuiltinCmd *body = &cmd->body.builtin;
    body->cmd_fn = 0;
  } else {
    UserCmd *body = &cmd->body.user;
    body->stmts = NULL;
  };
/*
 * Read the name of the command.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_keyword(stream, 0, 0)) {
    input_error(stream, 1, "No name has been given for the new command.\n");
    return NULL;
  };
/*
 * Record a copy of the command name.
 */
  cmd->name = new_ScriptString(sc, stream->work);
  if(!cmd->name)
    return NULL;
/*
 * Parse the argument list.
 */
  if(sCmd != 0)
    sCmd->name_ = cmd->name;

  cmd->args = parse_ArgumentList(sc, stream, cmd->name, !is_builtin,
				 &cmd->mods, is_builtin ? &cmd->opt : NULL, sCmd);
  if(!cmd->args)
    return NULL;
  cmd->narg = cmd->args->nnode;
  return cmd;
}

/*.......................................................................
 * Parse the () enclosed argument list of a function or command.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stream   InputStream *  The stream to parse from.
 *  name            char *  The name of the parent procedure.
 *  doexport           int    If true, associate symbols with the arguments.
 * Input/Output:
 *  mods    ModifierList ** If mods!=NULL and any modifiers are declared
 *                          a list will be assigned to *mods, containing
 *                          the modifier declarations.
 *  opt         ListNode ** If opt!=NULL then optional arguments will be
 *                          allowed. On output the list-node of the first
 *                          optional argument in the returned variable
 *                          list will be assigned to *opt.
 * Output:
 *  return  VariableList *  The new argument list, or NULL on error.
 */
static VariableList *parse_ArgumentList(Script *sc, InputStream *stream,
					char *name, int doexport,
					ModifierList **mods, ListNode **opt, ScriptCmd* sCmd)
{
  VariableList *args;   /* The argument list to be returned */
  int have_opt=0;       /* True after the end of the mandatory arguments */
/*
 * Allocate the list of arguments.
 */
  args = new_VariableList(sc);
  if(!args)
    return NULL;
/*
 * Assume that there aren't any optional arguments until proved otherwise. 
 */
  if(opt)
    *opt = NULL;
/*
 * Should we parse optional boolean modifier argument declarations?
 */
  if(mods && stream->nextc=='/') {
    *mods=parse_Modifiers(sc, stream, name, doexport, args);
    if(!*mods)
      return NULL;
  };
/*
 * Check for the open parenthesis that introduces the argument
 * declarations.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc != '(') {
    input_error(stream, 1, "Missing '(' in the declaration of procedure %s.\n",
		name);
    return NULL;
  };
/*
 * Parse arguments until the terminating parenthesis is encountered.
 */
  do {
    TypeSpec *arg_type;   /* The declaration of an argument */
    Variable *arg;        /* The variable that will hold the argument */
    ListNode *node;       /* The list node that contains 'arg' */
/*
 * Skip the argument separator (or the open parenthesis before the
 * first argument).
 */
    if(input_skip_white(stream, 1, 1))
      return NULL;
/*
 * Have we hit the start of a list of optional arguments?
 */
    if(stream->nextc == '[') {
      if(!opt || *opt) {
	input_error(stream, 1,
	    "Procedure %s has an unexpected optional argument declaration.\n",
	    name);
	return NULL;
      };
/*
 * Skip the option specifier.
 */
      if(input_skip_white(stream, 1, 1))
	return NULL;
/*
 * Keep a record of having seen the '[' so that we know to record
 * the next argument list-node in *opt, and that a ']' terminator
 * should be expected.
 */
      have_opt = 1;
    };
/*
 * No more declarations?
 */
    if(!isalpha(stream->nextc))
      break;
/*
 * Get the next argument declaration.
 */
    arg_type = parse_TypeSpec(sc, stream, NULL);
    if(!arg_type)
      return NULL;
/*
 * Allocate a variable to contain the argument when the command is
 * invoked.
 */
    arg = new_Variable(sc, arg_type);
    if(!arg)
      return NULL;
/*
 * Mark it as optional?
 */
    if(have_opt)
      arg->flags |= VAR_IS_OPT;
/*
 * Add the argument to the argument list of the command.
 */
    node = append_Variable(sc, args, arg);
    if(!node)
      return NULL;
/*
 * If this is the first of the list of optional nodes, record it for
 * return.
 */
    if(have_opt && *opt==NULL)
      *opt = node;
/*
 * If we are compiling a user command, add the argument to the
 * function scope.
 */
    if(doexport && !add_ScriptSymbol(sc, arg_type->name, SYM_VARIABLE, arg))
      return NULL;
/*
 * See if there are any more arguments.
 */
    if(input_skip_space(stream, 1, 0))
      return NULL;

    // If a command container was passed in, insert this argument into                                        
    // it                                                                                                     

    if(sCmd != 0)
      sc->insert(sCmd, CmdArg(arg_type->dt->name, arg_type->name, have_opt));

  } while(stream->nextc == ',');
/*
 * Check if the next character is a terminator.
 */
  if(stream->nextc != ')' && (have_opt && stream->nextc != ']')) {
    input_error(stream, 1,
	"Missing %c terminator or comma separator in the declaration of %s.\n",
	have_opt ? ']':')', name);
    return NULL;
  };
/*
 * Do we need a terminator for an optional argument-list?
 */
  if(have_opt) {
    if(stream->nextc == ']') {
      if(input_skip_white(stream, 1, 1))
	return NULL;
    } else {
      input_error(stream, 1,
		  "Missing ']' terminator in the declaration of %s.\n",
		  name);
      return NULL;
    };
  };
/*
 * Check for, and skip, the terminating parenthesis.
 */
  if(stream->nextc != ')') {
    input_error(stream, 1, "Missing ) terminator in the declaration of %s.\n",
		name);
    return NULL;
  };
  if(input_skip_white(stream, 1, 1))
    return NULL;
  return args;
}

/*.......................................................................
 * Create a new scoperator.
 *
 * Input:
 *  sc          Script *   The host scripting environment.
 *  return_dt DataType *   The datatype that the scoperator returns.
 *  narg           int     The number of arguments that the scoperator
 *                         expects. Note that it is the duty of the
 *                         DataType::parse_fn() method to determine the
 *                         types of the arguments.
 *  oper_fn    OPER_FN(*)  The C function that implements the scoperator.
 * Output:
 *  return    ScOperator *   The new scoperator object, or NULL on error.
 */
ScOperator *new_ScOperator(Script *sc, DataType *return_dt, int narg,
		       OPER_FN(*oper_fn))
{
  ScOperator *oper;   /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || !return_dt || narg < 1 || !oper_fn) {
    lprintf(stderr, "new_ScOperator: Invalid argument(s).\n");
    return NULL;
  };
/*
 * Allocate the scoperator container.
 */
  oper = (ScOperator* )new_ScriptObject(sc, sc->memory.scoperator, 0);
  if(!oper)
    return NULL;
/*
 * Initialize the container.
 */
  oper->oper_fn = oper_fn;
  oper->return_type = NULL;
  oper->narg = narg;
/*
 * Create the type-specification object that will be used by
 * add_OpFnOper() to create the result variable of the scoperator.
 */
  oper->return_type = new_TypeSpec(sc, NULL, return_dt, 0);
  if(!oper->return_type)
    return NULL;
  return oper;
}

/*.......................................................................
 * Create a free-list from which to allocate Function objects.
 */
FunctionMem *new_FunctionMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(Function));
}

/*.......................................................................
 * Create a free-list from which to allocate Command objects.
 */
CommandMem *new_CommandMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(Command));
}

/*.......................................................................
 * Create a free-list from which to allocate ScOperator objects.
 */
ScOperatorMem *new_ScOperatorMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(ScOperator));
}

/*.......................................................................
 * Display the contents of an argument list.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  output  OutputStream *  The stream to write to.
 *  expand           int    If true, expand list and group datatype
 *                          arguments, otherwise display them as "(list)"
 *                          and "(group)".
 *  args    ArgumentList *  The list of arguments to be displayed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int print_ArgumentList(Script *sc, OutputStream *output, int expand,
		       VariableList *args)
{
  ListNode *node;     /* A node from the 'args' list */
  int iarg = 0;       /* The number of arguments seen so far */
  int nprint = 0;     /* The number of mandatory and optional arguments */
                      /*  printed. */
/*
 * Check the arguments.
 */
  if(!sc || !output || !args) {
    lprintf(stderr, "print_ArgumentList: Invalid arguments.\n");
    return 1;
  };
/*
 * Display each argument.
 */
  for(node=args->head; node; node=node->next, iarg++) {
    Variable *arg = (Variable* )node->data;
/*
 * Modifier argument?
 */
    if((arg->flags & VAR_IS_MODIFIER)) {
/*
 * If the modifier is boolean and it is true, append it to the command name.
 */
      if(arg->type->dt == sc->builtin.boolvar_dt) {
	if(BOOL_VARIABLE(arg)->boolvar) {
	  if(write_OutputStream(output, "/") ||
	     write_OutputStream(output, arg->type->name))
	    return 1;
	};
/*
 * Otherwise, it must be a string containing either "" if it hasn't been
 * set, or the name of the modifier to be written.
 */
      } else {
	char *mod_name = STRING_VARIABLE(arg)->string;
	if(*mod_name != '\0') {
	  if(write_OutputStream(output, "/") ||
	     write_OutputStream(output, mod_name))
	    return 1;
	};
      };
/*
 * Ignore optional arguments that haven't been assigned values.
 */
    } else if(!(arg->flags & VAR_IS_NUL)) {
/*
 * Write the argument separator.
 */
      if(write_OutputStream(output, nprint++ ? ", ":" "))
	return 1;
/*
 * If the argument is optional precede the displayed value with 'name='.
 */
      if(arg->flags & VAR_IS_OPT) {
	if(write_OutputStream(output, arg->type->name) ||
	   write_OutputStream(output, "="))
	  return 1;
      };
/*
 * Display the value of the variable.
 */
      if(!expand && arg->type->is_list) {
	if(write_OutputStream(output, "(list)"))
	  return 1;
      } else if(!expand && arg->type->dt->dataclass != DT_BUILTIN) {
	if(write_OutputStream(output, "(group)"))
	  return 1;
      } else {
	if(print_variable(sc, output, arg))
	  return 1;
      };
    };
  };
  return 0;
}

/*.......................................................................
 * This is a private function of parse_ArgumentList() used to parse
 * the declarations of modifier arguments.
 *
 * Input:
 *  sc             Script *  The parent script.
 *  stream    InputStream *  The stream to parse from.
 *  name             char *  The name of the parent procedure.
 *  doexport            int    If true, associate symbols with the arguments.
 *  args     VariableList *  The argument list of the procedure.
 * Output:
 *  return   ModifierList *  The list of parsed modifiers, or NULL on
 *                           error.
 */
static ModifierList *parse_Modifiers(Script *sc, InputStream *stream,
				     char *name, int doexport,
				     VariableList *args)
{
  ModifierList *mods;   /* The list to be allocated and filled */
/*
 * Allocate the list that will record the modifiers.
 */
  mods = new_ScriptList(sc);
  if(!mods)
    return NULL;
/*
 * Modifiers are declared in the same way that they are used, by preceding
 * their names by /. No spaces are allowed before each /.
 */
  while(stream->nextc == '/') {
    Variable *arg;        /* The variable that will hold the argument */
/*
 * Skip the '/' and read the name of the modifier argument.
 */
    if(read_InputStream(stream, 0) ||
       input_keyword(stream, 0, 1)) {
      input_error(stream, 1,
		  "Missing modifier name in declaration of procedure %s.\n",
		  name);
      return NULL;
    };
/*
 * Allocate the variable that will hold the value of the lastest modifier
 * group. This will be boolean if the variable name itself is to be used
 * as the modifier, or string if the variable is being assigned a list
 * of possible values.
 */
    arg = new_Variable(sc, new_TypeSpec(sc, stream->work,
	stream->nextc=='=' ? sc->builtin.string_dt : sc->builtin.boolvar_dt, 0));
    if(!arg || !append_Variable(sc, args, arg))
      return NULL;
/*
 * Mark the argument as a modifier.
 */
    arg->flags |= VAR_IS_MODIFIER;
/*
 * If we are compiling a user command, add the argument to the
 * function scope.
 */
    if(doexport && !add_ScriptSymbol(sc, arg->type->name, SYM_VARIABLE, arg))
      return NULL;
/*
 * If the variable name is followed by a list of modifier names then we
 * need to add entries to the modifier list for each of these names,
 * each one pointing to the just allocated variable. Otherwise the
 * argument name itself is the name to be added to the modifier list.
 */
    if(stream->nextc=='=') {
      do {
	if(read_InputStream(stream, 0) ||   /* Skip the '=' or '|' */
	   input_keyword(stream, 0, 1)) {   /* Read the modifier name */
	  input_error(stream, 1,
		      "Missing modifier name in declaration of procedure %s.\n",
		      name);
	  return NULL;
	};
	if(add_Modifier(sc, mods, stream->work, arg)==NULL)
	  return NULL;
      } while(stream->nextc == '|');
    } else {
      if(add_Modifier(sc, mods, stream->work, arg)==NULL)
	return NULL;
    };
  };
  return mods;
}

/*.......................................................................
 * Add a modifier record to a given list of command modifiers.
 *
 * Input:
 *  sc          Script *  The parent script.
 *  mods  ModifierList *  The list to append the modifier to.
 *  name          char *  The name of the modifier.
 *  arg       Variable *  The command argument associated with the
 *                        modifier.
 * Output:
 *  return    Modifier *  The new modifier record, or NULL on error.
 */
static Modifier *add_Modifier(Script *sc, ModifierList *mods, char *name,
			      Variable *arg)
{
/*
 * Get the modifier to be added.
 */
  Modifier *m = (Modifier* )new_ScriptObject(sc, NULL, sizeof(Modifier));
  if(!m)
    return NULL;
/*
 * Initialize it.
 */
  m->name = new_ScriptString(sc, name);
  m->arg = arg;
  if(!m->name)
    return NULL;
/*
 * Add the modifier to the list.
 */
  if(!append_ListNode(mods, m))
    return NULL;
  return m;
}
