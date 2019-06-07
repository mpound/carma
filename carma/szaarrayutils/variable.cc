#include <string.h>

#include "carma/szaarrayutils/script.h"
#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/lprintf.h"

/*.......................................................................
 * Print the value of a variable to a given output stream.
 *
 * Input:
 *  sc           Script *   The parent script.
 *  stream  OutputStream *  The stream to write to.
 *  var        Variable *   The variable who's value is to be displayed.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
int print_variable(Script *sc, OutputStream *output, Variable *var)
{
/*
 * Check arguments.
 */
  if(!sc || !var || !output) {
    lprintf(stderr, "print_variable: Invalid arguments.\n");
    return 1;
  };
/*
 * Lists are not concrete datatypes, so include special code to
 * display them.
 */
  if(var->type->is_list) {
    ListNode *node;    /* A node in the list of variables */
/*
 * Enclose the list in braces.
 */
    if(write_OutputStream(output, "{"))
      return 1;
/*
 * Display list members separated by commas.
 */
    for(node=LIST_VARIABLE(var)->list->head; node; node=node->next) {
      Variable *member = (Variable* )node->data;
      if(print_variable(sc, output, member))
	return 1;
      if(node->next && write_OutputStream(output, ", "))
	return 1;
    };
    if(write_OutputStream(output, "}"))
      return 1;
/*
 * Defer display of concrete datatypes to appropriate datatype
 * method function.
 */
  } else {
    if(var->type->dt->print_fn(sc, output, var))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Create a new variable.
 *
 * Input:
 *  sc           Script *  The host script environment.
 *  type       TypeSpec *  The type of variable to create.
 * Output:
 *  return     Variable *  The new variable, or NULL on error.
 */
Variable *new_Variable(Script *sc, TypeSpec *type)
{
  Variable *var;   /* The variable to be returned */
/*
 * Check arguments.
 */
  if(!sc || !type) {
    lprintf(stderr, "new_Variable: NULL argument(s).\n");
    return NULL;
  };
/*
 * Allocate the container of the variable.
 */
  var = (Variable* )new_ScriptObject(sc, type->is_list ?
			 sc->memory.list_var : type->dt->vmemory, 0);
  if(!var)
    return NULL;
/*
 * Initialize the value of the variable.
 */
  var->type = type;
  var->flags = 0U;
  if(type->is_list) {
    LIST_VARIABLE(var)->list = NULL;
  } else {
    memset((char *)var + sizeof(Variable), 0,
	   type->dt->vsize - sizeof(Variable));
  };
  return var;
}

/*.......................................................................
 * Create an empty list of variables.
 *
 * Input:
 *  sc           Script *  The host script environment.
 * Output:
 *  return VariableList *  The new list, or NULL on error.
 */
VariableList *new_VariableList(Script *sc)
{
  if(!sc) {
    lprintf(stderr, "new_VariableList: NULL argument.\n");
    return NULL;
  };
/*
 * Allocate and return a generic list container.
 */
  return new_ScriptList(sc);
}

/*.......................................................................
 * Append a variable to a list of variables.
 *
 * Input:
 *  sc         Script *   The host script environment.
 *  vl   VariableList *   The list to append to.
 *  var      Variable *   The variable to append to the list.
 * Output:
 *  return   ListNode *   The list node that contains the variable.
 */
ListNode *append_Variable(Script *sc, VariableList *vl, Variable *var)
{
  return append_ListNode(vl, var);
}

/*.......................................................................
 * Copy the value of one variable to another of the same type.
 *
 * Input:
 *  dst     Variable *   The destination variable.
 *  src     Variable *   The variable to be copied.
 * Output:
 *  return  Variable *   The same as dst, or NULL on error.
 */
Variable *copy_Variable(Variable *dst, Variable *src)
{
  TypeSpec *type;  /* The type of the two variables */
/*
 * Check the arguments.
 */
  if(!dst) {
    lprintf(stderr, "copy_Variable: NULL destination variable.\n");
    return NULL;
  };
/*
 * Get the declared type of the destination variable.
 */
  type = dst->type;
/*
 * The empty source variables are allowed if the destination
 * variable is marked as being optional.
 */
  if(!src || src->flags & VAR_IS_NUL) {
    if(dst->flags | VAR_IS_OPT) {
      dst->flags |= VAR_IS_NUL;
      return dst;
    } else {
      lprintf(stderr, "copy_Variable: Empty source variable.\n");
      return NULL;
    };
  };
/*
 * The variables must be of the same type.
 */
  if(src->type->dt != type->dt ||
     !src->type->is_list != !type->is_list) {
    lprintf(stderr, "copy_Variable: Incompatible variables.\n");
    return NULL;
  };
/*
 * Avoid copying a variable to itself.
 * Copy just the derived part of the variable. This is the part that
 * follows the base-dataclass (Variable) member.
 */
  if(src != dst) {
    if(type->is_list) {
      LIST_VARIABLE(dst)->list = LIST_VARIABLE(src)->list;
    } else {
      memcpy((char *)dst + sizeof(Variable), (char *)src + sizeof(Variable),
	     type->dt->vsize - sizeof(Variable));
    };
/*
 * Mark the variable having a value.
 */
    dst->flags &= ~VAR_IS_NUL;
  };
  return dst;
}

/*.......................................................................
 * Return pointers to the variables of an argument list.
 *
 * Input:
 *  vl        VariableList *  The argument list.
 *  ...
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int get_Arguments(VariableList *vl, ...)
{
  ListNode *node;   /* A node of the argument list */
  va_list ap;       /* The variable-argument list of this function */
  int waserr = 0;   /* True after an error occurs */
/*
 * Null argument list?
 */
  if(!vl) {
    lprintf(stderr, "get_Arguments: NULL argument list.\n");
    return 1;
  };
/*
 * Record the argument pointers in the specified return variables.
 */
  va_start(ap, vl);
  for(node=vl->head; node; node=node->next) {
    Variable **vret = va_arg(ap, Variable **);
    if(!vret)
      break;
    *vret = (Variable* )node->data;
  };
/*
 * Were there too few return arguments?
 */
  if(node) {
    lprintf(stderr, "get_Arguments: Too few arguments requested.\n");
    waserr = 1;
/*
 * If we ended because the input argument list completed, make sure that
 * the next argument is NULL.
 */
  } else if(va_arg(ap, Variable *)) {
    lprintf(stderr, "get_Arguments: Missing NULL argument terminator?\n");
    waserr = 1;
  };
  va_end(ap);
  return waserr;
}

/*.......................................................................
 * Create a new type-specification object.
 *
 * Input:
 *  sc       Script *  The host scripting environment.
 *  name       char *  The name to associate with the type, or NULL.
 *                     A copy will be made of the input string.
 *  dt     DataType *  The data-type being named.
 *  is_list     int    True if the type is a list of 'dt'.
 * Output:
 *  return TypeSpec *  The new type-specification object, or NULL on
 *                     error.
 */
TypeSpec *new_TypeSpec(Script *sc, char *name, DataType *dt, int is_list)
{
  TypeSpec *ts;     /* The object to be returned */
/*
 * Check inputs.
 */
  if(!sc || !dt) {
    lprintf(stderr, "new_TypeSpec: NULL arguments.\n");
    return NULL;
  };
/*
 * Allocate the container.
 */
  ts = (TypeSpec* )new_ScriptObject(sc, sc->memory.typespec, 0);
  if(!ts)
    return NULL;
/*
 * Initialize the container.
 */
  ts->name = NULL;
  ts->dt = dt;
  ts->is_list = is_list;
/*
 * Allocate a copy of the input name.
 */
  if(name) {
    ts->name = new_ScriptString(sc, name);
    if(!ts->name)
      return NULL;
  };
  return ts;
}

/*.......................................................................
 * Create an empty list of type-specification objects.
 *
 * Input:
 *  sc      Script *  The host scripting environment.
 */
TypeSpecList *new_TypeSpecList(Script *sc)
{
  return new_ScriptList(sc);
}

/*.......................................................................
 * Append a type-specifier to a list of type-specifiers.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  tl  TypeSpecList *  The list to append to.
 *  ts      TypeSpec *  The type-specification object to add to the list.
 * Output:
 *  return  TypeSpec *  The same as 'ts', or NULL on error.
 */
TypeSpec *append_TypeSpec(Script *sc, TypeSpecList *tl, TypeSpec *ts)
{
  if(!append_ListNode(tl, ts))
    return NULL;
  return ts;
}

/*.......................................................................
 * Parse a type specification from an input stream.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  stream InputStream *  The stream to parse from.
 *  symbol      Symbol *  If the first symbol of the specification has
 *                        already been read, provide it here. Otherwise
 *                        send NULL.
 * Output:
 *  return    TypeSpec *  The type-specification object, or NULL on error.
 */
TypeSpec *parse_TypeSpec(Script *sc, InputStream *stream, Symbol *symbol)
{
  int is_list = 0;   /* True if the type is a list */
  DataType *dt;      /* The datatype of the type */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "parse_TypeSpec: NULL argument(s).\n");
    return NULL;
  };
/*
 * Get the first symbol.
 */
  if(!symbol) {
    if(input_skip_space(stream, 1, 0))
      return NULL;
    if(input_keyword(stream, 0, 0)) {
      input_error(stream, 1,
		"Missing datatype name or 'listof' keyword in declaration.\n");
      return NULL;
    };
    symbol = find_ScriptSymbol(sc, stream, stream->work);
    if(!symbol)
      return NULL;
  };
/*
 * The first symbol must either be listof, or the datatype.
 */
  if(symbol->code == SYM_LISTOF_KEYWORD) {
    is_list = 1;
/*
 * Read the following datatype symbol.
 */
    if(input_skip_space(stream, 1, 0))
      return NULL;
    if(input_keyword(stream, 0, 0)) {
      input_error(stream, 1, "Missing datatype name after 'listof'.\n");
      return NULL;
    };
    symbol = find_ScriptSymbol(sc, stream, stream->work);
    if(!symbol)
      return NULL;
  };
/*
 * The next symbol must name a datatype.
 */
  if(symbol->code != SYM_DATATYPE) {
    input_error(stream, 1, "Symbol '%s' isn't the name of a datatype.\n",
		symbol->name);
    return NULL;
  };
  dt = (DataType* )symbol->data;
/*
 * Get the name associated with the type.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_keyword(stream, 0, 0)) {
    input_error(stream, 1,
		"No symbol name has been given to the declared variable.\n");
    return NULL;
  };
/*
 * Allocate the type-specification object.
 */
  return new_TypeSpec(sc, stream->work, dt, is_list);
}

/*.......................................................................
 * Create a free-list from which to allocate TypeSpec objects.
 */
TypeSpecMem *new_TypeSpecMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(TypeSpec));
}

/*.......................................................................
 * Create a free-list from which to allocate ListVariable objects.
 */
ListVariableMem *new_ListVariableMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(ListVariable));
}

/*.......................................................................
 * Create a builtin variable.
 *
 * Note that on return the variable is marked as not having a value.
 * Once you have assigned a valid value, mark the variable as having
 * value, by executing:  var->flags &= ~VAR_IS_NUL;
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  declaration   char *  The declaration of the variable (type, followed
 *                        by name).
 * Output:
 *  return    Variable *  The new variable, or NULL on error.
 */
Variable *add_BuiltinVariable(Script *sc, char *declaration)
{
  Variable *var;    /* The variable to be returned */
  TypeSpec *ts;     /* The type-specification of the variable */
/*
 * Check the arguments.
 */
  if(!sc || !declaration) {
    lprintf(stderr, "add_BuiltinVariable: NULL argument(s).\n");
    return NULL;
  };
/*
 * Place an input-stream wrapper around the declaration string.
 */
  if(open_StringInputStream(sc->input, 0, declaration))
    return NULL;
/*
 * Parse the variable declaration.
 */
  ts = parse_TypeSpec(sc, sc->input, NULL);
  close_InputStream(sc->input);
  if(!ts)
    return NULL;
/*
 * Create the variable.
 */
  var = new_Variable(sc, ts);
  if(!var)
    return NULL;
/*
 * Mark the variable as not having a value.
 */
  var->flags = VAR_IS_NUL;
/*
 * Add the command to the builtin symbol table.
 */
  if(!add_ScriptSymbol(sc, ts->name, SYM_VARIABLE, var))
    return NULL;
  return var;
}
