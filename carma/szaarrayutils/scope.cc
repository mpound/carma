
#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"

/*.......................................................................
 * Push a new hash table onto the stack of program symbol tables. This
 * will contain the symbols of a new inner scope.
 *
 * Input:
 *  sc     Script *   The host script environment.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
int push_Scope(Script *sc)
{
  HashTable *hash;   /* The new symbol table */
/*
 * All builtin symbols are added to a special builtin scope.
 */
  if(sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr, "push_Scope: Not ready for program scopes.\n");
    return 1;
  };
/*
 * Create the new hash table.
 */
  hash = new_HashTable(sc->memory.hashtable, SC_HASH_SIZE, HONOUR_CASE,
		       NULL, 0);
  if(!hash)
    return 1;
/*
 * Push the table on to the scope stack.
 */
  if(prepend_ListNode(sc->script.scopes, hash) == NULL) {
    hash = del_HashTable(hash);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Remove and discard the symbol table at the top of the stack of scopes.
 *
 * Input:
 *  sc     Script *   The host script environment.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
int pop_Scope(Script *sc)
{
/*
 * Is there a symbol table to be removed?
 */
  if(!sc->script.scopes->head) {
    lprintf(stderr, "pop_Scope: There is no scope to pop.\n");
    return 1;
  };
/*
 * Remove the hash table from the top of the stack and
 * delete it.
 */
  del_HashTable((HashTable *)del_ListNode(sc->script.scopes, sc->script.scopes->head, NULL));
  return 0;
}

/*.......................................................................
 * Add a script symbol to the innermost scope.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  name        char *  The name of the symbol to be added.
 *  code  SymbolType    The dataclass of symbol to be added.
 *  data        void *  The data to associate with the symbol.
 * Output:
 *  return    Symbol *  The symbol container.
 */
Symbol *add_ScriptSymbol(Script *sc, char *name, SymbolType code, std::string description)
{
  // Parse the command declaration.

  ScriptCmd sFn;

  sFn.name_ = name;
  sFn.declaration_ = name;
  sFn.description_ = description;
  sFn.type_        = SCR_SYMBOL;

  sc->symbols_->insert(sc->symbols_->end(), sFn);

  return add_ScriptSymbol(sc, name, code, NULL);
}

Symbol *add_ScriptSymbol(Script *sc, char *name, SymbolType code, void *data)
{
  HashTable *hash;   /* The symbol table to add the symbol to */
/*
 * Find the appropriate hash table.
 */
  if(sc->script.state == SCRIPT_EMPTY) {
    hash = sc->builtin.symbols;
  } else if(!sc->script.scopes->head) {
    lprintf(stderr, "add_ScriptSymbol: No scope exists yet.\n");
    return NULL;
  } else {
    hash = (HashTable *)sc->script.scopes->head->data;
  };
/*
 * Add the symbol to the symbol table.
 */
  return new_HashSymbol(hash, name, (int) code, 0, data, 0);
}

/*.......................................................................
 * Work back from the innermost scope to locate a named symbol.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  name           char *  The name of the symbol to be sought.
 *  stream  InputStream *  Optional. If a stream is provided,
 *                         input_error(stream,...) will be used to
 *                         report errors. Otherwise lprintf() will be
 *                         used.
 * Output:
 *  return       Symbol *  The container of the located symbol.
 */
Symbol *find_ScriptSymbol(Script *sc, InputStream *stream, char *name)
{
  ListNode *node;    /* A node of the scope stack */
  Symbol *symbol;    /* The located symbol */
/*
 * Look from the innermost to the outermost script-scope.
 */
  for(node=sc->script.scopes->head; node; node=node->next) {
    HashTable *hash = (HashTable* )node->data;
    Symbol *symbol = find_HashSymbol(hash, name);
    if(symbol)
      return symbol;
  };
/*
 * Not a script-defined symbol, so look in the table of builtin's.
 */
  symbol = find_HashSymbol(sc->builtin.symbols, name);
  if(symbol)
    return symbol;
/*
 * Symbol not found.
 */
  if(stream)
    input_error(stream, 1, "Symbol '%s' is unknown.\n", name);
  else
    lprintf(stderr, "Symbol '%s' is unknown.\n", name);
  return NULL;
}

/*.......................................................................
 * This is a wrapper around find_ScriptSymbol(), used to lookup a
 * datatype by name.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  name           char *  The name of the datatype to be sought.
 *  stream  InputStream *  Optional. If a stream is provided,
 *                         input_error(stream,...) will be used to
 *                         report errors. Otherwise lprintf() will be
 *                         used.
 * Output:
 *  return     DataType *  The located datatype, or NULL if not found.
 */
DataType *find_DataType(Script *sc, InputStream *stream, char *name)
{
/*
 * Lookup the specified symbol.
 */
  Symbol *sym = find_ScriptSymbol(sc, stream, name);
  if(!sym)
    return NULL;
/*
 * Check that the located symbol refers to a datatype.
 */
  if(sym->code != SYM_DATATYPE) {
    if(stream)
      input_error(stream, 1, "Symbol \"%s\" isn't a datatype.\n", name);
    else
      lprintf(stderr, "Symbol \"%s\" isn't a datatype.\n", name);
  };
  return (DataType *) sym->data;
}
