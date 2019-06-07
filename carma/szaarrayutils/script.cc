#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"
#include "carma/szaarrayutils/pathname.h"

#include "carma/szautil/Exception.h"

/*
 * Specify the blocking factors of various free-lists.
 */
#define SC_NHASH_TABLE 50
#define SC_NHASH_NODE (10*(SC_HASH_SIZE))
#define SC_NLIST 100
#define SC_NLIST_NODE 200

/*
 * The block size of freelists (bytes) and the minimum blocking factor.
 */
#define SC_BLOCK_SIZE 1024U

static int bad_script(Script *sc);

/*
 * Prototype generic builtin commands.
 */
static CMD_FN(sc_whatis_cmd);
static int whatis_Variable(Script *sc, Variable *var);
static int whatis_Function(Script *sc, Function *func);
static int whatis_Command(Script *sc, Command *cmd);
static int whatis_DataType(Script *sc, DataType *dt);

static VariableList *script_args(Script *sc, InputStream *stream,
				 InputStream *argstr);
static DT_CHECK(sc_check_input_file);
static FUNC_FN(sc_signaled_fn);

/*.......................................................................
 * Create a new interpretter in which to compile and run SZA control
 * schedule scripts.
 *
 * Input:
 *  project      void *   Optional project-specific data to record in
 *                        script->project, for use by the methods of
 *                        project-specific data-types and procedures.
 *  new_fn  SC_NEW_FN(*)  Create script-specific project data. This
 *                        will be called after the script object has
 *                        otherwise been completely allocated and
 *                        initialized.
 *  clr_fn  SC_CLR_FN(*)  Prepare the data object created by new_fn()
 *                        for the compilation of a new script. This is
 *                        called just after the call to new_fn(), and
 *                        at the start of discard_script().
 *  del_fn  SC_DEL_FN(*)  Delete the data object created by new_fn().
 *                        This will be called at the start of del_Script(),
 *                        after a call to discard_script().
 *  signals HashTable *   An optional symbol table of signals, in which
 *                        the code field of each symbol-table entry, is
 *                        incremented whenever the corresponding signal
 *                        is received. This allows multiple scripts to
 *                        share the same signal names.
 *                        On entry ref_HashTable() is called on "signals",
 *                        and when the script is deleted, del_HashTable()
 *                        is called. Beware that this will delete the
 *                        caller's copy of the table, unless the caller
 *                        has called ref_HashTable() once as well.
 *                        If "signals" is supplied as NULL, an internal
 *                        hashtable is allocated.
 *                         
 * Output:
 *  return     Script *   The interpretter.
 */
Script *new_Script(void *project, SC_NEW_FN(*new_fn), SC_CLR_FN(*clr_fn),
		   SC_DEL_FN(*del_fn), HashTable *signals)
{
  Script *sc;      /* The object to be returned */
/*
 * Allocate the container.
 */
  sc = (Script *) malloc(sizeof(Script));
  if(!sc) {
    lprintf(stderr, "new_Script: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be
 * passed to del_Script().
 */
  sc->commands_  = 0;
  sc->functions_ = 0;
  sc->symbols_   = 0;

  sc->dataTypes_ = 0;

  sc->project = project;
  sc->data = NULL;
  sc->clr_fn = clr_fn;
  sc->del_fn = del_fn;
  sc->input = NULL;
  sc->output = NULL;
  sc->host = NULL;
  sc->memory.hashtable = NULL;
  sc->memory.list = NULL;
  sc->memory.command = NULL;
  sc->memory.function = NULL;
  sc->memory.scoperator = NULL;
  sc->memory.datatype = NULL;
  sc->memory.statement = NULL;
  sc->memory.loopstate = NULL;
  sc->memory.expr = NULL;
  sc->memory.exproper = NULL;
  sc->memory.typespec = NULL;
  sc->memory.exeframe = NULL;
  sc->memory.list_var = NULL;
  sc->memory.freelists = NULL;
  sc->builtin.symbols = NULL;
  sc->builtin.strings = NULL;
  sc->builtin.wildcard_dt = NULL;
  sc->builtin.boolvar_dt = NULL;
  sc->builtin.string_dt = NULL;
  sc->builtin.symbol_dt = NULL;
  sc->builtin.double_dt = NULL;
  sc->builtin.integer_dt = NULL;
  sc->builtin.sexagesimal_dt = NULL;
  sc->builtin.input_file_dt = NULL;
  sc->builtin.signal_dt = NULL;
  sc->script.name = NULL;
  sc->script.args = NULL;
  sc->script.cleanup.stmts = NULL;
  sc->script.cleanup.reason = NULL;
  sc->script.state = SCRIPT_EMPTY;
  sc->script.scopes = NULL;
  sc->script.comp_stack = NULL;
  sc->script.exe_stack = NULL;
  sc->script.stmts = NULL;
  sc->script.signals = NULL;
  sc->script.catch_list = NULL;
  sc->script.temporary.lists = NULL;
  sc->script.temporary.objects = NULL;
  sc->script.temporary.strings = NULL;

  sc->commands_  = new std::list<ScriptCmd>();
  sc->functions_ = new std::list<ScriptCmd>();
  sc->symbols_   = new std::list<ScriptCmd>();

  sc->dataTypes_ = new std::list<ScriptDataType>();
  sc->interactive_ = false;
/*
 * Allocate unassigned input stream for parsing.
 */
  sc->input = new_InputStream();
  if(!sc->input)
    return del_Script(sc);
/*
 * Create a stream wrapper around lprintf(stdout) for optional
 * use during printing.
 */
  sc->output = new_OutputStream();
  if(!sc->output || open_StdoutStream(sc->output))
    return del_Script(sc);
/*
 * Get the name of the host computer.
 */
  sc->host = get_name_of_host();
  if(!sc->host)
    return del_Script(sc);
/*
 * Allocate a freelist for hash-tables.
 */
  sc->memory.hashtable = new_HashMemory(SC_NHASH_TABLE, SC_NHASH_NODE);
  if(!sc->memory.hashtable)
    return del_Script(sc);
/*
 * Allocate a freelist for the generic lists that are used in
 * builtin procedures and datatypes.
 */
  sc->memory.list = new_ListMemory(SC_NLIST, SC_NLIST_NODE);
  if(!sc->memory.list)
    return del_Script(sc);
/*
 * Allocate an empty list of free-lists. Whenever a free-list
 * is needed for a new object type, new_ScriptFreeList() first
 * looks in this list to see if a compatible freelist already
 * exists. If it does, it returns it. Otherwise it creates the
 * freelist and adds it to the list.
 */
  sc->memory.freelists = new_List(sc->memory.list);
  if(!sc->memory.freelists)
    return del_Script(sc);
/*
 * Get free-lists for use in allocating script objects.
 * Note that the returned free-lists are shared freelists from
 * sc->memory.freelists, so they must not be deleted or cleared
 * through the following pointers.
 */
  sc->memory.command = new_CommandMem(sc);
  if(!sc->memory.command)
    return del_Script(sc);
  sc->memory.function = new_FunctionMem(sc);
  if(!sc->memory.function)
    return del_Script(sc);
  sc->memory.scoperator = new_ScOperatorMem(sc);
  if(!sc->memory.scoperator)
    return del_Script(sc);
  sc->memory.datatype = new_DataTypeMem(sc);
  if(!sc->memory.datatype)
    return del_Script(sc);
  sc->memory.statement = new_StatementMem(sc);
  if(!sc->memory.statement)
    return del_Script(sc);
  sc->memory.loopstate = new_LoopStateMem(sc);
  if(!sc->memory.loopstate)
    return del_Script(sc);
  sc->memory.expr = new_ExprMem(sc);
  if(!sc->memory.expr)
    return del_Script(sc);
  sc->memory.exproper = new_ExprOperMem(sc);
  if(!sc->memory.exproper)
    return del_Script(sc);
  sc->memory.typespec = new_TypeSpecMem(sc);
  if(!sc->memory.typespec)
    return del_Script(sc);
  sc->memory.exeframe = new_ExeFrameMem(sc);
  if(!sc->memory.exeframe)
    return del_Script(sc);
  sc->memory.list_var = new_ListVariableMem(sc);
  if(!sc->memory.list_var)
    return del_Script(sc);
/*
 * Allocate a symbol table for builtin procedures and datatypes.
 */
  sc->builtin.symbols = new_HashTable(sc->memory.hashtable, SC_HASH_SIZE,
				      HONOUR_CASE, NULL, 0);
  if(!sc->builtin.symbols)
    return del_Script(sc);
/*
 * Allocate memory for the string components of builtin objects.
 */
  sc->builtin.strings = new_StringPool();
  if(!sc->builtin.strings)
    return del_Script(sc);
/*
 * Allocate the list in which to record a stack of scoped symbol tables
 * during compilation of a script.
 */
  sc->script.scopes = new_List(sc->memory.list);
  if(!sc->script.scopes)
    return del_Script(sc);
/*
 * Allocate the list in which to record a stack of nested statements
 * during compilation of a script.
 */
  sc->script.comp_stack = new_List(sc->memory.list);
  if(!sc->script.comp_stack)
    return del_Script(sc);
/*
 * If we have a shared symbol table of signals, use it. Otherwise
 * allocate a private symbol table.
 */
  if(signals) {
    sc->script.signals = ref_HashTable(signals);
  } else {
    sc->script.signals = new_HashTable(sc->memory.hashtable, SC_HASH_SIZE,
				       HONOUR_CASE, NULL, 0);
    if(!sc->script.signals)
      return del_Script(sc);
  };
/*
 * Allocate the list that will be used to record the clauses of nested
 * catch statements.
 */
  sc->script.catch_list = new_List(sc->memory.list);
  if(!sc->script.catch_list)
    return del_Script(sc);
/*
 * Allocate a list to use to record the temporary lists that should be
 * discarded when a script has been completed.
 */
  sc->script.temporary.lists = new_List(sc->memory.list);
  if(!sc->script.temporary.lists)
    return del_Script(sc);
/*
 * Allocate a pool from which to allocate temporary script strings.
 */
  sc->script.temporary.strings = new_StringPool();
  if(!sc->script.temporary.strings)
    return del_Script(sc);
/*
 * Create mandatory builtin datatypes.
 */
  sc->builtin.wildcard_dt = add_WildcardDataType(sc);
  if(!sc->builtin.wildcard_dt)
    return del_Script(sc);

  sc->builtin.boolvar_dt = add_BooleanDataType(sc);
  if(!sc->builtin.boolvar_dt)
    return del_Script(sc);

  sc->builtin.string_dt = add_StringDataType(sc, "String", 1, 0);
  if(!sc->builtin.string_dt)
    return del_Script(sc);

  sc->builtin.symbol_dt = add_SymbolDataType(sc);
  if(!sc->builtin.symbol_dt)
    return del_Script(sc);

  sc->builtin.double_dt = add_DoubleDataType(sc, "Double", 0, sc_iterate_double,
					     "Double", 1);
  if(!sc->builtin.double_dt)
    return del_Script(sc);

  sc->builtin.integer_dt = add_IntDataType(sc, "Integer", 0, sc_iterate_int,
					   "Integer", 1);
  if(!sc->builtin.integer_dt)
    return del_Script(sc);

  sc->builtin.sexagesimal_dt = add_SexagesimalDataType(sc, "Sexagesimal", 0,
						       sc_iterate_double,
						       "Sexagesimal", 1);
  if(!sc->builtin.sexagesimal_dt)
    return del_Script(sc);

  sc->builtin.input_file_dt = add_PathDataType(sc, "InputFile",
					       sc_check_input_file);
  if(!sc->builtin.input_file_dt)
    return del_Script(sc);

  sc->builtin.signal_dt = add_SignalDataType(sc, "Signal");
  if(!sc->builtin.signal_dt)
    return del_Script(sc);

/*
 * Assign symbols to the reserved words of the language.
 */
  if(!add_ScriptSymbol(sc, "function",SYM_FUNCTION_KEYWORD, "Define a function") ||
     !add_ScriptSymbol(sc, "command", SYM_COMMAND_KEYWORD,  "Define a command")  ||
     !add_ScriptSymbol(sc, "listof",  SYM_LISTOF_KEYWORD,   "Create a list of objects")  ||
     !add_ScriptSymbol(sc, "group",   SYM_GROUP_KEYWORD,    "Create a group of objects")  ||
     !add_ScriptSymbol(sc, "foreach", SYM_FOREACH_KEYWORD,  "Iterate over a list")  ||
     !add_ScriptSymbol(sc, "do",      SYM_DO_KEYWORD,       "Iterate over a variable")  ||
     !add_ScriptSymbol(sc, "until",   SYM_UNTIL_KEYWORD,    "Wait for a condition to become true")  ||
     !add_ScriptSymbol(sc, "while",   SYM_WHILE_KEYWORD,    "Execute commands while a condition is true")  ||
     !add_ScriptSymbol(sc, "if",      SYM_IF_KEYWORD,       "Execute commands if a condition is true")  ||
     !add_ScriptSymbol(sc, "ifhost",  SYM_IFHOST_KEYWORD,   "Execute commands if running on the specified host machine") ||
     !add_ScriptSymbol(sc, "print",   SYM_PRINT_KEYWORD,    "Print a variable or expression") ||
     !add_ScriptSymbol(sc, "log",     SYM_LOG_KEYWORD,      "Write a statement to the control system log file") ||
     !add_ScriptSymbol(sc, "break",   SYM_BREAK_KEYWORD,    "Break out of a conditional or loop") ||
     !add_ScriptSymbol(sc, "next",    SYM_NEXT_KEYWORD,     "") ||
     !add_ScriptSymbol(sc, "exit",    SYM_EXIT_KEYWORD,     "Exit the current schedule") ||
     !add_ScriptSymbol(sc, "import",  SYM_IMPORT_KEYWORD,   "Import function definitions from another file") ||
     !add_ScriptSymbol(sc, "cleanup", SYM_CLEANUP_KEYWORD,  "") ||
     !add_ScriptSymbol(sc, "return",  SYM_RETURN_KEYWORD,   "Return from a function") ||
     !add_ScriptSymbol(sc, "catch",   SYM_CATCH_KEYWORD,    "Execute statements if a signal is caught"))
    return del_Script(sc);
/*
 * Create builtin commands.
 */
  if(!add_BuiltinCommand(sc, "whatis(Symbol symbol)", sc_whatis_cmd) ||
     !add_BuiltinFunction(sc, "Boolean signaled(Signal signal)",
			  sc_signaled_fn))
    return del_Script(sc);
/*
 * Allocate the script-specific data object if needed.
 */
  if(new_fn) {
    sc->data = new_fn(sc);
    if(!sc->data || (clr_fn && clr_fn(sc, sc->data)))
      return del_Script(sc);
  };
  return sc;
}

/*.......................................................................
 * Delete an interpretter and its contents.
 *
 * Input:
 *  sc     Script *   The interpretter to be deleted.
 * Output:
 *  return Script *   The deleted interpretter (always NULL).
 */
Script *del_Script(Script *sc)
{
  char *caller = "del_Script"; /* The name of this function */
  ListNode *node;              /* A node of a generic list */
  if(sc) {
/*
 * Discard the objects of the latest script.
 */
    discard_script(sc);
/*
 * Delete project-specific data.
 */
    if(sc->del_fn)
      sc->data = sc->del_fn(sc, sc->data);
    sc->script.temporary.lists = del_List(sc->script.temporary.lists);
    sc->script.temporary.strings = del_StringPool(sc->script.temporary.strings);
    sc->script.scopes = del_List(sc->script.scopes);
    sc->script.comp_stack = del_List(sc->script.comp_stack);
    sc->script.signals = del_HashTable(sc->script.signals);
    sc->script.catch_list = del_List(sc->script.catch_list);
/*
 * Delete builtin members.
 */
    sc->builtin.strings = del_StringPool(sc->builtin.strings);
    sc->builtin.symbols = del_HashTable(sc->builtin.symbols);
/*
 * Delete the temporary streams.
 */
    sc->input = del_InputStream(sc->input);
    sc->output = del_OutputStream(sc->output);
/*
 * Delete the host-name string.
 */
    if(sc->host)
      free(sc->host);
/*
 * Before globally deleting all lists, traverse the list of allocated
 * freelists and delete its members.
 */
    for(node=sc->memory.freelists->head; node; node=node->next)
      node->data = del_FreeList(caller, (FreeList *)node->data, 1);
/*
 * Discard specialized free-lists.
 */
    sc->memory.hashtable = del_HashMemory(sc->memory.hashtable, 1);
    sc->memory.list = del_ListMemory(sc->memory.list, 1);
/*
 * The following members were pointers to freelists that
 * have just been deleted from sc->memory.freelists. Don't attempt
 * to delete them again!
 */
    sc->memory.command = NULL;
    sc->memory.function = NULL;
    sc->memory.scoperator = NULL;
    sc->memory.datatype = NULL;
    sc->memory.statement = NULL;
    sc->memory.loopstate = NULL;
    sc->memory.expr = NULL;
    sc->memory.exproper = NULL;
    sc->memory.typespec = NULL;
    sc->memory.exeframe = NULL;
    sc->memory.list_var = NULL;

    if(sc->commands_)
      delete sc->commands_;
    sc->commands_ = 0;

    if(sc->functions_)
      delete sc->functions_;
    sc->functions_ = 0;

    if(sc->symbols_)
      delete sc->symbols_;
    sc->symbols_ = 0;

    if(sc->dataTypes_)
      delete sc->dataTypes_;
    sc->dataTypes_ = 0;

/*
 * Delete the container.
 */
    free(sc);
  };
  return NULL;
}

/*.......................................................................
 * Allocate a script string from the appropriate string pool.
 *
 * Input:
 *  sc    Script *  The host scripting environment.
 *  string  char *  The string to allocate a copy of.
 * Output:
 *  return  char *  The new string, or NULL on error.
 */
char *new_ScriptString(Script *sc, char *string)
{
  if(!sc || !string) {
    lprintf(stderr, "new_ScriptString: Insufficient memory.\n");
    return NULL;
  };
/*
 * Allocate a copy of the string.
 */
  return new_StringPool_string(sc->script.state == SCRIPT_EMPTY ?
	       sc->builtin.strings : sc->script.temporary.strings,
	       string);
}

/*.......................................................................
 * Allocate a new script object (one that has a ScriptObj as its first
 * member). If a script is active then the object will be pushed onto
 * the stack of objects that are to be returned to their respective
 * free-lists when the script has completed.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  fl       FreeList *  The free-list to allocate the object from. This
 *                       should be one of the free-lists in sc->memory.
 *                       It can also be NULL, in which case the value
 *                       of the 'size' argument will be used to get
 *                       a suitable free-list.
 *  node_size  size_t    This is ignored unless fl==NULL. It specifies
 *                       the free-list node size needed for the object
 *                       type. Compute this by applying sizeof() to
 *                       the object type.
 * Output:
 *  return       void *  The requested object, or NULL on error.
 */
void *new_ScriptObject(Script *sc, FreeList *fl, size_t node_size)
{
  ScriptObj *obj;    /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc || (!fl && node_size<1)) {
    lprintf(stderr, "new_ScriptObject: NULL argument(s).\n");
    return NULL;
  };
/*
 * If no freelist was supplied, find a suitable one.
 */
  if(!fl) {
    fl = new_ScriptFreeList(sc, node_size);
    if(!fl)
      return NULL;
  };
/*
 * Allocate the object from the specified free-list.
 */
  obj = (ScriptObj* )new_FreeListNode("new_ScriptObject", fl);
  if(!obj)
    return NULL;
/*
 * Initialize the object.
 */
  obj->next = NULL;
  obj->fl = fl;
/*
 * If a script is currently active, push the object onto the stack of
 * temporary objects that are to be reclaimed when the script
 * completes.
 */
  if(sc->script.state != SCRIPT_EMPTY) {
    if(!sc->script.temporary.objects) {
      sc->script.temporary.objects = obj;
    } else {
      obj->next = sc->script.temporary.objects;
      sc->script.temporary.objects = obj;
    };
  };
  return obj;
}

/*.......................................................................
 * Lists that are allocated as part of a script object must be allocated
 * via new_ScriptList(), not directly through new_List(). If a script
 * is active, the new list will be added to the list of temporary lists
 * that are to be returned to the free-list when the current script
 * completes.
 *
 * Input:
 *  sc       Script *   The host scripting environment.
 */
List *new_ScriptList(Script *sc)
{
  List *list;    /* The object to be returned */
/*
 * Check arguments.
 */
  if(!sc) {
    lprintf(stderr, "new_ScriptList: NULL argument.\n");
    return NULL;
  };
/*
 * Attempt to allocate the list.
 */
  list = new_List(sc->memory.list);
  if(!list)
    return NULL;
/*
 * If needed, attempt to add the list to the list of temporary
 * lists that are to be returned to the freelist when the current
 * script completes.
 */
  if(sc->script.state != SCRIPT_EMPTY &&
     prepend_ListNode(sc->script.temporary.lists, list) == NULL)
    return del_List(list);
  return list;
}

/*.......................................................................
 * Return a freelist suitable for a given script object.
 *
 * Input:
 *  sc         Script *   The host scripting environment.
 *  node_size  size_t     The size of the objects to be allocated from
 *                        the freelist. Compute this by applying the
 *                        sizeof() scoperator to the datatype that is to
 *                        be stored in the free-list.
 * Output:
 *  return   FreeList *   A compatible free-list, or NULL on error.
 */
FreeList *new_ScriptFreeList(Script *sc, size_t node_size)
{
  ListNode *node;   /* A node of sc->memory.freelists */
  FreeList *fl;     /* The freelist to be returned */
  unsigned bfac;    /* the blocking factor of the freelist */
/*
 * Check arguments.
 */
  if(!sc) {
    lprintf(stderr, "new_ScriptFreeList: NULL argument.\n");
    return NULL;
  };
  if(node_size < 1) {
    lprintf(stderr, "new_ScriptFreeList: node_size < 1.\n");
    return NULL;
  };
/*
 * Search the list of previously allocated free-lists for one that is
 * compatible with the specified node size.
 */
  for(node=sc->memory.freelists->head; node; node=node->next) {
    if(compatible_FreeList((FreeList* )node->data, node_size))
      return (FreeList* )node->data;
  };
/*
 * None of the previously allocated freelists are compatible, so prepare
 * to allocate a new one.
 *
 * Start by computing the blocking factor that will result in a block
 * size of at least SC_BLOCK_SIZE.
 */
  bfac = (SC_BLOCK_SIZE + node_size - 1U) / node_size;
/*
 * Attempt to allocate the freelist.
 */
  fl = new_FreeList("new_ScriptFreeList", node_size, bfac);
  if(!fl)
    return NULL;
/*
 * Attempt to append the new freelist to the list of freelists.
 */
  if(append_ListNode(sc->memory.freelists, fl) == NULL) {
    fl = del_FreeList("new_ScriptFreeList", fl, 1);
    return NULL;
  };
/*
 * Return the newly allocated freelist.
 */
  return fl;
}

/*.......................................................................
 * Discard the current script, if there is one.
 *
 * Input:
 *  sc      Script *   The host scripting environment.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
int discard_script(Script *sc)
{
  if(!sc) {
    lprintf(stderr, "discard_script: NULL argument.\n");
    return 1;
  };
/*
 * Already discarded?
 */
  if(sc->script.state == SCRIPT_EMPTY)
    return 0;
/*
 * Cleanup script-specific project data.
 */
  if(sc->clr_fn && sc->clr_fn(sc, sc->data))
    return 1;
/*
 * Discard the script arguments (the list will be garbage collected below).
 */
  sc->script.args = NULL;
/*
 * Discard all temporary symbol scopes.
 */
  if(sc->script.scopes) {
    ListNode *node;
    while((node=sc->script.scopes->head))
      del_HashTable((HashTable *)del_ListNode(sc->script.scopes, node, NULL));
  };
/*
 * Clear the loop stack.
 */
  while(sc->script.comp_stack->head)
    pop_CompStatement(sc);
/*
 * Clear the list of catch clauses.
 */
  clr_List(sc->script.catch_list);
/*
 * Clear the call stack.
 */
  while(sc->script.exe_stack)
    pop_ExeFrame(sc);
  sc->script.stmts = NULL;
  sc->script.cleanup.stmts = NULL;
  sc->script.cleanup.reason = NULL;
/*
 * Return all temporary lists to the freelist.
 */
  if(sc->script.temporary.lists) {
    ListNode *head;
    while((head = sc->script.temporary.lists->head))
    del_List((List *)del_ListNode(sc->script.temporary.lists, head, NULL));
  };
/*
 * Return all temporary script objects to their respective freelists.
 */
  {
    ScriptObj *obj;   /* The object being deleted */
    ScriptObj *next;  /* The object that follows 'obj' */
    for(obj=sc->script.temporary.objects; obj; obj=next) {
      next = obj->next;
      obj = (ScriptObj* )del_FreeListNode("discard_script", obj->fl, obj);
    };
    sc->script.temporary.objects = NULL;
  };
/*
 * Return all temporary strings to the transient string pool.
 */
  clr_StringPool(sc->script.temporary.strings);
  sc->script.name = NULL;
/*
 * The script no longer exists.
 */
  sc->script.state = SCRIPT_EMPTY;
  return 0;
}

/*.......................................................................
 * Compile a script from a given input stream. Any existing script will
 * be discarded first.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  name           char *  An informational name to give the script.
 *                         This is can subsequently be queried via
 *                         script_name(sc).
 *  argstr  InputStream *  The stream to read arguments from (this
 *                         can be NULL if the script doesn't take
 *                         any arguments).
 *  stream  InputStream *  The stream to read the script from.
 *  restricted      int    True to only allow simple one-liners.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int compile_script(Script *sc, char *name, InputStream *argstr,
		   InputStream *stream, int restricted, bool interactive)
{
  StatementList *stmts;   /* The top-level statement list of the script */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "compile_script: NULL argument(s).\n");
    return 1;
  };
/*
 * Discard any existing script.
 */
  discard_script(sc);
/*
 * Mark the script environment as active.
 */
  sc->script.state = SCRIPT_ACTIVE;
/*
 * Create a symbol-scope for the top-level statements of the script.
 */
  if(push_Scope(sc))
    return bad_script(sc);
/*
 * Record the name of the script.
 */
  sc->script.name = new_ScriptString(sc, name ? name : (char* )"(unnnamed)");
  if(!sc->script.name)
    return bad_script(sc);
/*
 * See if the file starts with an argument declaration line.
 */
  sc->script.args = script_args(sc, stream, argstr);
  if(!sc->script.args)
    return bad_script(sc);
/*
 * Compile the script.
 */
  stmts = sc->script.stmts = restricted ?
    parse_RestrictedStatementList(sc, stream) :
    parse_StatementList(sc, stream, 0);
  if(!sc->script.stmts)
    return bad_script(sc);
/*
 * Discard the top-level symbol-scope.
 */
  if(pop_Scope(sc))
    return bad_script(sc);
/*
 * Schedule the first statement to be run.
 */
  if(stmts->head && push_ExeFrame(sc, (Statement *)stmts->head->data, stmts->head)==NULL)
    return bad_script(sc);

  sc->interactive_ = interactive;

  return 0;
}

/*.......................................................................
 * This function restarts a running script at its first statement.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int rewind_script(Script *sc)
{
  if(sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr, "rewind_script: There is no script to be restarted.\n");
    return 1;
  };
/*
 * Clear the call stack.
 */
  while(sc->script.exe_stack)
    pop_ExeFrame(sc);
/*
 * Schedule the first statement to be run.
 */
  {
    StatementList *stmts = sc->script.stmts;
    if(stmts->head && push_ExeFrame(sc, (Statement *)stmts->head->data, stmts->head)==NULL)
      return bad_script(sc);
  };
/*
 * Mark the script as ready to be run.
 */
  sc->script.state = SCRIPT_ACTIVE;
  return 0;
}

/*.......................................................................
 * This is the private error return function. It discards the errant
 * script and returns 1 (the error return code of its caller).
 *
 * Input:
 *  sc     Script *   The host scripting environment.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int bad_script(Script *sc)
{
  discard_script(sc);
  return 1;
}

/*.......................................................................
 * Return the informational name of a given script.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 * Output:
 *  return         char *  The name passed to compile_script().
 */
char *script_name(Script *sc)
{
/*
 * Check the arguments.
 */
  if(!sc) {
    lprintf(stderr, "script_name: NULL argument(s).\n");
    return "(null)";
  };
/*
 * If no script is active, there is no name to be returned.
 */
  if(!sc->script.name) {
    lprintf(stderr, "script_name: There is no script.\n");
    return "(null)";
  };
  return sc->script.name;
}

/*.......................................................................
 * Compile, run and discard an interactive command line.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  log    OutputStream *  If you want commands to be logged before they
 *                         are executed, specify an open logging stream
 *                         here. This will also be used by the "log"
 *                         command.
 *  command        char *  The command-line string.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int run_interactive_command(Script *sc, OutputStream *log, char *command)
{
  int waserr;   /* True after error */

/*
 * Wrap the command-line string in a stream.
 */
  if(open_StringInputStream(sc->input, 0, command))
    return 1;
/*
 * Compile the command.
 */
  waserr = compile_script(sc, "interactive", NULL, sc->input, 1, true) ||
           step_script(sc, log) ||
	   discard_script(sc);
/*
 * Clean up.
 */
  close_InputStream(sc->input);

  return waserr;
}

/*.......................................................................
 * Implement a command that elaborates on a given symbol.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  args VariableList *  The list of command-line arguments:
 *                        symbol - The symbol to elaborate on.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static CMD_FN(sc_whatis_cmd)
{
  Variable *vsym;                /* The variable that contains the symbol */
  SymbolVariable *sym;           /* The derived type of 'vsym' */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsym, NULL))
    return 1;
  sym = SYMBOL_VARIABLE(vsym);
/*
 * Different types of symbols require different types of explanation.
 */
  switch(sym->type) {
  case SYM_VARIABLE:
    return whatis_Variable(sc, sym->data.var);
    break;
  case SYM_FUNCTION:
    return whatis_Function(sc, sym->data.func);
    break;
  case SYM_COMMAND:
    return whatis_Command(sc, sym->data.cmd);
    break;
  case SYM_DATATYPE:
    return whatis_DataType(sc, sym->data.dt);
    break;
  case SYM_FUNCTION_KEYWORD:
    lputs("The function keyword is used to declare a new function.\n", stdout);
    lputs("  function TYPE NAME {ARGUMENT_DECLARATIONS} {\n", stdout);
    lputs("    EXPRESSION\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_COMMAND_KEYWORD:
    lputs("The command keyword is used to declare a new command.\n", stdout);
    lputs("  command NAME(ARGUMENT_DECLARATIONS) {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_LISTOF_KEYWORD:
    lputs("The listof keyword declares a variable to contain a list.\n",
	  stdout);
    lprintf(stdout, "  listof TYPE NAME\n");
    break;
  case SYM_GROUP_KEYWORD:
    lputs("The group keyword declares an aggregate datatype.\n", stdout);
    lputs("  group NAME {\n", stdout);
    lputs("    MEMBER_DECLARATIONS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_FOREACH_KEYWORD:
    lputs("Foreach statements repeat a loop for each value of a list.\n",
	  stdout);
    lputs("  foreach(TYPE LOOP_VARIABLE_NAME) LIST_OF_TYPE {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_DO_KEYWORD:
    lputs("Do statements loop for each of a grid of loop-variable values",
	  stdout);
    lputs("  do TYPE LOOP_VARIABLE_NAME = START, END, INCREMENT {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_UNTIL_KEYWORD:
    lputs(
       "The until command halts execution until an expression becomes true.\n",
       stdout);
    lputs("  until BOOLEAN_EXPRESSION\n", stdout);
    break;
  case SYM_WHILE_KEYWORD:
    lputs(
	"The while command repeats a loop while an expression remains true.\n",
        stdout);
    lputs("  while(BOOLEAN_EXPRESSION) {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_IF_KEYWORD:
    lputs("The 'if' keyword executes statements if an expression is true.\n",
	  stdout);
    lputs("  if(BOOLEAN_EXPRESSION) {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  } else if(BOOLEAN_EXPRESSION) {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  } else {\n", stdout);
    lputs("    STATEMENTS\n", stdout);
    lputs("  }\n", stdout);
    break;
  case SYM_PRINT_KEYWORD:
    lputs("The print statement displays the values of its arguments.\n",
	  stdout);
    lputs("  print ARGUMENTS\n", stdout);
    break;
  case SYM_LOG_KEYWORD:
    lputs("The log statement writes the values of its arguments to the log.\n",
	  stdout);
    lputs("  log ARGUMENTS\n", stdout);
    break;
  case SYM_BREAK_KEYWORD:
    lputs("The 'break' statement is used to prematurely break out of a loop.\n",
	  stdout);
    break;
  case SYM_NEXT_KEYWORD:
    lputs(
      "The 'next' statement prematurely starts the next iteration of a loop.\n",
      stdout);
    break;
  case SYM_EXIT_KEYWORD:
    lputs("The 'exit' statement terminates a script.\n", stdout);
    break;
  case SYM_IMPORT_KEYWORD:
    lputs("The 'import' statement imports an external script file.\n", stdout);
    lputs("  import FILE_NAME\n", stdout);
    break;
  case SYM_CLEANUP_KEYWORD:
    lputs("The 'cleanup' command specifies statements to be run when the script exits.\n", stdout);
    lputs("  cleanup {statements}\n", stdout);
    break;
  case SYM_RETURN_KEYWORD:
    lputs("Return early from a user-defined command.\n", stdout);
    break;
  case SYM_CATCH_KEYWORD:
    lputs("The 'catch' statement catches specified signals.\n", stdout);
    lputs("  catch signals {statements} {remedial_statements}", stdout);
    break;
  default:
    lprintf(stderr, "whatis: Unknown symbol type.\n");
    return 1;
    break;
  };
  return 0;
}

/*.......................................................................
 * Display details about a given variable to stdout.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  var      Variable *  The variable to describe.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int whatis_Variable(Script *sc, Variable *var)
{
  lprintf(stdout, "\"%s%s %s\" is a variable.\n",
	  var->type->is_list ? "listof ":"",
	  var->type->dt->name, var->type->name);
  return 0;
}

/*.......................................................................
 * Display details about a given function to stdout.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  func     Function *  The user of builtin function to describe.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int whatis_Function(Script *sc, Function *func)
{
  ListNode *arg;   /* An argument-list node */
  int width;       /* The text width of the current output line */
  int arg_col;     /* The text column in which the arguments start */
  int nc;          /* The number of characters written by lprintf() */
/*
 * Set the text width at which a new line will be started.
 */
  const int bound = 60;
/*
 * Display the declaration of the function.
 */
  if((width = lprintf(stdout, "function %s %s%s", func->return_type->dt->name,
	       func->return_type->name, func->args->head ? "(" : "()")) < 0)
    return 1;
  arg_col = width;
/*
 * List the argument declarations.
 */
  for(arg=func->args->head; arg; arg=arg->next) {
    Variable *v = (Variable* )arg->data;
/*
 * If the width of the current description line exceeds 'bound', start
 * a new line before printing the next argument. Pad the line to the
 * arg_col'th column with spaces.
 */
    if(width > bound) {
      width = 0;
      if((width = lprintf(stdout, "\n%*s", arg_col, "")) < 0)
	return 1;
    };
/*
 * Print the next argument.
 */
    nc = lprintf(stdout, "%s%s %s%s", v->type->is_list ? "listof ":"",
		 v->type->dt->name, v->type->name, arg->next ? ", ":")\n");
    if(nc < 0)
      return 1;
    width += nc;
  };
  return 0;
}

/*.......................................................................
 * Display details about a given command to stdout.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  cmd       Command *  The user of builtin command to describe.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int whatis_Command(Script *sc, Command *cmd)
{
  ListNode *arg;   /* An argument-list node */
  int width;       /* The text width of the current output line */
  int arg_col;     /* The text column in which the arguments start */
  int nc;          /* The number of characters written by lprintf() */
/*
 * Set the text width at which a new line will be started.
 */
  const int bound = 60;
/*
 * Display the declaration of the command.
 */
  if((width = lprintf(stdout, "command %s%s", cmd->name,
		      cmd->args->head ? "(" : "()")) < 0)
    return 1;
  arg_col = width;
/*
 * List the argument declarations.
 */
  for(arg=cmd->args->head; arg; arg=arg->next) {
    Variable *v = (Variable* )arg->data;
/*
 * If the width of the current description line exceeds 'bound', start
 * a new line before printing the next argument. Pad the line to the
 * arg_col'th column with spaces.
 */
    if(width > bound) {
      width = 0;
      if((width = lprintf(stdout, "\n%*s", arg_col, "")) < 0)
	return 1;
    };
/*
 * Print the next argument.
 */
    nc = lprintf(stdout, "%s%s %s%s", v->type->is_list ? "listof ":"",
		 v->type->dt->name, v->type->name, arg->next ? ", ":")\n");
    if(nc < 0)
      return 1;
    width += nc;
  };
  return 0;
}

/*.......................................................................
 * Display details about a given datatype to stdout.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  dt       DataType *  The user of builtin datatype to describe.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int whatis_DataType(Script *sc, DataType *dt)
{
  ListNode *field; /* A group field-list node */
  GroupType *group;/* The details of a user-defined group datatype */
  int width;       /* The text width of the current output line */
  int field_col;   /* The text column in which the group fields start */
  int nc;          /* The number of characters written by lprintf() */
/*
 * Set the text width at which a new line will be started.
 */
  const int bound = 60;
/*
 * There isn't much to say about builtin datatypes.
 */
  if(dt->dataclass == DT_BUILTIN) {
    lprintf(stdout, "%s is a builtin data-type.\n", dt->name);
    return 0;
  };
/*
 * User defined datatypes are agglomerations of builtin datatypes.
 * Get the details of the current agglomeration.
 */
  group = (GroupType* )dt->context;
/*
 * Display the declaration of the user datatype.
 */
  if((width = lprintf(stdout, "group %s %s", dt->name,
		      group->fields->head ? "{":"{}")) < 0)
    return 1;
  field_col = width;
/*
 * List the field declarations.
 */
  for(field=group->fields->head; field; field=field->next) {
    TypeSpec *t = (TypeSpec* )field->data;
/*
 * If the width of the current description line exceeds 'bound', start
 * a new line before printing the next field. Pad the line to the
 * field_col'th column with spaces.
 */
    if(width > bound) {
      width = 0;
      if((width = lprintf(stdout, "\n%*s", field_col, "")) < 0)
	return 1;
    };
/*
 * Print the next field.
 */
    nc = lprintf(stdout, "%s%s %s%s", t->is_list ? "listof ":"",
		 t->dt->name, t->name, field->next ? ", ":"}\n");
    if(nc < 0)
      return 1;
    width += nc;
  };
  return 0;
}

/*.......................................................................
 * Return the builtin Boolean datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The boolean datatype, or NULL if sc==NULL.
 */
DataType *sc_Boolean_dt(Script *sc)
{
  return sc ? sc->builtin.boolvar_dt : NULL;
}

/*.......................................................................
 * Return the builtin String datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The string datatype, or NULL if sc==NULL.
 */
DataType *sc_String_dt(Script *sc)
{
  return sc ? sc->builtin.string_dt : NULL;
}

/*.......................................................................
 * Return the builtin Symbol datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The symbol datatype, or NULL if sc==NULL.
 */
DataType *sc_Symbol_dt(Script *sc)
{
  return sc ? sc->builtin.symbol_dt : NULL;
}

/*.......................................................................
 * Return the builtin Double datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The double datatype, or NULL if sc==NULL.
 */
DataType *sc_Double_dt(Script *sc)
{
  return sc ? sc->builtin.double_dt : NULL;
}

/*.......................................................................
 * Return the builtin Integer datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The integer datatype, or NULL if sc==NULL.
 */
DataType *sc_Integer_dt(Script *sc)
{
  return sc ? sc->builtin.integer_dt : NULL;
}

/*.......................................................................
 * Return the builtin Sexagesimal datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The sexagesimal datatype, or NULL if sc==NULL.
 */
DataType *sc_Sexagesimal_dt(Script *sc)
{
  return sc ? sc->builtin.sexagesimal_dt : NULL;
}

/*.......................................................................
 * Return the builtin InputFile datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The InputFile datatype, or NULL if sc==NULL.
 */
DataType *sc_InputFile_dt(Script *sc)
{
  return sc ? sc->builtin.input_file_dt : NULL;
}

/*.......................................................................
 * Return the builtin Signal datatype.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   DataType *  The Signal datatype, or NULL if sc==NULL.
 */
DataType *sc_Signal_dt(Script *sc)
{
  return sc ? sc->builtin.signal_dt : NULL;
}

/*.......................................................................
 * Compile script arguments.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The stream containing the script.
 *  argstr  InputStream *  The stream containing the arguments.
 * Output:
 *  return VariableList *  The list of script argument variables.
 */
static VariableList *script_args(Script *sc, InputStream *stream,
				 InputStream *argstr)
{
  VariableList *args;  /* The list of output variables */
  ListNode *node;      /* A node in the declared list of variables */
  Expr *expr;          /* The compiled argument assignment expressions */
  int first=1;         /* True until one argument has been parsed */
  int narg=0;          /* The number of parsed arguments */
/*
 * Create a list for the declared variables.
 */
  args = new_VariableList(sc);
  if(!args)
    return NULL;
/*
 * If the script takes any arguments it should start with an open parenthesis.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc == '(') {
/*
 * Skip the open parenthesis.
 */
    if(input_skip_white(stream, 1, 1))
      return NULL;
/*
 * Parse the argument declarations.
 */
    do {
      TypeSpec *type;   /* The declaration of an argument. */
      Variable *var;    /* The new argument variable */
/*
 * Locate the next argument declaration, skipping commas between arguments.
 */
      if(first ? input_skip_space(stream, 1, 0) :
	         input_skip_white(stream, 1, 1))
	return NULL;
      first = 0;
/*
 * Parse the declaration of the next field.
 */
      type = parse_TypeSpec(sc, stream, NULL);
      if(!type)
	return NULL;
/*
 * Use the declaration to create a variable, and
 * append it to the list of variables created so far.
 */
      var = new_Variable(sc, type);
      if(!var || !append_Variable(sc, args, var))
	return NULL;
/*
 * Mark the variables as constants so that they can be used in
 * compile-time statements such as the import command.
 */
      var->flags |= VAR_IS_CONST;
/*
 * Add the variable to the local scope.
 */
      if(!add_ScriptSymbol(sc, type->name, SYM_VARIABLE, var))
	return NULL;
/*
 * If there are any further declarations then the next token should
 * be a comma.
 */
      if(input_skip_space(stream, 1, 0))
	return NULL;
    } while(stream->nextc == ',');
/*
 * The argument list should end with a close parenthesis.
 */
    if(stream->nextc != ')') {
      input_error(stream, 1,
		  "Missing ')' at end of argument declaration list.\n");
      return NULL;
    };
    if(input_skip_space(stream, 1, 1))
      return NULL;
/*
 * There shouldn't be anything else on the line.
 */
    if(stream->nextc != '\n' && stream->nextc != EOF) {
      input_error(stream, 1,
		  "Extra characters follow a valid declaration list.\n");
      return NULL;
    };
    if(input_skip_white(stream, 1, 1))
      return NULL;
  };
/*
 * If the script expects arguments, then it is illegal for argstr to
 * be NULL.
 */
  if(!argstr) {
    if(args->head) {
      lprintf(stderr, "No arguments have been provided for script: %s\n",
	      sc->script.name);
      return NULL;
    };
    return args;
  };
/*
 * Find the start of the first argument expression, if any.
 */
  if(input_skip_space(argstr, 1, 0))
    return NULL;
/*
 * Should there be any arguments?
 */
  if(args->head) {
/*
 * Create a container for compiling argument expressions.
 */
    expr = new_Expr(sc);
    if(!expr)
      return NULL;
    if(argstr->nextc != '(') {
      input_error(argstr, 1, "Missing '(' before script arguments.\n");
      return NULL;
    };
    if(input_skip_white(argstr, 1, 1))
      return NULL;
/*
 * Parse the argument expressions.
 */
    for(node=args->head; node; node=node->next) {
      Variable *var = (Variable* )node->data;
/*
 * Parse the argument expression and arrange for its value to be assigned
 * to the script argument variable.
 */
      if(parse_argument(sc, var->type, argstr, expr) ||
	 input_skip_space(argstr, 1, 0) ||
	 add_StoreOper(sc, expr, var)==NULL)
	return NULL;
/*
 * Keep a tally of the number of arguments parsed so far.
 */
      narg++;
/*
 * Skip the inter-argument comma unless we have reached the end
 * of the mandatory arguments.
 */
      if(node->next) {
	if(argstr->nextc != ',') {
	  if(argstr->nextc == ')' || argstr->nextc=='\n') {
	    input_error(argstr, 1,
		"Too few arguments have been presented to script: %s\n",
 	       script_name(sc));
	  } else {
	    input_error(argstr, 1, "Argument %d of script '%s' is garbled.\n",
			narg, script_name(sc));
	  };
	  return NULL;
	};
	if(input_skip_white(argstr, 1, 1))
	  return NULL;
      };
    };
/*
 * The arguments should be followed by a close parenthesis.
 */
    if(input_skip_space(argstr, 1, 0))
      return NULL;
    if(argstr->nextc != ')') {
      if(argstr->nextc == ',') {
	input_error(argstr, 1,
		    "Too many arguments have been presented to script: %s\n",
		    sc->script.name);
      } else {
	input_error(argstr, 1, "Missing ')' at end of script argument list.\n");
      };
      return NULL;
    };
    if(input_skip_space(argstr, 1, 1))
      return NULL;
/*
 * Evaluate the expression to assign values to the arguments.
 */
    if(exe_Expr(sc, expr))
      return NULL;
/*
 * No argument declarations were provided, so the empty open and close
 * parentheses are optional and there shouldn't be any argument expressions.
 */
  } else {
    if(stream->nextc=='(') {
      if(input_skip_space(argstr, 1, 1))
	return NULL;
      if(stream->nextc!=')') {
	input_error(argstr, 1, "Script %s takes no arguments.\n",
		    sc->script.name);
	return NULL;
      };
      if(input_skip_space(argstr, 1, 1))
	return NULL;
    };
  };
  return args;
}

/*.......................................................................
 * Check whether the value of an InputFile datatype specifies a readable
 * file.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  var        Variable *  The variable who's value is to be checked.
 *  stream  InputStream *  The stream from which the file name was read.
 * Output:
 *  return       int    0 - Value ok.
 *                      1 - Not a valid file name.
 */
static DT_CHECK(sc_check_input_file)
{
  char *path;         /* The path name in var */
/*
 * Get the pathname to be checked.
 */
  path = STRING_VARIABLE(var)->string;
/*
 * Check that the pathname refers to a regular readable file.
 */
  if(test_pathname(path, PATH_IS_REG, PATH_READ) != NULL) {
    input_error(stream, 1, "Error: '%s' is not a readable file.\n", path);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Write the name and arguments of the current script to a given output
 * stream. If there are any arguments they are postfixed to the script
 * name enclosed in ().
 *
 * Input:
 *  out  OutputStream *  The stream to write to.
 *  sc         Script *  The script to name.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int output_script_spec(OutputStream *out, Script *sc)
{
/*
 * Check the arguments.
 */
  if(!out || !sc) {
    lprintf(stderr, "output_script_spec: NULL argument(s).\n");
    return 1;
  };
  if(!sc->script.name) {
    lprintf(stderr, "There is no compiled script to be named.\n");
    return 1;
  };
/*
 * Start by writing the script name.
 */
  if(write_OutputStream(out, sc->script.name))
    return 1;
/*
 * Are there any arguments?
 */
  if(sc->script.args->head) {
    if(write_OutputStream(out, "(") ||
       print_ArgumentList(sc, out, 0, sc->script.args) ||
       write_OutputStream(out, " )"))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Add the name of a signal to the list of known signals. It is considered
 * an error to do this when there is no script active.
 *
 * Input:
 *  sc       Script *  The script to add the signal to.
 *  name       char *  The signal name to be added. It isn't an error
 *                     to specify the same name twice.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
int add_script_signal(Script *sc, char *name)
{
/*
 * Check the arguments.
 */
  if(!sc || !name) {
    lprintf(stderr, "add_script_signal: NULL argument(s).\n");
    return 1;
  };
/*
 * Complain if no script is currently active.
 */
  if(sc->script.state != SCRIPT_ACTIVE) {
    lprintf(stderr, "add_script_signal: No script is active.\n");
    return 1;
  };
/*
 * If the symbol has already been added, do nothing.
 */
  if(find_HashSymbol(sc->script.signals, name) != NULL)
    return 0;
/*
 * Add the new symbol.
 */
  if(new_HashSymbol(sc->script.signals, name, 0, 0, NULL, 0) == NULL)
    return 1;
  return 0;
}

/*.......................................................................
 * Return the hash-table entry of a specified signal.
 *
 * Input:
 *  sc       Script *  The script to add the signal to.
 *  name       char *  The signal name to be added. It isn't an error
 *                     to specify the same name twice.
 * Output:
 *  return   Symbol *  The symbol table entry of the signal, or NULL
 *                     if not found.
 */
Symbol *lookup_script_signal(Script *sc, char *name)
{
/*
 * Check the arguments.
 */
  if(!sc || !name) {
    lprintf(stderr, "lookup_script_signal(%s): NULL argument(s).\n",
	    name ? name : "(NULL)");
    return NULL;
  };
/*
 * There are no signals if no script is currently active.
 */
  if(sc->script.state != SCRIPT_ACTIVE)
    return NULL;
/*
 * Lookup the specified signal.
 */
  return find_HashSymbol(sc->script.signals, name);
}

/*.......................................................................
 * Decrement the count of the number of times that a given signal has
 * been received but not acknowledged.
 *
 * Input:
 *  sig      Symbol *  The symbol table entry of the signal, as returned
 *                     by lookup_script_signal().
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
int clear_script_signal(Symbol *sig)
{
  if(!sig) {
    lprintf(stderr, "clear_script_signal: NULL argument(s).\n");
    return 1;
  };
/*
 * Decrement its count.
 */
  if(sig->code > 0)
    sig->code--;
  return 0;
}

/*.......................................................................
 * Clear the count of the number of times that a given signal has been
 * received but not acknowledged.
 *
 * Input:
 *  sig      Symbol *  The symbol table entry of the signal, as returned
 *                     by lookup_script_signal().
 * Output:
 *  return      int    0 - OK.
 *                     1 - Non-existent signal.
 */
int reset_script_signal(Symbol *sig)
{
  if(!sig) {
    lprintf(stderr, "reset_script_signal: NULL argument(s).\n");
    return 1;
  };
/*
 * Reset its count to zero.
 */
  sig->code = 0;
  return 0;
}

/*.......................................................................
 * Implement a function that returns true if a specified signal has been
 * received.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  args   VariableList *  The list of command-line arguments:
 *                          sig - The signal.
 * Input/Output:
 *  result     Variable *  The return value.
 *  state      Variable *  Unused.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static FUNC_FN(sc_signaled_fn)
{
  Variable *vsig;    /* The signal to check */
/*
 * Get the command-line arguments.
 */
  if(get_Arguments(args, &vsig, NULL))
    return 1;
/*
 * Record the state of the signal in the return variable.
 */
  BOOL_VARIABLE(result)->boolvar = SIGNAL_VARIABLE(vsig)->sym->code > 0;
  return 0;
}

/*.......................................................................
 * Return the current status of a given script.
 *
 * Input:
 *  sc          Script *  The script of interest.
 * Output:
 *  return ScriptState    The state of the script.
 */
ScriptState script_state(Script *sc)
{
  return sc ? sc->script.state : SCRIPT_EMPTY;
}
