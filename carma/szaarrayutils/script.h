#ifndef script_h
#define script_h

#include <stdarg.h>

#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/list.h"
#include "carma/szaarrayutils/hash.h"
#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"

#include "carma/szautil/String.h"

#include <vector>
#include <list>

/*
 * Specify the number of buckets per hash table.
 * This should be a prime number.
 */
#define SC_HASH_SIZE 31

/*
 * Create a type-alias for the primary object of this interface.
 */
typedef struct Script Script;        /* see below for definition */

/*-----------------------------------------------------------------------
 * The following functions are required for creating and deleting
 * a script environment, and compiling and running a script.
 */

/*
 * Where project-specific datatypes and/or variables allocate non-script
 * resources that need to be released when the script is discarded and/or
 * deleted the caller should provide a constructor and destructor function
 * to create and destroy this data. Any of the data which is associated
 * with a particular compilation run (eg. with local variables) should
 * be discarded by clr_fn() when it is called by discard_script().
 */
#define SC_NEW_FN(fn) void *(fn)(Script *sc)
#define SC_CLR_FN(fn) int (fn)(Script *sc, void *data)
#define SC_DEL_FN(fn) void *(fn)(Script *sc, void *data)

/*
 * The following are the script environment constructor and
 * destructor functions.
 */
Script *new_Script(void *project, SC_NEW_FN(*new_fn), SC_CLR_FN(*clr_fn),
		   SC_DEL_FN(*del_fn), HashTable *signals);
Script *del_Script(Script *sc);

/*
 * The following functions can be used to compile a new script
 * or prematurely discard an old one. Note that compile_script()
 * calls discard_script().
 */
int compile_script(Script *sc, char *name, InputStream *argstr,
		   InputStream *stream, int restricted, bool interactive);

int rewind_script(Script *sc);
int discard_script(Script *sc);
char *script_name(Script *sc);
int output_script_spec(OutputStream *out, Script *sc);

/*
 * Compile, run and discard an interactive command.
 */
int run_interactive_command(Script *sc, OutputStream *log, char *command);

/*
 * Test wether a script is currently polling in an until statement.
 */
int script_is_polling(Script *sc);

/* Scripts are executed one leaf-statement at a time.  After a
 * successful call to compile_script(), one leaf-statement will be
 * executed each time that you call step_script(). This function will
 * return SCRIPT_ACTIVE if no error occured and there are further
 * statements to be run, SCRIPT_ERROR if an error occured, or
 * SCRIPT_END when the last statement has been executed. After an
 * error, or on reaching the end of the script, you should call either
 * discard_script() to allow a new script to be compiled, or
 * rewind_script() to stage the same script to be run again. A running
 * script can also be restarted at any time by calling rewind_script()
 * before the next call to step_script().
 */
typedef enum {
  SCRIPT_EMPTY,  /* No script has been compiled yet */
  SCRIPT_ACTIVE, /* The next statement of the script can be run */
  SCRIPT_ERROR,  /* An error occured */
  SCRIPT_EXITING,/* An exit handler is being executed */
  SCRIPT_END     /* There are no more statements to be executed */
} ScriptState;

ScriptState step_script(Script *sc, OutputStream *log);
ScriptState exit_script(Script *sc, char *reason);
ScriptState script_state(Script *sc);

/*-----------------------------------------------------------------------
 * The following section describes the contents of a script environment.
 *
 * Create type aliases for free-lists of script objects and
 * provide constructor wrappers around new_FreeList() to request
 * objects of the required sizes.
 */
typedef FreeList CommandMem;
CommandMem *new_CommandMem(Script *sc);

typedef FreeList FunctionMem;
FunctionMem *new_FunctionMem(Script *sc);

typedef FreeList ScOperatorMem;
ScOperatorMem *new_ScOperatorMem(Script *sc);

typedef FreeList DataTypeMem;
DataTypeMem *new_DataTypeMem(Script *sc);

typedef FreeList StatementMem;
StatementMem *new_StatementMem(Script *sc);

typedef FreeList ExprMem;
ExprMem *new_ExprMem(Script *sc);

typedef FreeList ExprOperMem;
ExprOperMem *new_ExprOperMem(Script *sc);

typedef FreeList LoopStateMem;
LoopStateMem *new_LoopStateMem(Script *sc);

typedef FreeList TypeSpecMem;
TypeSpecMem *new_TypeSpecMem(Script *sc);

typedef FreeList ExeFrameMem;
ExeFrameMem *new_ExeFrameMem(Script *sc);

typedef FreeList ListVariableMem;
ListVariableMem *new_ListVariableMem(Script *sc);

/*
 * Create type aliases for lists of script-objects.
 */
typedef List VariableList;
typedef List TypeSpecList;
typedef List StatementList;
typedef List ScopeStack;
typedef List CompStack;
typedef List ExprOperList;
typedef List LoopStateList;
typedef List ModifierList;

/*
 * Create type aliases for script objects.
 */
typedef struct Command Command;      /* see below for definition */
typedef struct Function Function;    /* see below for definition */
typedef struct ScOperator ScOperator;    /* see below for definition */
typedef struct DataType DataType;    /* see below for definition */
typedef struct Variable Variable;    /* see below for definition */
typedef struct Statement Statement;  /* see statement.c for definition */
typedef struct Expr Expr;            /* see expr.c for definition */
typedef struct ExprOper ExprOper;    /* see expr.c for definition */
typedef struct LoopState LoopState;  /* see statement.c for definition */
typedef struct TypeSpec TypeSpec;    /* see below for definition */
typedef struct ScriptObj ScriptObj;  /* see below for definition */
typedef struct ExeFrame ExeFrame;    /* See below for definition */
typedef struct StringPool StringPool;/* See stringpool.c for implementation */

/*
 * All of the objects that are both allocated from free-lists and
 * defined in this file, have a member of the following type as their
 * first member. A new object of any of the provided types must be
 * allocated via new_ScriptObject(), which will use the specified
 * freelist to allocate the object and also record it in the header
 * of the object. If the object is allocated while a script is active
 * it will be pushed onto a stack of temporary objects that are to
 * be returned to their respective free-lists after the active
 * script is completed. Otherwise the object is treated as a permanent
 * builtin and thus will not be reclaimed until its freelist is deleted.
 */
struct ScriptObj {
  ScriptObj *next;    /* The next in a list of temporary objects */
  FreeList *fl;       /* The free-list from which the object was allocated */
};

void *new_ScriptObject(Script *sc, FreeList *fl, size_t node_size);

/*
 * To acquire a freelist for a Script object, call this function
 * in place of new_FreeList(). If a compatible freelist has already been
 * allocated via a previous call to this function, then that freelist
 * is returned. Otherwise it creates a new freelist and adds it to the
 * list of allocated freelists in Script::memory.freelists.
 */
FreeList *new_ScriptFreeList(Script *sc, size_t node_size);

/*
 * Lists that are allocated as part of a script object must be allocated
 * via new_ScriptList(), not directly through new_List(). If a script
 * is active, the new list will be added to the list of temporary lists
 * that are to be returned to the free-list when the current script
 * completes.
 */
List *new_ScriptList(Script *sc);

enum DataTypeId {
  DT_UNK,
  DT_LIST,
  DT_UINT,
  DT_SET,
  DT_CHOICE,
  DT_BOOL,
  DT_INT,
  DT_DOUBLE,
  DT_SCRIPT,
  DT_SEXAGESIMAL,
  DT_STRING,
  DT_SYMBOL,
  DT_WILDCARD
};

struct CmdArg {
  std::string dataTypeName_;
  std::string varName_;
  bool isOptional_;
  
  CmdArg() {}
  
  CmdArg(std::string dataTypeName, std::string varName, bool isOptional) {
    dataTypeName_ = dataTypeName;
    varName_      = varName;
    isOptional_   = isOptional;
  }
  
  CmdArg(CmdArg& arg) {
    *this = arg;
  }
  
  CmdArg(const CmdArg& arg) {
    *this = arg;
  }
  
  void operator=(const CmdArg& arg) {
    *this = (CmdArg&)arg;
  }
  
  void operator=(CmdArg& arg) {
    dataTypeName_  = arg.dataTypeName_;
    varName_       = arg.varName_;
    isOptional_    = arg.isOptional_;
  }
};

struct ScriptDataType {
  sza::util::String name_;
  DataTypeId id_;
  void* context_;
  
  ScriptDataType() {}
  
  ScriptDataType(std::string name, DataTypeId id, void* context=0) {
    name_    = name;
    id_      = id;
    context_ = context;
  }
  
  ScriptDataType(const ScriptDataType& dataType) {
    name_    = dataType.name_;
    id_      = dataType.id_;
    context_ = dataType.context_;
  }
  
  bool operator<(ScriptDataType& cmd) {
    return name_ < cmd.name_;
  }
  
  bool operator==(ScriptDataType& dataType) {
    return name_ == dataType.name_;
  }
};

enum {
  SCR_COMMAND,
  SCR_FUNCTION,
  SCR_SYMBOL
};

struct ScriptCmd {
  std::string declaration_;
  std::string description_;
  sza::util::String name_;
  std::list<CmdArg> argList_;
  CmdArg retType_;
  unsigned type_;
  
  ScriptCmd() {}
  
  ScriptCmd(std::string name) {
    name_ = name;
  }
  
  ScriptCmd(const ScriptCmd& cmd) {
    name_        = cmd.name_;
    declaration_ = cmd.declaration_;
    description_ = cmd.description_;
    type_        = cmd.type_;
    retType_     = cmd.retType_;
    
    argList_.clear();
    for(std::list<CmdArg>::const_iterator iArg=cmd.argList_.begin(); iArg != cmd.argList_.end(); iArg++)
      argList_.insert(argList_.end(), *iArg);
  }
  
  bool operator<(ScriptCmd& cmd) {
    return name_ < cmd.name_;
  }
};

struct Script {
  void *project;              /* Optional project-specific environment data */
  void *data;                 /* Script-specific project data */
  SC_CLR_FN(*clr_fn);         /* The discard function for 'data' if needed */
  SC_DEL_FN(*del_fn);         /* The destructor for 'data' if needed */
  InputStream *input;         /* An unopened generic input-stream */
  OutputStream *output;       /* A stream wrapper around lprintf(stdout) */
  char *host;                 /* The name of the host computer */
  /*
   * All script objects except strings are allocated from the following freelists.
   */
  struct {
    HashMemory *hashtable;    /* Memory for HashTable objects */
    ListMemory *list;         /* Memory for allocating lists */
    CommandMem *command;      /* Memory for Command objects */
    FunctionMem *function;    /* Memory for Function objects */
    ScOperatorMem *scoperator;    /* Memory for ScOperator objects */
    DataTypeMem *datatype;    /* Memory for Datatype objects */
    StatementMem *statement;  /* Memory for Statement objects */
    LoopStateMem *loopstate;  /* Memory for LoopState objects */
    ExprMem *expr;            /* Memory for Expr objects */
    ExprOperMem *exproper;    /* Memory for ExprOper objects */
    TypeSpecMem *typespec;    /* Memory for TypeSpec objects */
    ExeFrameMem *exeframe;    /* Memory for ExeFrame objects */
    ListVariableMem *list_var;/* Memory for list variables */
    List *freelists;          /* A list of freelists of different sizes */
  } memory;
  /*
   * Builtin symbols are kept in a separate scope from compiled programs.
   * Similarly, to allow fast garbage collection of strings used by
   * temporary scripts strings for builtin objects are allocated
   * from a separate pool of strings than strings for ephemeral script
   * objects.
   */
  struct {
    HashTable *symbols;      /* Symbol table of builtin objects */
    StringPool *strings;     /* A pool for allocating builtin strings */
    DataType *wildcard_dt;   /* An alias for a generic datatype object */
    DataType *boolvar_dt;    /* An alias of the Boolean datatype object */
    DataType *string_dt;     /* An alias of the String datatype object */
    DataType *symbol_dt;     /* An alias of the Symbol datatype object */
    DataType *double_dt;     /* A generic floating point datatype */
    DataType *integer_dt;    /* A generic integral datatype */
    DataType *sexagesimal_dt;/* A generic floating point datatype, input */
    /*  and output in sexagesimal notation. */
    DataType *input_file_dt; /* A generic readable file datatype */
    DataType *signal_dt;     /* The Signal datatype */
  } builtin;
  /*
   * The following container contains transitory information associated
   * with the current user script.
   */
  struct {
    char *name;             /* The name of the script */
    VariableList *args;     /* The argument list of the script */
    struct {
      StatementList *stmts; /* Statements to be run on exit */
      Variable *reason;     /* The argument of the cleanup handler */
    } cleanup;
    ScriptState state;      /* The executable status of the script */
    ScopeStack *scopes;     /* A compile-time stack of local symbol scopes */
    CompStack *comp_stack;  /* A compile-time stack of nested statements */
    ExeFrame *exe_stack;    /* The stack of currently executing statements */
    StatementList *stmts;   /* The top-level statements of the program */
    HashTable *signals;     /* A symbol table of currently known signals */
    List *catch_list;       /* The list of running catch statements */
    /*
     * The following structure contains lists of all temporary objects that
     * are allocated to the current script. When the script is complete
     * the listed objects will be returned to their respective freelists.
     */
    struct {
      List *lists;             /* The list of lists to be deleted */
      ScriptObj *objects;      /* The list of script objects to be deleted */
      StringPool *strings;     /* The pool of temporary strings */
    } temporary;

  } script;
  
  std::list<ScriptCmd>* commands_;
  std::list<ScriptCmd>* functions_;
  std::list<ScriptCmd>* symbols_;

  std::list<ScriptDataType>* dataTypes_;
  bool interactive_;         // True if this is an interactive script
  
  void insert(const ScriptDataType& dataType) {
    if(!exists(dataType.name_))
      dataTypes_->insert(dataTypes_->end(), dataType);
  }
  
  bool exists(const sza::util::String& name) {
    for(std::list<ScriptDataType>::iterator iDat=dataTypes_->begin();
	iDat != dataTypes_->end(); iDat++)
      if((*iDat).name_ == name)
	return true;
    
    return false;
  }
  
  // Insert an argument into the passed command.  If the argument data                                        
  // type doesn't already exist, then create an entry for it in our                                           
  // list of data types                                                                                       
  
  void insert(ScriptCmd* cmd, const CmdArg& cmdArg) {
    if(!exists(cmdArg.dataTypeName_))
      dataTypes_->insert(dataTypes_->end(),
			 ScriptDataType(cmdArg.dataTypeName_, DT_UNK));
    
    cmd->argList_.insert(cmd->argList_.end(), cmdArg);
  }
  
};

/*
 * Provide accessor functions for builtin types.
 */
DataType *sc_Boolean_dt(Script *sc);
DataType *sc_String_dt(Script *sc);
DataType *sc_Symbol_dt(Script *sc);
DataType *sc_Double_dt(Script *sc);
DataType *sc_Integer_dt(Script *sc);
DataType *sc_Sexagesimal_dt(Script *sc);
DataType *sc_InputFile_dt(Script *sc);
DataType *sc_Signal_dt(Script *sc);

/*-----------------------------------------------------------------------
 * The following section is concerned with compilation of statements
 * and expressions.
 */

/*
 * The different types of symbols in a given symbol table are
 * distinguished by the following enumerators.
 */
typedef enum {
  SYM_VARIABLE,            /* Symbol::data contains a pointer to a Variable */
  SYM_FUNCTION,            /* Symbol::data contains a pointer to a Function */
  SYM_COMMAND,             /* Symbol::data contains a pointer to a Command */
  SYM_DATATYPE,            /* Symbol::data contains a pointer to a DataType */
  SYM_FUNCTION_KEYWORD,    /* The "function" reserved word */
  SYM_COMMAND_KEYWORD,     /* The "command" reserved word */
  SYM_LISTOF_KEYWORD,      /* The "listof" reserved word */
  SYM_GROUP_KEYWORD,       /* The "group" reserved word */
  SYM_FOREACH_KEYWORD,     /* The "foreach" reserved word */
  SYM_DO_KEYWORD,          /* The "do" reserved word */
  SYM_UNTIL_KEYWORD,       /* The "until" reserved word */
  SYM_WHILE_KEYWORD,       /* The "while" reserved word */
  SYM_IF_KEYWORD,          /* The "if" reserved word */
  SYM_IFHOST_KEYWORD,      /* The "ifhost" command */
  SYM_PRINT_KEYWORD,       /* The "print" reserved word */
  SYM_LOG_KEYWORD,         /* The "log" reserved word */
  SYM_BREAK_KEYWORD,       /* The "break" command */
  SYM_NEXT_KEYWORD,        /* The "next" command */
  SYM_EXIT_KEYWORD,        /* The "exit" command */
  SYM_IMPORT_KEYWORD,      /* The "import" command */
  SYM_CLEANUP_KEYWORD,     /* The "cleanup" command */
  SYM_RETURN_KEYWORD,      /* The "return" command */
  SYM_CATCH_KEYWORD        /* The "catch" command */
} SymbolType;

/*
 * Parse a brace-enclosed or non-brace-enclosed list of statements.
 */
StatementList *parse_StatementList(Script *sc, InputStream *stream, int braces);
StatementList *parse_RestrictedStatementList(Script *sc, InputStream *stream);

/*
 * Parse a statement from an input stream. In interactive mode restrict
 * to simple builtin statements.
 */
Statement *parse_Statement(Script *sc, InputStream *stream, int interactive);

/*
 * The following type is used to record the declaration of a given
 * variable.
 */
struct TypeSpec {
  ScriptObj header;     /* The generic script-object header */
  char *name;           /* The name of the variable or function */
  DataType *dt;         /* The type-declaration of the variable */
  int is_list;          /* True if the variable is a list of type 'dt' */
};

TypeSpec *new_TypeSpec(Script *sc, char *name, DataType *dt, int is_list);
TypeSpec *append_TypeSpec(Script *sc, TypeSpecList *tl, TypeSpec *ts);
TypeSpec *parse_TypeSpec(Script *sc, InputStream *stream, Symbol *symbol);
TypeSpecList *new_TypeSpecList(Script *sc);

/*
 * The body of a user-defined command is a list of statements.
 */
typedef struct {
  StatementList *stmts;   /* The statements that implement the procedure */
} UserCmd;

/*
 * Command modifier arguments are listed in containers of the following
 * type.
 */
typedef struct {
  ScriptObj header;    /* The generic script-object header */
  char *name;          /* The name used to refer to the modifier */
  Variable *arg;       /* The argument that records the modifier */
} Modifier;

/*
 * Builtin commands use a C function to implement the procedure.
 */

#define CMD_FN(fn) int (fn)(Script *sc, VariableList *args)

typedef struct {
  CMD_FN(*cmd_fn);     /* The C function that implements the script command */
} BuiltinCmd;

struct Command {
  ScriptObj header;    /* The generic script-object header */
  char *name;          /* The name of the command */
  ModifierList *mods;  /* The list of command modifiers, or NULL if */
  /*  there aren't any. */
  VariableList *args;  /* The list of command arguments order as */
  /*  boolean modifiers, mandatory arguments, */
  /*  optional arguments. */
  unsigned narg;       /* The number of command arguments */
  ListNode *opt;       /* The first of the trailing list of optional */
  /*  arguments in 'args' (NULL if none are optional). */
  int is_builtin;      /* True if the function is implemented in C */
  union {
    BuiltinCmd builtin;/* A builtin command implemented in C */
    UserCmd user;      /* A user-defined command */
  } body;
};

Command *add_UserCommand(Script *sc, InputStream *stream);
Command *add_BuiltinCommand(Script *sc, char *declaration, CMD_FN(*cmd_fn));
Command *add_BuiltinCommand(Script *sc, char *declaration, CMD_FN(*cmd_fn), std::string description);

/*
 * The body of a user-defined function is a single expression which
 * evaluates to the function return value.
 * No statements are allowed.
 */
typedef struct {
  Expr *expr;   /* The expression that implements the function */
} UserFn;

/*
 * Builtin functions use a C function to implement the procedure, and an
 * optional loop-state function that allows loop-specific data to be
 * initialized at the start of loops which use the procedure, and
 * stepped in parallel with the loop. The FUN_FN() state member is NULL
 * for normal functions. For loop-state functions the state member is
 * used to pass the current state of the value that is initialized and
 * stepped by loop_fn(). Examples of loop-state functions include:
 *
 * A boolean elapsed(time) function that returns true when the enclosing
 * loop has been running for a given length of time.
 *
 * A boolean repeated(n) function that returns true after the nth iteration
 * of the loop, or a loop_count() function that returns the current iteration
 * number.
 */

#define FUNC_FN(fn) int (fn)(Script *sc, VariableList *args, Variable *result, \
			     Variable *state)
typedef enum {
  LOOP_ENTER,     /* Initialize a loop-state object on entry to loop */
  LOOP_INCR,      /* Step the loop-state object on new iteration of loop */
  LOOP_EXIT       /* Cleanup loop-state object on loop exit */
} LoopOper;

#define LOOP_FN(fn) void (fn)(Script *sc, Variable *state, LoopOper oper)

typedef struct {
  FUNC_FN(*func_fn);    /* The C function that implements the script function */
  LOOP_FN(*loop_fn);    /* An optional loop-iteration function */
  TypeSpec *loop_type;  /* The loop-state datatype (NULL if loop_fn==0) */
} BuiltinFn;

/*
 * This is used by expr::parse_function_call() to get the state
 * objects associated with loop-state functions and their nearest enclosing
 * loops.
 */
Variable *get_loop_data(Script *sc, InputStream *stream, Function *func);

struct Function {
  ScriptObj header;      /* The generic script-object header */
  TypeSpec *return_type; /* The return type of the function */
  VariableList *args;    /* The list of function arguments */
  unsigned narg;         /* The number of function arguments */
  ListNode *opt;         /* The first of the trailing list of optional */
  /*  arguments in 'args' (NULL if none are optional). */
  int is_builtin;        /* True if the function is implemented in C */
  union {
    BuiltinFn builtin;   /* A builtin function implemented in C */
    UserFn user;         /* A user-defined function */
  } body;
};

Function *add_UserFunction(Script *sc, InputStream *stream);
Function *add_BuiltinFunction(Script *sc, char *declaration, FUNC_FN(*func_fn));
Function *add_BuiltinFunction(Script *sc, char *declaration, FUNC_FN(*func_fn), std::string description);
Function *add_LoopStateFunction(Script *sc, char *declaration,
				FUNC_FN(*func_fn), LOOP_FN(*loop_fn),
				char *datatype);

/*
 * ScOperators are implemented via C functions of the following form.
 */
#define OPER_FN(fn) int (fn)(Script *sc, VariableList *args, Variable *result)
struct ScOperator {
  ScriptObj header;        /* The generic script-object header */
  OPER_FN(*oper_fn);       /* The C function that implements the scoperator */
  TypeSpec *return_type;   /* The return type of the scoperator */
  int narg;                /* The number of arguments expected by the scoperator*/
};
ScOperator *new_ScOperator(Script *sc, DataType *return_dt, int narg,
			   OPER_FN(*oper_fn));


/*
 * Define the method functions that support a given datatype.
 */

/*.......................................................................
 * The following method is called to parse an expression of its datatype
 * from an input stream.
 *
 * If the datatype doesn't support any expressions then this
 * method can be omitted. To read the operands of the expression such
 * methods should call parse_operand(). To parse scoperators they should
 * use InputStream functions defined in input.h to read from 'stream', and
 * add_OpFnOper() to push scoperator functions onto the expression 'e'.
 *
 * On error this function should return non-zero (eg. 1) and leave an
 * error message in the input stream error buffer, via a call to
 * input_error().
 */
#define DT_PARSE(fn) int (fn)(Script *sc, DataType *dt, \
			      InputStream *stream, Expr *e)

/*.......................................................................
 * The following mandatory method is called to parse a constant of its
 * datatype from an input stream.
 *
 * It should use functions from input.h to parse the constant, then
 * call new_Variable() to allocate a variable for the constant,
 * append an instruction to load that variable to the expression 'e',
 * and initialize the variable with the constant value that was read.
 *
 * On error this function should return non-zero (eg. 1) and leave an
 * error message in the input stream error buffer, via a call to
 * input_error().
 */
#define DT_CONST(fn) int (fn)(Script *sc, DataType *dt, \
			      InputStream *stream, Expr *e)

/*.......................................................................
 * The following method is called to print the value of a variable of its
 * datatype to a given output stream. It should return non-zero on error.
 */
#define DT_PRINT(fn) int (fn)(Script *sc, OutputStream *output, Variable *var)

/*.......................................................................
 * The following method is called to check a value parsed by a DT_CONST()
 * method. It is kept separate from the DT_CONST() method so as to cater
 * for datatypes that differ only in the legal ranges of values that they
 * support. For other types, where the value is validated by the DT_CONST()
 * method, this method can be omitted (pass 0 to new_DataType()).
 */
#define DT_CHECK(fn) int (fn)(Script *sc, Variable *var, InputStream *stream)

/*.......................................................................
 * The following optional method is used by the 'do' command to set the
 * current value of a do-loop variable of the associated datatype.
 * At the start of a new loop that has a variable of the datatype that
 * this function is registered to, the function will be called with
 * value=NULL. This indicates that the function should compute
 * the number of steps involved in the do loop and return it. The step
 * should be computed as:
 *
 *  nstep = floor((last - first) / step) + 1
 *
 * If the returned value is less than 2, no iteration is possible, so
 * the do-statement will simply run the body of the loop once with
 * multiplier=0. If first==last or 'step' goes in the opposite
 * direction to first..last, simply return 0.
 *
 * The function will be called nstep times, with 'multiplier'
 * set to 0..nstep-1. If nstep < 1, the function will be called
 * once with multiplier=0. The function should assign the following
 * value to 'value':
 *
 *   first + step * multiplier
 */
#define DT_ITER(fn) int (fn)(Script *sc, Variable *first, Variable *last, \
			      Variable *step, int multiplier, Variable *value)

/*
 * This is a do-loop iterator function suitable for use with datatypes
 * in which both the datatype itself, and its incrementing datatype,
 * are implemented as DoubleVariable's. Possible incrementing datatypes
 * include the builtin datatypes returned by sc_Double_dt(sc) and
 * sc_Sexagesimal_dt(sc).
 */
DT_ITER(sc_iterate_double);

/*
 * This is a do-loop iterator function suitable for use with datatypes
 * in which both the datatype itself, and its incrementing datatype,
 * are implemented as IntVariable's. Possible incrementing datatypes
 * include the builtin datatype returned by sc_Integer_dt(sc).
 */
DT_ITER(sc_iterate_int);

/*
 * This is a do-loop iterator function suitable for use with datatypes
 * in which the datatype is implemented as a UintVariable and its
 * incrementing datatype is implemented as a IntVariable. Possible
 * incrementing datatypes include the builtin datatype returned by
 * sc_Integer_dt(sc).
 */
DT_ITER(sc_iterate_uint);

/*
 * Group datatypes are the equivalent of C structs or Pascal records.
 * A pointer to a different object of the following type is recorded in the
 * context member of the DataType structure. It contains the distinguishing
 * attributes of each group type.
 */
typedef struct {          /* The instance data of a 'group' datatype */
  ScriptObj header;       /* The required garbage collection header */
  char *name;             /* The symbol-name given to the type */
  TypeSpecList *fields;   /* The group-field variable declarations */
  int nfield;             /* The number of group fields */
} GroupType;

DataType *add_GroupDataType(Script *sc, InputStream *stream);
DataType *add_BuiltinGroupDataType(Script *sc, char *declaration);

/*
 * The following type is used to contain one name/value pair of
 * a 'choice' or 'set' datatype.
 */
typedef struct {         /* An enumeration name/value pair */
  char *name;            /* The name of the enumerator */
  unsigned value;        /* The value of the enumerator */
  char* explanation;
} Enumerator;

/*
 * There is only one wildcard datatype.
 *
 * Wildcard values are stored in Variable variables.
 */
DataType *add_WildcardDataType(Script *sc);

/*
 * There is only one boolean datatype.
 *
 * Boolean values are stored in BoolVariable variables.
 */
DataType *add_BooleanDataType(Script *sc);

/*
 * Choice datatypes are like C enumerations. They present the user with
 * a symbolic menu of choices from which the user can pick one entry
 * at a time.
 *
 * Choice values are stored in ChoiceVariable variables.
 */
DataType *add_ChoiceDataType(Script *sc, char *name, Enumerator *choices,
			     int nchoice, bool allow_bit_mask=false);

/*
 * Set datatypes allow the user to specify one or more members of a set
 * of up to 32 symbolic names, via expressions in which members are added
 * to the set with '+' and removed from the set with '-'. The C values
 * associated with set members are to be interpretted as bit-masks.
 * Adding a member to a set is equivalent to a bitwise OR of the member
 * value with the current bitset. Removing from a set is equivalent to
 * a bitwise AND of the complement of the member with the current bitset.
 *
 * Set values are stored in SetVariable variables.
 */
DataType *add_SetDataType(Script *sc, char *name, int allow_bit_mask,
			  Enumerator *members, unsigned nmember, DT_ITER(*iter_fn), char *incr_name);

/*
 * Create a quoted or optionally unquoted string datatype.
 * If the 'quoted' argument of add_StringDataType() is true, then
 * strings that the user enters must be enclosed in quotes.
 * If the quoted argument is false, then quotes can optionally be omitted,
 * and the end of the string will be the point at which an unopened
 * close bracket or quote is seen, or a comma is encountered outside of
 * parentheses or quotes.
 */
DataType *add_StringDataType(Script *sc, char *name, int quoted,
			     DT_CHECK(*check_fn));

/*
 * Create a keyword datatype. Constants of this datatype are
 * stored in StringVariable's. A valid keyword is one that
 * starts with an alphabetic character then continues with
 * alphanumeric and underscore characters. If you need the
 * alphabetical characters to be folded to lower case, pass
 * fold=1, otherwise pass fold=0. After a syntactically
 * valid keyword has been read, the optional check_fn() is
 * called to see if the keyword is valid for the particular
 * datatype.
 */
DataType *add_KeywordDataType(Script *sc, char *name, int fold,
			      DT_CHECK(*check_fn));

/*
 * Create a pathname datatype. Constants of this datatype are
 * read as literal strings. If the first character is a ~ then
 * home-directory substitution is performed. Validation of the
 * pathname should be performed by the caller's check_fn().
 */
DataType *add_PathDataType(Script *sc, char *name, DT_CHECK(*check_fn));

/*
 * There is only one symbol datatype.
 *
 * Symbol values are stored in SymbolVariable variables.
 */
DataType *add_SymbolDataType(Script *sc);

/*
 * Provide a way to distinguish between builtin and aggregate types.
 */
typedef enum {
  DT_GROUP,        /* User-defined group datatypes */
  DT_BUILTIN       /* A builtin datatype */
} TypeClass;

/*
 * There are two relational scoperators that datatypes can support.
 * The equality scoperator is mandatory. The greater-than method
 * is optional. They are implemented by functions of the following
 * type.
 *
 * Input:
 *  va     Variable *   The first of two variables of the parent dataype.
 *  vb     Variable *   The second of two variables of the parent dataype.
 * Output:
 *  return      int     0 - The relation was not satisfied.
 *                      1 - The relation was satisfied.
 */
#define DT_RELFN(fn)   int (fn)(Variable *va, Variable *vb)

/*
 * Each data-type that is recognized by the interpretter is described by
 * an object of the following type. Datatypes that are added when
 * sc->prog is NULL are added to the global scope. Thereafter they
 * are added to the current local scope.
 */
struct DataType {
  ScriptObj header;      /* The generic script-object header */
  char *name;            /* The name of the datatype */
  DT_PARSE(*parse_fn);   /* A method to parse an expression of the datatype */
  DT_CONST(*const_fn);   /* A method to parse a constant of the datatype */
  DT_CHECK(*check_fn);   /* A method to domain-check a value of the datatype */
  DT_PRINT(*print_fn);   /* A method to print an instance of the datatype */
  DT_RELFN(*eq_fn);      /* A method that returns 1 if a == b, 0 if a != b */
  DT_RELFN(*gt_fn);      /* An optional method that returns 1 if a > b */
  DT_RELFN(*in_fn);      /* An optional method that returns 1 if a contains b */
  DT_ITER(*iter_fn);     /* The optional do-loop iteration function */
  DataType *incr_dt;     /* The incremental datatype to for iter_fn() steps */
  void *context;         /* Type-specific context data */
  size_t vsize;          /* The size of a variable of this datatype */
  FreeList *vmemory;     /* The free-list from which to allocate variables */
  TypeSpec *list_reg;    /* The listof type specifier for registers */
  TypeSpec *atom_reg;    /* The type specifier for registers */
  Variable *null_atom;   /* A null atom variable of this datatype */
  Variable *null_list;   /* A null list variable of this datatype */
  TypeClass dataclass;   /* The dataclass of datatype */
  DataTypeId id;         // The generic data type that this inherits from
};

DataType *new_DataType(Script *sc, char *name, TypeClass dataclass,
		       void *context, size_t vsize,
		       DT_CHECK(*check_fn), DT_PARSE(*parse_fn),
		       DT_CONST(*const_fn), DT_PRINT(*print_fn),
		       DT_RELFN(*equal_fn), DT_RELFN(*gt_fn), DT_RELFN(*in_fn),
		       DT_ITER(*iter_fn), char *incr_name,
		       DataTypeId id = DT_UNK);

/*
 * Some data-types have well defined syntactic and storage
 * formats formats but value domains that depend on their
 * specific usage. Thus the following constructors provide the
 * parsing and print functions for a set of general datatypes,
 * but require a domain-checking function to complete the datatype
 * implementation.
 */

/* Create an unsigned integer datatype (See UintVariable) */

DataType *add_UintDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			   DT_ITER(*iter_fn), char *incr_name);

/* Create a signed-int datatype (see IntVariable) */

DataType *add_IntDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			  DT_ITER(*iter_fn), char *incr_name,
			  int allow_negation);

/* Create a double-precision datatype used (see DoubleVariable) */

DataType *add_DoubleDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			     DT_ITER(*iter_fn), char *incr_name,
			     int allow_negation);

/*
 * Add a sexagesimal datatype.
 *
 * The resulting number is recorded in a DoubleVariable with the units
 * of the first sexagesimal component. For example if one entered
 * -12:30:36.0, then the recorded value would be -12.51.
 */
DataType *add_SexagesimalDataType(Script *sc, char *name, DT_CHECK(*check_fn),
				  DT_ITER(*iter_fn), char *incr_name,
				  int allow_negation);

/*
 * Parse an operand of a given datatype. This is intended for use
 * by the parse_fn() methods of datatypes.
 */
int parse_operand(Script *sc, DataType *dt, int is_list,
		  InputStream *stream, Expr *e);

/*
 * Parse a variable reference, function call, or cast expression after
 * encountering a $ scoperator. The type of the resulting expression is
 * returned. This will be consistent with dt and is_list unless dt==NULL,
 * or an error occurs. If dt==NULL, then type-checking will be disabled
 * and it is left to the caller to check the returned type.
 */
TypeSpec *parse_dollar_expr(Script *sc, DataType *dt, int is_list,
			    InputStream *stream, Expr *e);

/*
 * Parse a single argument expression.
 */
int parse_argument(Script *sc, TypeSpec *target, InputStream *stream, Expr *e);

/*
 * Parse the argument list of a function or command.
 */
int parse_procedure_arguments(Script *sc, char *name, int is_func, List *args,
			      ModifierList *mods, ListNode *opts,
			      InputStream *stream, Expr *e);

/*
 * The following enumerates the values associated with individual bits
 * of a bitwise union of variable usage flags.
 */
typedef enum {
  VAR_IS_OPT=1,     /* The variable is an optional procedure argument. */
  /*  When the parent procedure is active the */
  /*  VAR_IS_NUL flag must be used to determine whether */
  /*  the argument has been given a value. */
  VAR_IS_NUL=2,     /* The variable currently has no value. */
  VAR_IS_CONST=4,   /* The value of the variable is unchangeable */
  VAR_IS_MODIFIER=8 /* True if the variable is a boolean command modifier */
} VarFlags;

/*
 * All script variables have an initial member of the following type.
 * This contains the description of the variable type and how to
 * release it once redundant. All functions that operate on variables
 * take a Variable * argument, including the type-specific method functions
 * in Variable::type->dt. Where access to a variable's value is needed,
 * the Variable * pointer should be cast back to the specific variable type.
 * This will be a ListVariable type if Variable::type->is_list is
 * non-zero.
 */
struct Variable {
  ScriptObj header;     /* The generic script-object header */
  TypeSpec *type;       /* The declaration of the variable */
  unsigned flags;       /* A bitwise union of VarFlags enumerators */
};

Variable *new_Variable(Script *sc, TypeSpec *type);
Variable *copy_Variable(Variable *dst, Variable *src);
ListNode *append_Variable(Script *sc, VariableList *vl, Variable *var);
int print_variable(Script *sc, OutputStream *output, Variable *var);
int get_Arguments(VariableList *vl, ...);
int print_ArgumentList(Script *sc, OutputStream *stream, int expand,
		       VariableList *args);
Variable *add_BuiltinVariable(Script *sc, char *declaration);

VariableList *new_VariableList(Script *sc);

/*
 * In builtin procedures that take optional arguments, the following
 * macro should be applied to each optional argument variable to determine
 * whether it has been given a value. The value of the variable should be
 * ignored if the macro returns zero.
 */
#define OPTION_HAS_VALUE(arg) (~(arg)->flags & VAR_IS_NUL)

/*
 * Describe canned variable types.
 */
typedef struct {           /* A variable that contains a list of variables */
  Variable v;              /* Generic variable members */
  VariableList *list;      /* A list of variables of type v.type->dt */
} ListVariable;

#define LIST_VARIABLE(v) ((ListVariable *)(v))

typedef struct {           /* An unsigned integer datatype */
  Variable v;              /* Generic variable members */
  unsigned uint;           /* The value of the variable */
} UintVariable;

#define UINT_VARIABLE(v) ((UintVariable *)(v))

DT_RELFN(sc_equal_uint);   /* See if two UintVariable's have the same value */
DT_RELFN(sc_gt_uint);      /* See if one UintVariable value is greater */
                           /*  than another. */

typedef struct {           /* A set variable */
  Variable v;              /* Generic variable members */
  unsigned set;            /* The bit-mask value of the variable */
} SetVariable;

#define SET_VARIABLE(v) ((SetVariable *)(v))

typedef struct {           /* A enumerated choice variable */
  Variable v;              /* Generic variable members */
  unsigned choice;         /* The enumerated value of the choice */
} ChoiceVariable;

#define CHOICE_VARIABLE(v) ((ChoiceVariable *)(v))

typedef struct {           /* A boolean variable */
  Variable v;              /* Generic variable members */
  unsigned boolvar;           /* The boolean value of the variable. Logical */
  /*  true is represented by a non-zero value. */
} BoolVariable;

#define BOOL_VARIABLE(v) ((BoolVariable *)(v))

typedef struct {           /* A signed integer variable */
  Variable v;              /* Generic variable members */
  int i;                   /* The integer value of the variable */
} IntVariable;

#define INT_VARIABLE(v) ((IntVariable *)(v))

DT_RELFN(sc_equal_int);    /* See if two IntVariable's have the same value */
DT_RELFN(sc_gt_int);       /* See if one IntVariable value is greater than */
                           /*  another. */

typedef struct {           /* A double-precision floating point variable */
  Variable v;              /* Generic variable members */
  double d;                /* The value of the variable */
} DoubleVariable;

#define DOUBLE_VARIABLE(v) ((DoubleVariable *)(v))

DT_RELFN(sc_equal_double); /* See if two DoubleVariable's have the same value */
DT_RELFN(sc_gt_double);    /* See if one DoubleVariable value is greater */
                           /*  than another. */

typedef struct {           /* A string-precision floating point variable */
  Variable v;              /* Generic variable members */
  char *string;            /* An immutable string allocated from the string */
  /*  pool of the script */
} StringVariable;

#define STRING_VARIABLE(v) ((StringVariable *)(v))

DT_RELFN(sc_equal_string); /* See if two StringVariable's have equal values */
DT_RELFN(sc_in_string);    /* See if one StringVariable value is a substring */
                           /*  of another StringVariable value. */

typedef struct {           /* A script symbol */
  Variable v;              /* Generic variable members */
  SymbolType type;         /* The type of symbol refered to by the variable */
  union {
    Variable *var;         /* The variable refered to if type==SYM_VARIABLE */
    Function *func;        /* The function refered to if type==SYM_FUNCTION */
    Command *cmd;          /* The command refered to if type==SYM_COMMAND */
    DataType *dt;          /* The datatype refered to if type==SYM_DATATYPE */
    char *keyword;         /* The name of a keyword */
  } data;
} SymbolVariable;

#define SYMBOL_VARIABLE(v) ((SymbolVariable *)(v))

/*-----------------------------------------------------------------------
 * The Signal datatype is used to specify a signal to be sent or
 * received.
 *
 * It is stored in a SignalVariable and parsed as a lowercase keyword.
 */
typedef struct {
  Variable v;       /* The base-dataclass members of the variable (see script.h) */
  Symbol *sym;      /* The symbol-table entry of the signal */
} SignalVariable;

#define SIGNAL_VARIABLE(v)  ((SignalVariable *)(v))

DataType *add_SignalDataType(Script *sc, char *name);

/* Expr objects contain compiled expressions (see expr.c) */

Expr *new_Expr(Script *sc);

/* An ExprOper object contains one instruction of an expression */

ExprOper *add_LoadOper(Script *sc, Expr *e, Variable *var);
ExprOper *add_FuncOper(Script *sc, Expr *e, Function *func, Variable *state);
ExprOper *add_ListOper(Script *sc, Expr *e, DataType *dt, unsigned n);
ExprOper *add_GroupOper(Script *sc, Expr *e, DataType *dt);
ExprOper *add_FieldOper(Script *sc, Expr *e, unsigned field);
ExprOper *add_CloneOper(Script *sc, Expr *e, TypeSpec *type);
ExprOper *add_OpFnOper(Script *sc, Expr *e, ScOperator *oper);
ExprOper *add_SkipOper(Script *sc, Expr *e);
ExprOper *add_StoreOper(Script *sc, Expr *e, Variable *var);
ExprOper *add_UnsetOper(Script *sc, Expr *e, Variable *var);
ExprOper *add_SetBoolOper(Script *sc, Expr *e, Variable *var, int state);
ExprOper *add_SetStringOper(Script *sc, Expr *e, Variable *var, char *string);

/*
 * The following two functions are provided for parsing functions that
 * need to ensure that an argument doesn't alias a user variable.
 * Before parsing the argument, get_ExprEnd() is called to record the
 * current end of the expression. Then immediately after parsing the
 * argument, remove_alias() is called, along with the position previously
 * returned by get_ExprEnd(). remove_alias() checks to see if the
 * expression that follows 'old_end' is a variable alias, and if so it
 * appends a copy-variable instruction to the instruction list.
 */
ListNode *get_ExprEnd(Expr *e);
int remove_alias(Script *sc, Expr *e, ListNode *old_end);

/*
 * Evaluate an expression.
 */
int exe_Expr(Script *sc, Expr *expr);

/*
 * Pop a given number of arguments from the expression-evaluation stack and
 * return them as an argument list.
 */
VariableList *pop_ExprStackArgs(Script *sc, Expr *expr, int narg);

/*
 * Pop a single value from the top of the expression-evaluation stack.
 */
Variable *pop_ExprStack(Script *sc, Expr *expr);

/*
 * Allocate a string from the appropriate string pool.
 */
char *new_ScriptString(Script *sc, char *string);

/*
 * During compilation the symbol-tables of nested scopes will be
 * kept as a list of hash-tables, organized as a stack.
 */
int push_Scope(Script *sc);
int pop_Scope(Script *sc);

/*
 * During compilation nested statements are pushed onto a stack.
 * This is then used when parsing break, next and loop-state functions.
 */
Statement *push_CompStatement(Script *sc, Statement *stmt);
Statement *pop_CompStatement(Script *sc);

/*
 * Add a symbol to the innermost lexical scope.
 */
Symbol *add_ScriptSymbol(Script *sc, char *name, SymbolType code, void *data);
Symbol *add_ScriptSymbol(Script *sc, char *name, SymbolType code, std::string description);

/*
 * Work back from the innermost scope to locate a named symbol.
 * Note that the stream argument is optional and only used for
 * error reporting.
 */
Symbol *find_ScriptSymbol(Script *sc, InputStream *stream, char *name);

/*
 * Attempt to find a datatype by name.
 */
DataType *find_DataType(Script *sc, InputStream *stream, char *name);

/*
 * Executing statements or lists of statements are recorded on a stack
 * of ExeFrame objects. Each frame records the current statement being
 * executed, its list node if it is part of a statement list, and
 * the parent execution frame that invoked it.
 */
ExeFrame *push_ExeFrame(Script *sc, Statement *stmt, ListNode *next);
Statement *pop_ExeFrame(Script *sc);

/*
 * String pool methods.
 */
StringPool *new_StringPool(void);
StringPool *del_StringPool(StringPool *sp);
char *new_StringPool_string(StringPool *sp, char *string);
int clr_StringPool(StringPool *sp);

/*
 * Script signaling functions.
 */
int add_script_signal(Script *sc, char *name);
Symbol *lookup_script_signal(Script *sc, char *name);
int signal_script(Script *sc, Symbol *sig);
int clear_script_signal(Symbol *sig);
int reset_script_signal(Symbol *sig);

/*
 * The following function inspects all enclosing catch statements, looking
 * for catch clauses that have become true. If such a statement is found,
 * the current statement is cancelled, and the execution stack is unwound
 * to that statement.
 */
typedef enum {
  CAUGHT_EVENT,    /* A catch clause was true */
  CAUGHT_NOTHING,  /* No catch clauses were true */
  CAUGHT_ERROR     /* A fatal error occured */
} CatchState;

CatchState catch_script_events(Script *sc);

/*
 * The context member of each choice DataType will point to a object
 * different object of the following type. It contains the distinguishing
 * details of a given choice datatype.
 */
typedef struct {              /* The instance data of a 'choice' datatype */
  ScriptObj header;           /* The required garbage collection header */
  Enumerator *choices;        /* The 'nmember' name/value pairs */
  unsigned nchoice;           /* The number of choices in choices[] */
  bool allow_bit_mask;
} ChoiceType;

/*
 * The context member of each Set DataType will point to a object
 * different object of the following type. It contains the distinguishing
 * details of a given Set datatype.
 */
typedef struct {            /* The instance data of a 'set' datatype */
  ScriptObj header;         /* The required garbage collection header */
  Enumerator *members;      /* The 'nmember' name/value pairs */
  unsigned nmember;         /* The number of set member values */
  unsigned all;             /* The union of all set member values */
  unsigned nbit;            /* The number of bits set in 'all' */
  int allow_bit_mask;       /* True to allow users to type numeric bit masks */
  ScOperator *inc_op;         /* An scoperator proc that includes bits to a set */
  ScOperator *exc_op;         /* An scoperator proc that excludes bits from a set */
} SetType;


#endif
