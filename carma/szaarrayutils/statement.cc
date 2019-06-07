#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "carma/szaarrayutils/script.h"
#include "carma/szaarrayutils/pathname.h"
#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/Exception.h"

/*
 * Variable definition statements are recorded in objects of the
 * following type.
 */
typedef struct {
  Variable *var;       /* The variable to be set */
  Expr *expr;          /* The expression to evaluate and assign */
} VariableStmt;

/*
 * Foreach-loops are recorded in objects of the following type.
 */
typedef struct {
  Variable *var;        /* The loop-variable of the foreach statement */
  Expr *expr;           /* The list-expression */
  StatementList *stmts; /* The statements enclosed by the loop */
  ListNode *next_node;  /* When the foreach loop is active, this member */
                        /*  records the variable-list node who's value is to */
                        /*  assigned to the loop variable on the next */
                        /*  iteration. Otherwise it is NULL */
} ForeachStmt;

/*
 * Do-loops are recorded in objects of the following type.
 */
typedef struct {
  Variable *var;        /* The loop-variable of the foreach statement */
  DT_ITER(*iter_fn);    /* The iterator function of the loop */
  Expr *expr;           /* The expression for the loop bounds and increment */
  StatementList *stmts; /* The statements enclosed by the loop */
  Variable *first;      /* The initial value of the loop-variable */
  Variable *last;       /* The final value of the loop-variable */
  Variable *step;       /* The loop increment */
  int iteration;        /* The number of iterations completed so far */
  int nstep;            /* The number of iterations to perform. This value */
                        /*  queried from iter_fn() whenever the loop is */
                        /*  entered. */
} DoStmt;

/*
 * Until-statements are recorded in objects of the following type.
 */
typedef struct {
  Expr *clause;         /* The boolean clause expression */
} UntilStmt;

/*
 * While-loops are recorded in objects of the following type.
 */
typedef struct {
  Expr *clause;         /* The boolean expression clause */
  StatementList *stmts; /* The statements enclosed by the loop */
} WhileStmt;

/*
 * Command invokations are recorded in objects of the following type.
 */
typedef struct {
  Command *cmd;        /* The declaration of the command */
  Expr *expr;          /* The argument-list expressions */
} CallStmt;

/*
 * The print command takes a variable number of polymorphic arguments.
 */
typedef struct {
  Expr *expr;          /* The argument-list expressions */
  int narg;            /* The number of arguments provided */
} PrintStmt;

typedef struct {
  Expr *expr;          /* The clause evaluation expression */
  List *blocks;        /* The list of statement lists (one list per clause) */
} IfStmt;

/*
 * The ifhost command is used for host-specific conditional compilation.
 * If the specified hostname matches that of the host computer, the 
 * statement-list that follows it is compiled and placed in Ifhost::stmts.
 * If the hostname doesn't match, the statements are skipped and Ifhost::stmts
 * is set to NULL.
 */
typedef struct {
  StatementList *stmts;/* The list of statements to be executed, or NULL */
} IfhostStmt;

/*
 * The exit statement tells the script to terminate after running
 * any cleanup statements that the user has registered.
 */
typedef struct {
  Expr *expr;          /* The optional reason-for-exit string expression */
} ExitStmt;

/*
 * The "catch" statement evaluates its clause before executing each
 * of the statements in its statement list, aborting the block of
 * statements early if the clause becomes true.
 */
typedef struct {
  Expr *clause;          /* The clause to catch */
  StatementList *stmts;  /* The list of statements to be enclosed */
  StatementList *action; /* The list of statements to execute if the clause */
                         /*  becomes true */
  ListNode *node;        /* If the catch statement is active, this will be */
                         /*  the node in sc->script.catch_list where the */
                         /*  the catch statement is currently registered. */
} CatchStmt;

/*
 * Compiled statements are recorded in containers of the following
 * type.
 */
typedef enum {
  STMT_EMPTY,          /* A null statement */
  STMT_VAR,            /* A variable assignment statement */
  STMT_FOREACH,        /* A foreach loop-statement */
  STMT_DO,             /* A do-loop statement */
  STMT_UNTIL,          /* An until barrier-statement */
  STMT_WHILE,          /* A while loop-statement */
  STMT_CALL,           /* A command invokation statement */
  STMT_PRINT,          /* A print command */
  STMT_LOG,            /* A log command */
  STMT_IF,             /* An if() statement */
  STMT_IFHOST,         /* An ifhost statement */
  STMT_BREAK,          /* An break statement */
  STMT_NEXT,           /* A next statement (like C's continue statement) */
  STMT_EXIT,           /* An exit statement */
  STMT_RETURN,         /* A return-from-procedure statement */
  STMT_CATCH           /* A catch-event statement */
} StmtType;

struct Statement {      /* This is typedef'd as Statement in script.h */
  ScriptObj header;     /* The generic script-object header */
  StmtType type;        /* The type of statement */
  LoopStateList *loop;  /* If the statement is a loop or until statement */
                        /*  then this will be a list of parallel loop-state */
                        /*  objects. Otherwise it will be NULL */
  union {
    VariableStmt var;   /* A variable declaration statement */
    ForeachStmt foreach;/* A foreach statement */
    DoStmt do_s;        /* A do-loop statement */
    UntilStmt until;    /* An until statement */
    WhileStmt while_s;  /* A while() statement */
    CallStmt  call;     /* A command invokation statement */
    PrintStmt print;    /* A print command */
    IfStmt if_s;        /* An if() statement */
    IfhostStmt ifhost;  /* An "ifhost hostname {}" statement */
    ExitStmt exit_s;    /* An exit program statement */
    CatchStmt catchstmt;    /* A catch-event statement */
  } context;
};

static Statement *new_Statement(Script *sc, StmtType type);
static Statement *append_Statement(Script *sc, StatementList *sl,
				   Statement *stmt);
static StatementList *new_StatementList(Script *sc);
static int skip_StatementList(Script *sc, InputStream *stream);

static int parse_VariableStmt(Script *sc, InputStream *stream, Symbol *symbol,
			      VariableStmt *stmt);
static int parse_ForeachStmt(Script *sc, InputStream *stream,ForeachStmt *stmt);
static int parse_DoStmt(Script *sc, InputStream *stream, DoStmt *stmt);
static int parse_UntilStmt(Script *sc, InputStream *stream, UntilStmt *stmt);
static int parse_WhileStmt(Script *sc, InputStream *stream, WhileStmt *stmt);
static int parse_CallStmt(Script *sc, InputStream *stream, Command *cmd,
			  CallStmt *stmt);
static int parse_PrintStmt(Script *sc, InputStream *stream, PrintStmt *stmt);
static int parse_IfStmt(Script *sc, InputStream *stream, IfStmt *stmt);
static int parse_IfNode(Script *sc, InputStream *stream, IfStmt *stmt,
			int final_else);
static int parse_IfhostStmt(Script *sc, InputStream *stream, IfhostStmt *stmt);
static int parse_BreakStmt(Script *sc, InputStream *stream);
static int parse_NextStmt(Script *sc, InputStream *stream);
static int parse_ImportStmt(Script *sc, InputStream *stream);
static int parse_ExitStmt(Script *sc, InputStream *stream, ExitStmt *stmt);
static int parse_CleanupStmt(Script *sc, InputStream *stream);
static int parse_CatchStmt(Script *sc, InputStream *stream, CatchStmt *stmt);

static int exe_VariableStmt(Script *sc, VariableStmt *stmt);
static int exe_ForeachStmt(Script *sc, ForeachStmt *stmt);
static int exe_DoStmt(Script *sc, DoStmt *stmt);
static int exe_UntilStmt(Script *sc, UntilStmt *stmt);
static int exe_WhileStmt(Script *sc, WhileStmt *stmt);
static int exe_CallStmt(Script *sc, OutputStream *log, CallStmt *stmt);
static int exe_PrintStmt(Script *sc, OutputStream *output, PrintStmt *stmt);
static int exe_IfStmt(Script *sc, IfStmt *stmt);
static int exe_IfhostStmt(Script *sc, IfhostStmt *stmt);
static int exe_BreakStmt(Script *sc);
static int exe_NextStmt(Script *sc);
static int exe_ExitStmt(Script *sc, ExitStmt *stmt);
static int exe_ReturnStmt(Script *sc);
static int exe_CatchStmt(Script *sc, CatchStmt *stmt);

static ScriptState bad_script(Script *sc);

/*
 * The following enumeration is used to specify what to do when
 * the statement of an executing frame is next considered for
 * execution. By default after step_ExeFrame() starts a new statement
 * it sets the pending state to EXE_STMT_FINISH. Thus the execution
 * functions of loop statements must change this to EXE_STMT_REPEAT
 * if the statement should be repeated.
 */
typedef enum {
  EXE_STMT_START,       /* Initialize and run the statement */
  EXE_STMT_REPEAT,      /* Run the statement again */
  EXE_STMT_FINISH       /* Cleanup and start the next statement */
} NextExe;

/*
 * At run time, active statements are pushed onto a stack, with
 * the leaf statement being executed positioned at the top of
 * the stack. The statement is recorded in a container of the
 * following type.
 */
struct ExeFrame {
  ExeFrame *parent;     /* The statement node that invoked this statement */
  Statement *stmt;      /* The statement being executed */
  ListNode *stmt_node;  /* If the statement is part of a list, stmt_node */
                        /*  should be the ListNode that contains 'stmt' */
  NextExe whatnext;     /* This specifies what to do with 'stmt' the next */
                        /*  time that step_ExeFrame() is called. */
};

static Statement *step_ExeFrame(Script *sc);
static ExeFrame *top_ExeFrame(Script *sc);
static void end_ExeFrame_Statement(Script *sc, ExeFrame *frame);
static int requeue_ExeFrame_Statement(Script *sc);

/*
 * When a loop-state function is encountered, the innermost enclosing
 * loop-statement is located and an object of the following type is
 * associated with that statement. Whenever the loop is entered this
 * state is initialized via a call to the LOOP_FN() function associated
 * with the loop-state function. Similarly, the state is iterated on each
 * subsequent call and cleared when the loop exits. The data is also
 * recorded in the FuncOper instructions associated with the enclosed
 * calls to the loop-state function, and is passed to that function whenever
 * it is evaluated.
 */
struct LoopState {
  ScriptObj header;    /* The generic script-object header */
  Variable *state;     /* The current value of the loop-state function */
  LOOP_FN(*loop_fn);   /* The function to use to initialize and step var */
};

static Statement *find_CompLoopStmt(Script *sc);
static LoopState *get_LoopState(Script *sc, InputStream *stream,Function *func);
static int update_LoopState(Script *sc, Statement *stmt, LoopOper oper);
static LoopStateList *new_LoopStateList(Script *sc);

/*.......................................................................
 * Parse a list of statements enclosed in braces.
 *
 * Note that it is left to the caller to check for trailing input after
 * the '}' terminator of the list.
 *
 * Input:
 *  sc             Script *  The host script environment.
 *  stream    InputStream *  The stream to parse from.
 *  braces            int    True if the statement list must be delimited
 *                           by enclosing braces.
 * Output:
 *  return  StatementList *  The new statement list, or NULL on error.
 */
StatementList *parse_StatementList(Script *sc, InputStream *stream, int braces)
{
  StatementList *stmts;   /* The list to be returned */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "parse_StatementList: NULL argument(s).\n");
    return NULL;
  };
/*
 * We can't compile statements into the builtin environment.
 */
  if(sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr, "parse_StatementList called before compilation enabled.\n");
    return NULL;
  };
/*
 * Create the list.
 */
  stmts = new_StatementList(sc);
  if(!stmts)
    return NULL;
/*
 * Most statement lists are delimited by braces.
 */
  if(braces) {
    if(input_skip_white(stream, 1, 0))
      return NULL;
    if(stream->nextc != '{') {
      input_error(stream, 1, "Missing '{' at start of statement list.\n");
      return NULL;
    };
    if(input_skip_white(stream, 1, 1))
      return NULL;
  };
/*
 * Parse and append statements to the statement list until the
 * close brace is encountered.
 */
  while(stream->nextc != (braces ? '}' : EOF)) {
    Statement *stmt = parse_Statement(sc, stream, 0);
    if(!stmt)
      return NULL;
    if(stmt->type != STMT_EMPTY && !append_Statement(sc, stmts, stmt))
      return NULL;
/*
 * Locate the next statement or the terminator of the list.
 */
    if(input_skip_white(stream, 1, 0))
      return NULL;
  };
/*
 * Skip the terminator of the list.
 */
  if(braces && input_skip_space(stream, 1, 1))
    return NULL;
  return stmts;
}

/*.......................................................................
 * Parse the statement-list of an interactive single-statement program.
 *
 * Input:
 *  sc             Script *  The host script environment.
 *  stream    InputStream *  The stream to parse from.
 * Output:
 *  return  StatementList *  The new statement list, or NULL on error.
 */
StatementList *parse_RestrictedStatementList(Script *sc, InputStream *stream)
{
  StatementList *stmts;   /* The list to be returned */
  Statement *stmt;        /* The statement that constitutes the program */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "parse_RestrictedStatementList: NULL argument(s).\n");
    return NULL;
  };
/*
 * We can't compile statements into the builtin environment.
 */
  if(sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr,
        "parse_RestrictedStatementList() called before compilation enabled.\n");
    return NULL;
  };
/*
 * Create the list.
 */
  stmts = new_StatementList(sc);
  if(!stmts)
    return NULL;
/*
 * Parse a single restricted statement.
 */
  stmt = parse_Statement(sc, stream, 1);
  if(!stmt)
    return NULL;
  if(stmt->type != STMT_EMPTY && !append_Statement(sc, stmts, stmt))
    return NULL;
/*
 * There should be nothing following the statement.
 */
  if(input_skip_white(stream, 1, 0))
    return NULL;
  if(stream->nextc != EOF) {
    input_error(stream, 1, "Unexpected characters follow a valid statement.\n");
    return NULL;
  };
  return stmts;
}

/*.......................................................................
 * Parse a script statement from an input stream.
 *
 * Function, commmand and group definition statements are not compiled
 * into statement objects. Instead each one parsed results in the defined
 * object being created and registered as a symbol in the local
 * scope. This function will parse as many such statements as occur
 * before the next statement that results in the creation of a
 * statement object. If, after successfully parsing at least one
 * declarative statement, the next character is not dataclassifiable as a
 * statement, a statically allocated empty statement
 * (type==STMT_EMPTY), will be returned.
 *
 * Input:
 *  sc          Script *  The host script environment.
 *  stream InputStream *  The stream to parse from.
 *  interactive    int    If true, disallow all statements except
 *                        calls to builtin commands. This facilitates
 *                        support for fast interactive command/response
 *                        style environments.
 * Output:
 *  return   Statement *  The parsed statement, an empty statement if
 *                        only command and variable definitions were
 *                        found, or NULL on error.
 */
Statement *parse_Statement(Script *sc, InputStream *stream, int interactive)
{
  static Statement empty_stmt = {{0}, STMT_EMPTY};  /* An empty statement */
  Statement *stmt = NULL;  /* The statement to be returned */
  StmtType type;           /* The type of statement to allocate */
  Symbol *symbol;          /* A known symbol */
  int restricted;          /* True if a statement is restricted to batch mode */
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "parse_Statement: NULL argument(s).\n");
    return NULL;
  };
/*
 * We can't compile statements into the builtin environment.
 */
  if(sc->script.state == SCRIPT_EMPTY) {
    lprintf(stderr, "parse_Statement called before compilation enabled.\n");
    return NULL;
  };
/*
 * Command and function definitions aren't stored as statements, so
 * iterate until the first statement object is found, or an undataclassifiable
 * statement is encountered.
 */
  do {
/*
 * Locate the start of the statement.
 */
    if(input_skip_white(stream, 1, 0))
      return NULL;
/*
 * All statements begin with an identifier.
 */
    if(input_keyword(stream, 0, 0)) {
      input_error(stream, 1, "Invalid statement.\n");
      return NULL;
    };
/*
 * See if the symbol is known in the current stack of scopes.
 */
    symbol = find_ScriptSymbol(sc, stream, stream->work);
    if(!symbol)
      return NULL;
/*
 * Determine what type of statement to allocate and determine
 * whether the statement is one of the restricted ones.
 */
    switch(symbol->code) {
    case SYM_COMMAND_KEYWORD:	/* A command definition statement */
      restricted = 1;
      type = STMT_EMPTY;
      break;
    case SYM_FUNCTION_KEYWORD:	/* A function definition statement */
      restricted = 1;
      type = STMT_EMPTY;
      break;
    case SYM_GROUP_KEYWORD:     /* A group datatype definition statement */
      restricted = 1;
      type = STMT_EMPTY;
      break;
    case SYM_DATATYPE:		/* A variable definition statement */
    case SYM_LISTOF_KEYWORD:
    case SYM_VARIABLE:
      restricted = 1;
      type = STMT_VAR;
      break;
    case SYM_FOREACH_KEYWORD:	/* A foreach loop-statement */
      restricted = 1;
      type = STMT_FOREACH;
      break;
    case SYM_DO_KEYWORD:	/* A do-loop statement */
      restricted = 1;
      type = STMT_DO;
      break;
    case SYM_UNTIL_KEYWORD:	/* An until barrier-statement */
      restricted = 1;
      type = STMT_UNTIL;
      break;
    case SYM_WHILE_KEYWORD:	/* A while-loop statement */
      restricted = 1;
      type = STMT_WHILE;
      break;
    case SYM_COMMAND:	        /* A command-invokation statement */
      restricted = !((Command *)symbol->data)->is_builtin;
      type = STMT_CALL;
      break;
    case SYM_PRINT_KEYWORD:	/* A print statement */
      restricted = 0;
      type = STMT_PRINT;
      break;
    case SYM_LOG_KEYWORD:	/* A log statement */
      restricted = 0;
      type = STMT_LOG;
      break;
    case SYM_IF_KEYWORD:	/* A conditional-if statement */
      restricted = 1;
      type = STMT_IF;
      break;
    case SYM_BREAK_KEYWORD:
      restricted = 1;
      type = STMT_BREAK;
      break;
    case SYM_NEXT_KEYWORD:
      restricted = 1;
      type = STMT_NEXT;
      break;
    case SYM_EXIT_KEYWORD:
      restricted = 1;
      type = STMT_EXIT;
      break;
    case SYM_IMPORT_KEYWORD:
      restricted = 1;
      type = STMT_EMPTY;
      break;
    case SYM_IFHOST_KEYWORD:
      restricted = 1;
      type = STMT_IFHOST;
      break;
    case SYM_CLEANUP_KEYWORD:
      restricted = 1;
      type = STMT_EMPTY;
      break;
    case SYM_RETURN_KEYWORD:
      restricted = 1;
      type = STMT_RETURN;
      break;
    case SYM_CATCH_KEYWORD:
      restricted = 1;
      type = STMT_CATCH;
      break;
    default:
      input_error(stream, 1, "Unexpected use of '%s' as a statement.\n",
		  stream->work);
      return NULL;
      break;
    };
/*
 * Disallow restricted statements in interactive mode.
 */
    if(restricted && interactive) {
      input_error(stream, 1,
		  "The %s symbol is not available for interactive use.\n",
		  symbol->name);
      return NULL;
    };
/*
 * Allocate the statement container and push the statement onto
 * the compile stack.
 */
    if(type != STMT_EMPTY) {
      stmt = new_Statement(sc, type);
      if(!stmt || !push_CompStatement(sc, stmt))
	return NULL;
    };
/*
 * Parse the statement.
 */
    switch(symbol->code) {
    case SYM_COMMAND_KEYWORD:	/* A command definition statement */
      if(!add_UserCommand(sc, stream))
	return NULL;
      break;
    case SYM_FUNCTION_KEYWORD:	/* A function definition statement */
      if(!add_UserFunction(sc, stream))
	return NULL;
      break;
    case SYM_GROUP_KEYWORD:  /* A group datatype definition statement */
      if(!add_GroupDataType(sc, stream))
	return NULL;
      break;
    case SYM_DATATYPE:		/* A variable definition statement */
    case SYM_LISTOF_KEYWORD:
    case SYM_VARIABLE:
      if(parse_VariableStmt(sc, stream, symbol, &stmt->context.var))
	return NULL;
      break;
    case SYM_FOREACH_KEYWORD:	/* A foreach loop-statement */
      if(parse_ForeachStmt(sc, stream, &stmt->context.foreach))
	return NULL;
      break;
    case SYM_DO_KEYWORD:	/* A do-loop statement */
      if(parse_DoStmt(sc, stream, &stmt->context.do_s))
	return NULL;
      break;
    case SYM_UNTIL_KEYWORD:	/* An until barrier-statement */
      if(parse_UntilStmt(sc, stream, &stmt->context.until))
	return NULL;
      break;
    case SYM_WHILE_KEYWORD:	/* A while-loop statement */
      if(parse_WhileStmt(sc, stream, &stmt->context.while_s))
	return NULL;
      break;
    case SYM_COMMAND:	/* A command-invokation statement */
      if(parse_CallStmt(sc, stream, (Command* )symbol->data, 
			&stmt->context.call))
	return NULL;
      break;
    case SYM_PRINT_KEYWORD:	/* A print or log statement */
    case SYM_LOG_KEYWORD:
      if(parse_PrintStmt(sc, stream, &stmt->context.print))
	return NULL;
      break;
    case SYM_IF_KEYWORD:	/* A conditional-if statement */
      if(parse_IfStmt(sc, stream, &stmt->context.if_s))
	return NULL;
      break;
    case SYM_IFHOST_KEYWORD:
      if(parse_IfhostStmt(sc, stream, &stmt->context.ifhost))
	return NULL;
      break;
    case SYM_BREAK_KEYWORD:
      if(parse_BreakStmt(sc, stream))
	return NULL;
      break;
    case SYM_NEXT_KEYWORD:
      if(parse_NextStmt(sc, stream))
	return NULL;
      break;
    case SYM_EXIT_KEYWORD:
      if(parse_ExitStmt(sc, stream, &stmt->context.exit_s))
	return NULL;
      break;
    case SYM_IMPORT_KEYWORD:
      if(parse_ImportStmt(sc, stream))
	return NULL;
      continue;
    case SYM_CLEANUP_KEYWORD:
      if(parse_CleanupStmt(sc, stream))
	return NULL;
      break;
    case SYM_RETURN_KEYWORD:
      break;
    case SYM_CATCH_KEYWORD:
      if(parse_CatchStmt(sc, stream, &stmt->context.catchstmt))
	return NULL;
      break;
    default:
      input_error(stream, 1, "Unexpected use of '%s' as a statement.\n",
		  stream->work);
      return NULL;
      break;
    };
/*
 * Remove the statement from the compile stack.
 */
    if(type != STMT_EMPTY && !pop_CompStatement(sc))
      return NULL;
/*
 * Make sure that there are no unexpected characters on the last line of
 * the statement.
 */
    if(input_skip_space(stream, 1, 0))
      return NULL;
    if(stream->nextc != '\n' && stream->nextc != EOF) {
      input_error(stream, 1, "Extra characters follow a valid statement.\n");
      return NULL;
    };
/*
 * Locate the start of the next statement.
 */
    if(input_skip_white(stream, 1, 0))
      return NULL;
/*
 * All statements start with an identifier.
 */
  } while(stmt==NULL && isalpha(stream->nextc));
/*
 * Return the empty statement if no executable statements were
 * found.
 */
  return stmt ? stmt : &empty_stmt;
}

/*.......................................................................
 * Parse a variable-declaration/definition statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 *  symbol       Symbol *   The first symbol of the statement. This
 *                          must either be the listof attribut or a
 *                          a datatype.
 * Input/Output:
 *  stmt   VariableStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_VariableStmt(Script *sc, InputStream *stream,
				     Symbol *symbol, VariableStmt *stmt)
{
  TypeSpec *type;   /* The variable declaration */
/*
 * Is this an assignment to an existing variable?
 */
  if(symbol->code == SYM_VARIABLE) {
    stmt->var = (Variable *) symbol->data;
    type = stmt->var->type;
/*
 * Prevent assignment to constants.
 */
    if(((Variable *)symbol->data)->flags & VAR_IS_CONST) {
      input_error(stream, 1, "Illegal attempt to change the constant \'%s\'.\n",
		  symbol->name);
      return 1;
    };
  } else {
/*
 * Parse the declaration part of the statement.
 */
    type = parse_TypeSpec(sc, stream, symbol);
    if(!type)
      return 1;
/*
 * Create the named variable.
 * Note that we don't want the variable to be installed in a symbol
 * table until after the assignment expression has been parsed. This
 * prevents the variable from being used before it has been initialized.
 */
    stmt->var = new_Variable(sc, type);
    if(!stmt->var)
      return 1;
  };
/*
 * All variables are initialized when they are created. The
 * initializing assignment expression is separated from the
 * variable declaration by an = character.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != '=')
    return input_error(stream, 1, "Missing = in an assignment.\n");
  if(input_skip_white(stream, 1, 1))
    return 1;
/*
 * Create an assignment expression.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
/*
 * Now parse the assignment expression.
 */
  if(parse_argument(sc, type, stream, stmt->expr))
    return 1;
/*
 * Now add the variable to the local scope.
 */
  if(symbol->code != SYM_VARIABLE &&
     !add_ScriptSymbol(sc, type->name, SYM_VARIABLE, stmt->var))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a foreach-loop statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Input/Output:
 *  stmt    ForeachStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_ForeachStmt(Script *sc, InputStream *stream,
				    ForeachStmt *stmt)
{
  TypeSpec *type;   /* The variable declaration */
/*
 * Create a new symbol scope for the loop.
 */
  if(push_Scope(sc))
    return 1;
/*
 * For the sake of clarity, require the loop-variable declaration to
 * be enclosed in parentheses.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != '(')
    return input_error(stream, 1,
		       "Missing ( around a loop-variable declaration.\n");
  if(input_skip_white(stream, 1, 1))
    return 1;
/*
 * Parse the declaration of the loop variable, but don't add it to
 * the local symbol scope yet so as to prevent it from being referenced
 * in the list expression that follows.
 */
  type = parse_TypeSpec(sc, stream, NULL);
  if(!type)
    return 1;
/*
 * Check for and skip the closing paren.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != ')')
    return input_error(stream, 1,
		       "Missing ) after a loop-variable declaration.\n");
  if(input_skip_space(stream, 1, 1))
    return 1;
/*
 * List loop variables are not allowed because that would require
 * the following list expression to be a list of lists, which the
 * language doesn't support.
 */
  if(type->is_list)
    return input_error(stream, 1, "Foreach loop-variables can't be lists.\n");
/*
 * Create the loop variable.
 */
  stmt->var = new_Variable(sc, type);
  if(!stmt->var)
    return 1;
/*
 * Create and parse the list expression. Note that the target type
 * of the expression is a list of the type of the loop variable.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
  if(parse_argument(sc, type->dt->list_reg, stream, stmt->expr))
    return 1;
/*
 * Now add the loop-variable to the local scope of the loop.
 */
  if(!add_ScriptSymbol(sc, type->name, SYM_VARIABLE, stmt->var))
    return 1;
/*
 * Parse the list of statements that constitute the body of the
 * loop.
 */
  stmt->stmts = parse_StatementList(sc, stream, 1);
  if(!stmt->stmts)
    return 1;
/*
 * Destroy the local scope of the loop.
 */
  if(pop_Scope(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a do-loop statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Input/Output:
 *  stmt         DoStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_DoStmt(Script *sc, InputStream *stream, DoStmt *stmt)
{
  TypeSpec *type;   /* The variable declaration */
/*
 * Create a new symbol scope for the loop.
 */
  if(push_Scope(sc))
    return 1;
/*
 * Parse the declaration of the loop variable, but don't add it to
 * the local symbol scope yet so as to prevent it from being referenced
 * in the loop-bound expression that follows.
 */
  type = parse_TypeSpec(sc, stream, NULL);
  if(!type)
    return 1;
/*
 * Does this data-type support do-loop iteration?
 */
  if(!type->dt->incr_dt || !type->dt->iter_fn) {
    return input_error(stream, 1,
		       "%s variables can't be used as do-loop variables.\n",
		       type->dt->name);
  };
/*
 * Check for and skip the following '=' scoperator.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != '=')
    return input_error(stream, 1,
	     "The '=' after the do-loop variable declaration is missing.\n");
  if(input_skip_space(stream, 1, 1))
    return 1;
/*
 * List loop variables are not allowed.
 */
  if(type->is_list)
    return input_error(stream, 1, "Do-loop variables can't be lists.\n");
/*
 * Create the loop variable.
 */
  stmt->var = new_Variable(sc, type);
  if(!stmt->var)
    return 1;
  stmt->iter_fn = type->dt->iter_fn;
  stmt->iteration = 0;
  stmt->nstep = 0;
/*
 * Create and parse the three arguments of the do-loop. The first
 * and second represent the bounds of the loop variable, and the
 * third represents the loop increment.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
  if(parse_argument(sc, type->dt->atom_reg, stream, stmt->expr) ||
     input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != ',') {
    return input_error(stream, 1,
		       "Missing comma after do-loop starting value.\n");
  };
  if(input_skip_white(stream, 1, 1) ||
     parse_argument(sc, type->dt->atom_reg, stream, stmt->expr) ||
     input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != ',') {
    return input_error(stream, 1,
		       "Missing comma after do-loop end value.\n");
  };
  if(input_skip_white(stream, 1, 1) ||
     parse_argument(sc, type->dt->incr_dt->atom_reg, stream, stmt->expr) ||
     input_skip_space(stream, 1, 0))
    return 1;
/*
 * Now add the loop-variable to the local scope of the loop.
 */
  if(!add_ScriptSymbol(sc, type->name, SYM_VARIABLE, stmt->var))
    return 1;
/*
 * Parse the list of statements that constitute the body of the
 * loop.
 */
  stmt->stmts = parse_StatementList(sc, stream, 1);
  if(!stmt->stmts)
    return 1;
/*
 * Destroy the local scope of the loop.
 */
  if(pop_Scope(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse an until-loop statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Input/Output:
 *  stmt      UntilStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_UntilStmt(Script *sc, InputStream *stream,
				  UntilStmt *stmt)
{
/*
 * Create and parse the boolean expression.
 */
  stmt->clause = new_Expr(sc);
  if(!stmt->clause)
    return 1;
  if(parse_argument(sc, sc->builtin.boolvar_dt->atom_reg, stream, stmt->clause))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a while-loop statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Input/Output:
 *  stmt      WhileStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_WhileStmt(Script *sc, InputStream *stream,
				  WhileStmt *stmt)
{
/*
 * Create a new symbol scope for the loop.
 */
  if(push_Scope(sc))
    return 1;
/*
 * Require the loop condition to  be enclosed in parentheses.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != '(')
    return input_error(stream, 1, "Missing ( around a loop-condition.\n");
  if(input_skip_white(stream, 1, 1))
    return 1;
/*
 * Create and parse the boolean expression.
 */
  stmt->clause = new_Expr(sc);
  if(!stmt->clause)
    return 1;
  if(parse_argument(sc, sc->builtin.boolvar_dt->atom_reg, stream, stmt->clause))
    return 1;
/*
 * Check for and skip the closing paren.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != ')')
    return input_error(stream, 1, "Missing ) after a loop-condition.\n");
  if(input_skip_space(stream, 1, 1))
    return 1;
/*
 * Parse the list of statements that constitute the body of the
 * loop.
 */
  stmt->stmts = parse_StatementList(sc, stream, 1);
  if(!stmt->stmts)
    return 1;
/*
 * Destroy the local scope of the loop.
 */
  if(pop_Scope(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a command-invokation statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 *  cmd         Command *   The procedure being called.
 *  stmt       CallStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_CallStmt(Script *sc, InputStream *stream, Command *cmd,
			  CallStmt *stmt)
{
/*
 * Record the named command.
 */
  stmt->cmd = cmd;
/*
 * Create a container for compiling argument expressions.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
/*
 * Parse the argument list.
 */
  if(parse_procedure_arguments(sc, cmd->name, 0, cmd->args, cmd->mods, cmd->opt,
			       stream, stmt->expr))
    return 1;
/*
 * All the expected arguments have been read.
 * Let parse_Statement() check for unexpected trailing characters.
 */
  return 0;
}

/*.......................................................................
 * Parse a print statement. The print command can not be implemented
 * as a normal command because it takes a variable number of untyped
 * arguments.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Input/Output:
 *  stmt      PrintStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_PrintStmt(Script *sc, InputStream *stream,
				  PrintStmt *stmt)
{
  int iarg;   /* The ordinal number of the argument being printed */
/*
 * Create a container for compiling argument expressions.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
  stmt->narg = 0;
/*
 * Locate the start of the argument list (or end of line if there isn't one).
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * Parse one or more argument expressions. These expressions are
 * restricted to function calls, variable references or expressions
 * in which the type of the expression is indicated by a cast of the
 * form datatype(expression).
 */
  for(iarg=1; ; iarg++) {
/*
 * A function call, variable reference or cast expression?
 */
    switch(stream->nextc) {
    case '$':
      if(parse_dollar_expr(sc, NULL, 0, stream, stmt->expr) == NULL)
	return 1;
      break;
/*
 * String expressions can be recognized unambiguously.
 */
    case '"':
      if(parse_argument(sc, sc->builtin.string_dt->atom_reg, stream,
			stmt->expr))
	return 1;
      break;
/*
 * An undataclassifiable argument.
 */
    default:
      return input_error(stream, 1,
	  "Unable to guess the type of argument %d of a print command.\n",
	  iarg);
      break;
    };
/*
 * Count arguments.
 */
    stmt->narg++;
/*
 * If there are any further arguments then there should be a comma
 * separating the current argument from the next. If found, skip it
 * and advance to the start of the next argument. Otherwise we are done.
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
    if(stream->nextc != ',')     /* No more arguments? */
      break;
    if(input_skip_white(stream, 1, 1))
      return 1;
  };
/*
 * All heralded arguments have been read.
 * Let parse_Statement() check for unexpected trailing characters.
 */
  return 0;
}

/*.......................................................................
 * Parse an if,then,else statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 *  stmt         IfStmt *   The output statement container.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_IfStmt(Script *sc, InputStream *stream, IfStmt *stmt)
{
  int final_else = 0; /* True after a clause-less else has been seen */
/*
 * Allocate the expression that will contain the clauses.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
/*
 * Create the list of clauses/statement nodes.
 */
  stmt->blocks = new_ScriptList(sc);
  if(!stmt->blocks)
    return 1;
/*
 * Parse the first clause/statement_list.
 */
  if(parse_IfNode(sc, stream, stmt, final_else))
    return 1;
/*
 * Parse one or more "else if(boolvar) {statements}" blocks.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  while(stream->nextc == 'e') {
/*
 * Read the else.
 */
    if(input_keyword(stream, 0, 0) || strcmp(stream->work, "else") != 0)
      return input_error(stream, 1, "Missing 'else' statement.\n");
/*
 * See whether this is an "else if(boolvar)" or "else" clause.
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
    if(stream->nextc == '{') {    /* else {statements} */
      final_else = 1;
    } else {                      /* else if(boolvar) {statements }*/
/*
 * Read the "if" identifier.
 */
      if(input_keyword(stream, 0, 0) || strcmp(stream->work, "if") != 0)
	return input_error(stream, 1,
			"Unidentifiable input follows an 'else' statement.\n");
    };
/*
 * Parse the next node in the list of clause/statement_list blocks.
 */
    if(parse_IfNode(sc, stream, stmt, final_else))
      return 1;
/*
 * If a clause-less else has just been parsed, or the next character
 * is not 
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Parse one clause/statement_list block of an if statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 *  stmt         IfStmt *   The parent IF statement.
 *  final_else      int     If true, no clause will be expected.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_IfNode(Script *sc, InputStream *stream, IfStmt *stmt,
			int final_else)
{
  StatementList *stmts;   /* The list of statements in the block */
/*
 * Locate the start of the block.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * Parse the boolean clause expression. If this is the final else
 * clause, fake an always-true clause.
 */
  if(final_else) {
    Variable *t = new_Variable(sc, sc->builtin.boolvar_dt->atom_reg);
    if(!t || add_LoadOper(sc, stmt->expr, t) == NULL)
      return 1;
    BOOL_VARIABLE(t)->boolvar = 1;
  } else {
    if(parse_argument(sc, sc->builtin.boolvar_dt->atom_reg, stream, stmt->expr) ||
       add_SkipOper(sc, stmt->expr) == NULL ||
       input_skip_space(stream, 1, 0))
       return 1;
  };
/*
 * Create a new symbol scope for the statement list.
 */
  if(push_Scope(sc))
    return 1;
/*
 * Parse the statement list and add it to the list of if-blocks.
 */
  stmts = parse_StatementList(sc, stream, 1);
  if(!stmts || !append_ListNode(stmt->blocks, stmts))
    return 1;
/*
 * Destroy the symbol scope of the statement list.
 */
  if(pop_Scope(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a "break" statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_BreakStmt(Script *sc, InputStream *stream)
{
/*
 * Check that there is a loop to associate with the statement.
 */
  if(!find_CompLoopStmt(sc)) {
    return input_error(stream, 1,
		"Unable to find the target-loop of a 'break' statement.\n");
  };
  return 0;
}

/*.......................................................................
 * Parse a "next" statement.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_NextStmt(Script *sc, InputStream *stream)
{
/*
 * Check that there is a loop to associate with the statement.
 */
  if(!find_CompLoopStmt(sc)) {
    return input_error(stream, 1,
		"Unable to find the target-loop of a 'next' statement.\n");
  };
  return 0;
}

/*.......................................................................
 * Parse and obey an "import" command.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_ImportStmt(Script *sc, InputStream *stream)
{
  DataType *dt;         /* The InputFile datatype */
  char *path = NULL;    /* The file-name */
/*
 * Get the InputFile datatype.
 */
  dt = sc_InputFile_dt(sc);
/*
 * Find the file-name argument.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * If the next character is a '$' then require the following name
 * to be that of a constant InputFile argument (non-constant variables
 * don't have values yet, because we haven't evaluated the script yet).
 */
  if(stream->nextc == '$') {
    Symbol *sym;     /* The argument symbol */
    Variable *arg;   /* The argument variable */
/*
 * Skip the '$' character.
 */
    if(read_InputStream(stream, 1))
      return 1;
/*
 * Read the symbol name.
 */
    if(input_keyword(stream, 0, 0))
      return input_error(stream, 1, "Missing variable name after '$'\n");
/*
 * Lookup the variable.
 */
    sym = find_ScriptSymbol(sc, stream, stream->work);
    if(!sym)
      return 1;
/*
 * Make sure that the symbol points to an InputFile constant.
 */
    if(sym->code != SYM_VARIABLE || (arg=(Variable* )sym->data)->type->dt != dt ||
       arg->type->is_list || !(arg->flags & VAR_IS_CONST)) {
      return input_error(stream, 1, "Import filenames must be literal values or constants.\n");
    };
/*
 * Extract the pathname from the variable.
 */
    path = STRING_VARIABLE(arg)->string;
  } else {
/*
 * Parse the file-name directly.
 */
    if(stream->nextc=='"' ?
       input_quoted_string(stream, 0) : 
       input_literal(stream,  0, "([{",  ")]}",  isgraph, ""))
      return input_error(stream, 1, "Missing import file name.\n");
    path = stream->work;
  };
/*
 * Check that nothing follows the path name.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc != '\n' && stream->nextc != EOF) {
    input_error(stream, 1,
		"Extra characters follow a valid import statement.\n");
    return 1;
  };
/*
 * Push the specified file onto the input stream stack.
 */
  if(open_FileInputStream(stream, "", path))
    return 1;
/*
 * Locate the start of the next statement in the modified stream.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse and obey an "ifhost hostname {stmts}" command.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to parse from.
 *  stmt     IfhostStmt *   The container for the statement.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_IfhostStmt(Script *sc, InputStream *stream, IfhostStmt *stmt)
{
  int last = 0;   /* True when a terminating host-less clause is being parsed */
/*
 * There may be more than one host clause.
 */
  do {
/*
 * Find the start of the host name.
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
/*
 * If no hostname is given then this is the last clause and
 * if no other hostname matched, this should be executed.
 */
    if(stream->nextc == '{')
      last = 1;
    else if(input_literal(stream, 0, "", "", isHostName, ""))
      return input_error(stream, 1, "Invalid ifhost hostname argument.\n");
/*
 * If the host name matches the name of the host computer, compile the
 * following statement list.
 */
    if(!stmt->stmts && (last || strcmp(stream->work, sc->host) == 0)) {
/*
 * Create a new symbol scope for the statement list.
 */
      if(push_Scope(sc))
	return 1;
/*
 * Append statements within the block to the current statement list.
 */
      stmt->stmts = parse_StatementList(sc, stream, 1);
      if(!stmt->stmts)
	return 1;
/*
 * Destroy the symbol scope of the statement list.
 */
      if(pop_Scope(sc))
	return 1;
/*
 * If the host-name doesn't match that of the current host computer,
 * skip the statement list of the ifhost clause.
 */
    } else {
      if(skip_StatementList(sc, stream))
	return 1;
    };
  } while(stream->nextc != '\n' && !last);
  return 0;
}

/*.......................................................................
 * Parse an "exit" command.
 *
 * Input:
 *  sc               Script *   The host scripting environment.
 *  stream      InputStream *   The stream to parse from.
 *  stmt           ExitStmt *   The container for the statement.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_ExitStmt(Script *sc, InputStream *stream, ExitStmt *stmt)
{
/*
 * Is there a "reason-for-exit" argument.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * If there isn't an argument, leave stmt->expr NULL.
 */
  if(stream->nextc == '\n')
    return 0;
/*
 * Allocate an expression container.
 */
  stmt->expr = new_Expr(sc);
  if(!stmt->expr)
    return 1;
/*
 * Parse the argument as a string.
 */
  if(parse_argument(sc, sc->builtin.string_dt->atom_reg, stream, stmt->expr))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a "cleanup {statements}" command.
 *
 * Input:
 *  sc               Script *   The host scripting environment.
 *  stream      InputStream *   The stream to parse from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_CleanupStmt(Script *sc, InputStream *stream)
{
  TypeSpec *ts;   /* The type specification of the "reason" variable */
/*
 * There can only be one cleanup handler per script.
 */
  if(sc->script.cleanup.stmts) {
    return input_error(stream, 1,
		       "There can only be one cleanup statement per script.\n");
  };
/*
 * Find the start of the comamnd name.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
/*
 * Create a new symbol scope for the statement list.
 */
  if(push_Scope(sc))
    return 1;
/*
 * Add a variable to be used to pass the reason for the
 * script ending.
 */
  if((ts = new_TypeSpec(sc, "reason", sc_String_dt(sc), 0)) == NULL ||
     (sc->script.cleanup.reason = new_Variable(sc, ts)) == NULL ||
     !add_ScriptSymbol(sc, "reason", SYM_VARIABLE, sc->script.cleanup.reason))
    return 1;
/*
 * Append statements within the block to the current statement list.
 */
  sc->script.cleanup.stmts = parse_StatementList(sc, stream, 1);
  if(!sc->script.cleanup.stmts)
    return 1;
/*
 * Destroy the symbol scope of the statement list.
 */
  if(pop_Scope(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Parse a "catch(clause) {statements} [{response}]" command.
 *
 * Input:
 *  sc               Script *   The host scripting environment.
 *  stream      InputStream *   The stream to parse from.
 *  stmt          CatchStmt *   The statement to parse into.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int parse_CatchStmt(Script *sc, InputStream *stream, CatchStmt *stmt)
{
/*
 * Find the start of the catch clause.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
/*
 * Create and parse the clause.
 */
  stmt->clause = new_Expr(sc);
  if(!stmt->clause)
    return 1;
  if(parse_argument(sc, sc->builtin.boolvar_dt->atom_reg, stream, stmt->clause))
    return 1;
/*
 * Create a symbol scope for the catch block.
 */
  if(push_Scope(sc))
    return 1;
/*
 * Parse the list of statements that constitute the body of the
 * loop.
 */
  stmt->stmts = parse_StatementList(sc, stream, 1);
  if(!stmt->stmts)
    return 1;
/*
 * Destroy the local scope of the loop.
 */
  if(pop_Scope(sc))
    return 1;
/*
 * See if there is a list of statements to execute when the catch clause
 * becomes true.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  if(stream->nextc == '{') {
    if(push_Scope(sc))
      return 1;
    stmt->action = parse_StatementList(sc, stream, 1);
    if(!stmt->action)
      return 1;
    if(pop_Scope(sc))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Create an empty statement container.
 *
 * Input:
 *  sc        Script *  The host script environment.
 *  type    StmtType    The type of statement to be contained.
 * Output:
 *  return Statement *  The new container, or NULL on error.
 */
static Statement *new_Statement(Script *sc, StmtType type)
{
  Statement *stmt;   /* The new container */
/*
 * Allocate the container.
 */
  stmt = (Statement* )new_ScriptObject(sc, sc->memory.statement, 0);
  if(!stmt)
    return NULL;
/*
 * Clear the container according to its type.
 */
  stmt->type = type;
  stmt->loop = NULL;
  switch(type) {
  case STMT_EMPTY:
    break;
  case STMT_VAR:
    stmt->context.var.var = NULL;
    stmt->context.var.expr = NULL;
    break;
  case STMT_FOREACH:
    stmt->loop = new_LoopStateList(sc);
    if(!stmt->loop)
      return NULL;
    stmt->context.foreach.var = NULL;
    stmt->context.foreach.expr = NULL;
    stmt->context.foreach.stmts = NULL;
    stmt->context.foreach.next_node = NULL;
    break;
  case STMT_DO:
    stmt->loop = new_LoopStateList(sc);
    if(!stmt->loop)
      return NULL;
    stmt->context.do_s.var = NULL;
    stmt->context.do_s.iter_fn = NULL;
    stmt->context.do_s.expr = NULL;
    stmt->context.do_s.stmts = NULL;
    stmt->context.do_s.iteration = 0;
    stmt->context.do_s.first = NULL;
    stmt->context.do_s.last = NULL;
    stmt->context.do_s.step = NULL;
    stmt->context.do_s.nstep = 0;
    break;
  case STMT_UNTIL:
    stmt->loop = new_LoopStateList(sc);
    if(!stmt->loop)
      return NULL;
    stmt->context.until.clause = NULL;
    break;
  case STMT_WHILE:
    stmt->loop = new_LoopStateList(sc);
    if(!stmt->loop)
      return NULL;
    stmt->context.while_s.clause = NULL;
    stmt->context.while_s.stmts = NULL;
    break;
  case STMT_CALL:
    stmt->context.call.cmd = NULL;
    stmt->context.call.expr = NULL;
    break;
  case STMT_PRINT:
  case STMT_LOG:
    stmt->context.print.expr = NULL;
    stmt->context.print.narg = 0;
    break;
  case STMT_IF:
    stmt->context.if_s.expr = NULL;
    stmt->context.if_s.blocks = NULL;
    break;
  case STMT_IFHOST:
    stmt->context.ifhost.stmts = NULL;
    break;
  case STMT_BREAK:
  case STMT_NEXT:
    break;
  case STMT_EXIT:
    stmt->context.exit_s.expr = NULL;
    break;
  case STMT_RETURN:
    break;
  case STMT_CATCH:
    stmt->loop = new_LoopStateList(sc);
    if(!stmt->loop)
      return NULL;
    stmt->context.catchstmt.clause = NULL;
    stmt->context.catchstmt.stmts = NULL;
    stmt->context.catchstmt.action = NULL;
    stmt->context.catchstmt.node = NULL;
    break;
  default:
    lprintf(stderr, "new_Statement: Unknown type requested.\n");
    return NULL;
    break;
  };
  return stmt;
}

/*.......................................................................
 * Create a statement-list container.
 *
 * Input:
 *  sc            Script *  The host script environment.
 * Output:
 *  return StatementList *  The new list, or NULL on error.
 */
static StatementList *new_StatementList(Script *sc)
{
  return new_ScriptList(sc);
}

/*.......................................................................
 * Append a statement to a statement list.
 *
 * Input:
 *  sc          Script *   The host script environment.
 *  sl   StatementList *   The list to append to.
 *  stmt     Statement *   The statement to append.
 * Output:
 *  return   Statement *   The same as 'stmt', or NULL on error.
 */
static Statement *append_Statement(Script *sc, StatementList *sl,
				   Statement *stmt)
{
  if(append_ListNode(sl, stmt) == NULL)
    return NULL;
  return stmt;
}

/*.......................................................................
 * Push a new node onto the stack of executing statements.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  stmt     Statement *  The statement that should occupy the node.
 *                        This will be the next statement to be executed
 *                        at the scope level of the new node.
 *  stmt_node ListNode *  If the statement is part of a statement list
 *                        this should point to the list node that
 *                        contains stmt. Otherwise pass NULL.
 * Output:
 *  return    ExeFrame *  The new stack node, or NULL on error.
 */
ExeFrame *push_ExeFrame(Script *sc, Statement *stmt, ListNode *stmt_node)
{
/*
 * Allocate the new stack entry.
 */
  ExeFrame *frame = (ExeFrame* )new_FreeListNode("push_ExeFrame", sc->memory.exeframe);
  if(!frame)
    return NULL;
/*
 * Initialize the new entry.
 */
  frame->parent = top_ExeFrame(sc);
  frame->stmt = stmt;
  frame->stmt_node = stmt_node;
  frame->whatnext = EXE_STMT_START;
  sc->script.exe_stack = frame;
  return frame;
}

/*.......................................................................
 * Remove and discard the top node of the stack of executing statements.
 * Return the statement that it refered to.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 * Output:
 *  return   Statement *  The statement that was contained in the deleted
 *                        node, or NULL on error.
 */
Statement *pop_ExeFrame(Script *sc)
{
  Statement *stmt;   /* The node that has been displaced */
/*
 * Get the top node of the stack.
 */
  ExeFrame *top = sc->script.exe_stack;
  if(!top) {
    lprintf(stderr, "pop_ExeFrame: Stack already empty.\n");
    return NULL;
  };
/*
 * End the statement that was being executed.
 */
  end_ExeFrame_Statement(sc, top);
/*
 * Replace the top node with its parent.
 */
  sc->script.exe_stack = top->parent;
/*
 * Keep a record of the statement that is being displaced.
 */
  stmt = top->stmt;
/*
 * Discard the redundant node.
 */
  del_FreeListNode("pop_ExeFrame", sc->memory.exeframe, top);
  return stmt;
}

/*.......................................................................
 * Return the statement that is currently at the top of the execution-
 * frame stack.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 * Output:
 *  return   ExeFrame *  The statement node at the top of the stack.
 */
static ExeFrame *top_ExeFrame(Script *sc)
{
  return sc->script.exe_stack;
}

/*.......................................................................
 * Arrange for the next call to step_script() to re-execute the currently
 * executing statement.
 *
 * Input:
 *  sc         Script *   The host scripting environment.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int requeue_ExeFrame_Statement(Script *sc)
{
/*
 * Get the top node of the stack.
 */
  ExeFrame *top = sc->script.exe_stack;
  if(!top) {
    lprintf(stderr, "requeue_ExeFrame: Stack empty.\n");
    return 1;
  };
  top->whatnext = EXE_STMT_REPEAT;
  return 0;
}

/*.......................................................................
 * Return the next executable statement to be run.
 *
 * Input:
 *  sc         Script *   The host scripting environment.
 * Output:
 *  return  Statement *   The statement to be run next, or NULL if the
 *                        end of the script has been reached.
 */
static Statement *step_ExeFrame(Script *sc)
{
  ExeFrame *top;   /* The top of the stack of executing frames */
/*
 * Traverse the statement lists of the current and parent execution
 * frames until a new statement is found.
 */
  for(top=top_ExeFrame(sc); top && top->whatnext==EXE_STMT_FINISH;
      top=top_ExeFrame(sc)) {
/*
 * Cleanup the completed statement.
 */
    end_ExeFrame_Statement(sc, top);
/*
 * Are there any remaining statements in the current frame?
 */
    if(top->stmt_node && top->stmt_node->next) {
/*
 * Prepare the next statement.
 */
      top->stmt_node = top->stmt_node->next;
      top->stmt = (Statement* )top->stmt_node->data;
      top->whatnext = EXE_STMT_START;
/*
 * If there are no further statements in the current frame, discard it
 * and continue the search in its parent frame.
 */
    } else {
      pop_ExeFrame(sc);
    };
  };
/*
 * No more statements?
 */
  if(!top)
    return NULL;
/*
 * Start executing a new statement?
 */
  switch(top->whatnext) {
  case EXE_STMT_START:
/*
 * Initialize any execution state of the statement.
 */
    switch(top->stmt->type) {
    case STMT_FOREACH:
      top->stmt->context.foreach.next_node = NULL;
      break;
    case STMT_DO:
      top->stmt->context.do_s.iteration = 0;
      break;
    case STMT_CATCH:
      {
	CatchStmt *catchstmt = &top->stmt->context.catchstmt;
	(void) del_ListNode(sc->script.catch_list, catchstmt->node, NULL);
	catchstmt->node = NULL;
      };
      break;
    default:
      break;
    };
/*
 * If the statement contains a loop, initialize its loop-state objects.
 */
    if(top->stmt->loop)
      update_LoopState(sc, top->stmt, LOOP_ENTER);
    break;
/*
 * Repeat the previously executed statement of the executing frame.
 */
  case EXE_STMT_REPEAT:
/*
 * Update any associated loop-state objects.
 */
    if(top->stmt->loop)
      update_LoopState(sc, top->stmt, LOOP_INCR);
    break;
  default:
    lprintf(stderr, "step_ExeFrame: Unknown whatnext value.\n");
    return NULL;
  };
/*
 * Set the statement to terminate after this iteration, unless otherwise
 * specified by an intervening call to requeue_ExeFrame_Statement().
 */
  top->whatnext = EXE_STMT_FINISH;
/*
 * Return the statement for execution.
 */
  return top->stmt;
}

/*.......................................................................
 * This function is called whenever a statement completes or is aborted.
 * It resets any associated execution state.
 *
 * Input:
 *  sc        Script *   The host scripting environment. 
 *  frame   ExeFrame *   The execution frame that contains the statement.
 */
static void end_ExeFrame_Statement(Script *sc, ExeFrame *frame)
{
  Statement *stmt = frame->stmt;
/*
 * Do nothing unless the execution state of the statement has been initialized.
 */
  if(stmt && frame->whatnext!=EXE_STMT_START) {
    switch(stmt->type) {
    case STMT_FOREACH:
      stmt->context.foreach.next_node = NULL; /* Discard the expression list */
      break;
    case STMT_DO:
      stmt->context.do_s.iteration = 0;       /* Mark all iterations as done */
      break;
    case STMT_CATCH:
      {
	CatchStmt *catchstmt = &stmt->context.catchstmt;
	(void) del_ListNode(sc->script.catch_list, catchstmt->node, NULL);
	catchstmt->node = NULL;
      };
      break;
    default:
      break;
    };
/*
 * If the statement contains a loop, cleanup its loop-state objects.
 */
    if(stmt->loop)
      update_LoopState(sc, stmt, LOOP_EXIT);
  };
}

/*.......................................................................
 * Create a free-list from which to allocate Statement objects.
 */
StatementMem *new_StatementMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(Statement));
}

/*.......................................................................
 * Create a free-list from which to allocate LoopState objects.
 */
LoopStateMem *new_LoopStateMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(LoopState));
}

/*.......................................................................
 * During compilation of an expression this function should be called
 * whenever a loop-state function is encountered. It gets the loop state
 * object of the loop-state function from the innermost enclosing loop.
 * If the function hasn't been registered before to that loop, then a
 * new loop-state object is created and returned. If there is no enclosing
 * loop, or some other error occurs, NULL is returned.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  stream  InputStream *  The parser input stream.
 *  func       Function *  The loop-state function to register.
 * Output:
 *  return    LoopState *  The state object associated with 'func', or
 *                         NULL on error.
 */
static LoopState *get_LoopState(Script *sc, InputStream *stream, Function *func)
{
  LOOP_FN(*loop_fn);  /* The loop-state control function */
  Statement *stmt;    /* The associated loop statement */
  ListNode *node;     /* A node of stmt->loop */
  LoopState *state;   /* The state object to be returned */
/*
 * Locate the innermost enclosing loop statement.
 */
  stmt = find_CompLoopStmt(sc);
  if(!stmt) {
    input_error(stream, 1,
		"Unable to find the target loop of a loop-state function.\n");
    return NULL;
  };
/*
 * Get the loop-state function.
 */
  loop_fn = func->is_builtin ? func->body.builtin.loop_fn : NULL;
  if(!loop_fn) {
    lprintf(stderr, "get_LoopState: Not a loop-state function.\n");
    return NULL;
  };
/*
 * See if the function has been registered before to the current statement.
 */
  for(node=stmt->loop->head; node; node=node->next) {
    state = (LoopState* )node->data;
    if(state->loop_fn == loop_fn)
      return state;
  };
/*
 * The function hasn't been registered before, so create and append
 * a new loop-state node for it.
 */
  state = (LoopState* )new_ScriptObject(sc, sc->memory.loopstate, 0);
  if(!state || !append_ListNode(stmt->loop, state))
    return NULL;
  state->loop_fn = loop_fn;
  state->state = new_Variable(sc, func->body.builtin.loop_type);
  return state->state ? state : NULL;
}

/*.......................................................................
 * During compilation return the state object associated with a given
 * loop-state function and its nearest enclosing loop.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stream   InputStream *  The parser input stream.
 *  func        Function *  The loop-state function to register.
 * Output:
 *  return      Variable *  The state-value variable associated with 'func',
 *                          or NULL on error.
 */
Variable *get_loop_data(Script *sc, InputStream *stream, Function *func)
{
  LoopState *state = get_LoopState(sc, stream, func);
  return state ? state->state : NULL;
}

/*.......................................................................
 * Initialize, iterate or cleanup the loop-state objects of a given
 * loop statement.
 *
 * Input:
 *  sc       Script *  The host scripting environment.
 *  stmt  Statement *  The loop statement being entered, iterated or
 *                     exited.
 *  oper   LoopOper    The operation to perform.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
static int update_LoopState(Script *sc, Statement *stmt, LoopOper oper)
{
  ListNode *node;       /* A node of stmt->loop */
/*
 * Make sure that the statement is a loop.
 */
  if(!stmt->loop) {
    lprintf(stderr, "update_LoopState: Not a loop statement.\n");
    return 1;
  };
/*
 * Update each state object in the list.
 */
  for(node=stmt->loop->head; node; node=node->next) {
    LoopState *state = (LoopState* )node->data;
    state->loop_fn(sc, state->state, oper);
  };
  return 0;
}

/*.......................................................................
 * Create an empty list of loop-state objects for a statement.
 *
 * Input:
 *  sc             Script *  The host scripting environment.
 * Output:
 *  return  LoopStateList *  The new list, or NULL on error.
 */
static LoopStateList *new_LoopStateList(Script *sc)
{
  return new_ScriptList(sc);
}

/*.......................................................................
 * Create a free-list from which to allocate ExeFrame objects.
 */
ExeFrameMem *new_ExeFrameMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(ExeFrame));
}

/*.......................................................................
 * Execute the next statement of an active script.
 *
 * Input:
 *  sc            Script *   The host scripting environment.
 *  log     OutputStream *   If you want commands to be logged before they
 *                           are executed, specify an open logging stream
 *                           here. This will also be used by the "log"
 *                           command.
 * Output:
 *  return   ScriptState     The execution state of the script, from:
 *                             SCRIPT_EMPTY  - No script has been compiled.
 *                             SCRIPT_ACTIVE - A script is still in the
 *                                             process of being executed.
 *                             SCRIPT_ERROR  - An error occured.
 *                             SCRIPT_END    - An executing script
 *                                             completed.
 */
ScriptState step_script(Script *sc, OutputStream *log)
{
  Statement *stmt;  /* top->stmt */
/*
 * Check arguments.
 */
  if(!sc) {
    lprintf(stderr, "step_script: Invalid argument(s).\n");
    return SCRIPT_ERROR;
  };
/*
 * Check the script execution status.
 */
  switch(sc->script.state) {
  case SCRIPT_EMPTY:
    lprintf(stderr, "step_script: There is no program to run.\n");
    return SCRIPT_EMPTY;
    break;
  case SCRIPT_ACTIVE:
  case SCRIPT_EXITING:
    break;
  case SCRIPT_ERROR:
  case SCRIPT_END:
    return sc->script.state;
    break;
  };
/*
 * Find the next statement to be run.
 */
  stmt = step_ExeFrame(sc);
  if(!stmt)
    return exit_script(sc, "end");
  
  // Execute the statement according to its type.

  switch(stmt->type) {
  case STMT_EMPTY:
    lprintf(stderr, "step_script: Empty statement unexpected.\n");
    return bad_script(sc);
    break;
  case STMT_VAR:
    if(exe_VariableStmt(sc, &stmt->context.var))
      return bad_script(sc);
    break;
  case STMT_FOREACH:
    if(exe_ForeachStmt(sc, &stmt->context.foreach))
      return bad_script(sc);
    break;
  case STMT_DO:
    if(exe_DoStmt(sc, &stmt->context.do_s))
      return bad_script(sc);
    break;
  case STMT_UNTIL:
    if(exe_UntilStmt(sc, &stmt->context.until))
      return bad_script(sc);
    break;
  case STMT_WHILE:
    if(exe_WhileStmt(sc, &stmt->context.while_s))
      return bad_script(sc);
    break;
  case STMT_CALL:
    if(exe_CallStmt(sc, log, &stmt->context.call))
      return bad_script(sc);
    break;
  case STMT_PRINT:
    if(exe_PrintStmt(sc, sc->output, &stmt->context.print))
      return bad_script(sc);
    break;
  case STMT_LOG:
    log->interactive = sc->interactive_;
    if(exe_PrintStmt(sc, log, &stmt->context.print))
      return bad_script(sc);
    log->interactive = false;
    break;
  case STMT_IF:
    if(exe_IfStmt(sc, &stmt->context.if_s))
      return bad_script(sc);
    break;
  case STMT_IFHOST:
    if(exe_IfhostStmt(sc, &stmt->context.ifhost))
      return bad_script(sc);
    break;
  case STMT_BREAK:
    if(exe_BreakStmt(sc))
      return bad_script(sc);
    break;
  case STMT_NEXT:
    if(exe_NextStmt(sc))
      return bad_script(sc);
    break;
  case STMT_EXIT:
    if(exe_ExitStmt(sc, &stmt->context.exit_s))
      return bad_script(sc);
    break;
  case STMT_RETURN:
    if(exe_ReturnStmt(sc))
      return bad_script(sc);
    break;
  case STMT_CATCH:
    if(exe_CatchStmt(sc, &stmt->context.catchstmt))
      return bad_script(sc);
    break;
  default:
    lprintf(stderr, "new_Statement: Unknown type requested.\n");
    return bad_script(sc);
    break;
  };
  return sc->script.state;
}

/*.......................................................................
 * This is the private error-return function of step_script().  It
 * marks the script as having suffered an error and returns this code
 * for return by step_script().
 */
static ScriptState bad_script(Script *sc)
{
  return sc->script.state=SCRIPT_ERROR;
}

/*.......................................................................
 * Cause a script to exit as though an exit statement had been executed.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  reason        char *  The reason for exiting. This string will be
 *                        copied and passed to an exit handler.
 * Output:
 *  return ScriptState    The status of the script. The following values
 *                        are returned when successful:
 *                         SCRIPT_EXITING - An exit handler is now being
 *                                          executed. Call step_script()
 *                                          until it returns SCRIPT_END.
 *                         SCRIPT_END     - The script has terminated.
 */
ScriptState exit_script(Script *sc, char *reason)
{
  if(!sc || !reason) {
    lprintf(stderr, "exit_script: NULL argument(s).\n");
    return bad_script(sc);
  };
/*
 * If any cleanup code was specified by the user and the script is in an
 * appropriate state, execute this code.
 */
  if(sc->script.cleanup.stmts && sc->script.state==SCRIPT_ACTIVE) {
/*
 * Attempt to allocate a copy of the "reason" string.
 */
    char *reason_string = new_ScriptString(sc, reason);
    if(!reason_string)
      return bad_script(sc);
/*
 * Clear the call stack.
 */
    while(sc->script.exe_stack)
      pop_ExeFrame(sc);
/*
 * Assign the reason string for use by the handler.
 */
    STRING_VARIABLE(sc->script.cleanup.reason)->string = reason_string;
/*
 * Stage the user's statements to be run.
 */
    {
      ListNode *head = sc->script.cleanup.stmts->head;
      if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
	return bad_script(sc);
    };
    return sc->script.state=SCRIPT_EXITING;
  };
  return sc->script.state=SCRIPT_END;
}

/*.......................................................................
 * Return true if the script is polling an until statement.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 * Output:
 *  return         int    0 - Not polling.
 *                        1 - Polling.
 */
int script_is_polling(Script *sc)
{
  ExeFrame *top;   /* The top of the stack of executing frames */
/*
 * Get the latest execution frame.
 */
  top = top_ExeFrame(sc);
  return top && top->stmt->type==STMT_UNTIL && top->whatnext==EXE_STMT_REPEAT;
}

/*.......................................................................
 * Execute a variable-declaration/initialization statement.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  stmt  VariableStmt *  The statement to be executed.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int exe_VariableStmt(Script *sc, VariableStmt *stmt)
{
  Variable *result;   /* The value to give the variable */
/*
 * Evaluate the assignment expression.
 */
  if(exe_Expr(sc, stmt->expr))
    return 1;
/*
 * The only value on the stack should be the value to assign to
 * the variable.
 */
  result = pop_ExprStack(sc, stmt->expr);
  if(!result)
    return 1;
/*
 * Assign the computed value to the variable.
 */
  if(copy_Variable(stmt->var, result) == NULL)
    return 1;
  return 0;
}

/*.......................................................................
 * Execute a foreach statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt     ForeachStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_ForeachStmt(Script *sc, ForeachStmt *stmt)
{
  Variable *var;  /* The list variable or one of its members */
/*
 * First iteration?
 */
  if(!stmt->next_node) {
/*
 * Evaluate the list expression of the foreach statement.
 */
    if(exe_Expr(sc, stmt->expr))
      return 1;
/*
 * Get the list variable who's members are to be assigned one at a time
 * to the loop variable.
 */
    var = pop_ExprStack(sc, stmt->expr);
    if(!var)
      return 1;
/*
 * Schedule the first node of the variable list to be used first.
 */
    stmt->next_node = LIST_VARIABLE(var)->list->head;
  };
/*
 * Get the next variable of the list.
 */
  var = (Variable* )stmt->next_node->data;
/*
 * Assign it to the loop variable.
 */
  copy_Variable(stmt->var, var);
/*
 * Set up for the following iteration of the foreach statement.
 */
  stmt->next_node = stmt->next_node->next;
/*
 * If there are any remaining variables on the foreach list, schedule
 * the following statement to be run again after the current iteration
 * of the foreach loop completes.
 */
  if(stmt->next_node && requeue_ExeFrame_Statement(sc))
    return 1;
/*
 * Schedule the first statement of the body of the loop to be executed next.
 */
  {
    ListNode *head = stmt->stmts->head;
    if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute a do-loop statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt          DoStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_DoStmt(Script *sc, DoStmt *stmt)
{
/*
 * First iteration?
 */
  if(stmt->iteration == 0) {
/*
 * Evaluate the two loop-variable bounds and increment.
 */
    if(exe_Expr(sc, stmt->expr) ||
       !(stmt->step=pop_ExprStack(sc, stmt->expr)) ||
       !(stmt->last=pop_ExprStack(sc, stmt->expr)) ||
       !(stmt->first=pop_ExprStack(sc, stmt->expr)))
      return 1;
/*
 * Ask the loop iterator function to compute the number of iterations needed.
 */
    stmt->nstep = static_cast<int>(stmt->iter_fn(sc, stmt->first, stmt->last, 
						 stmt->step, 0, NULL));
  };
/*
 * Determine the next value of the loop variable.
 */
  if(stmt->iter_fn(sc, stmt->first, stmt->last, stmt->step, stmt->iteration,
		   stmt->var))
    return 1;
/*
 * If there are any iterations remaining to be done, schedule
 * the following statement to be run again after the current iteration
 * of the do-loop completes.
 */
  if(++stmt->iteration < stmt->nstep && requeue_ExeFrame_Statement(sc))
    return 1;
/*
 * Schedule the first statement of the body of the loop to be executed next.
 */
  {
    ListNode *head = stmt->stmts->head;
    if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute an until statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt       UntilStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_UntilStmt(Script *sc, UntilStmt *stmt)
{
  Variable *var;    /* The value of the until clause */
/*
 * Since until statements can delay script execution indefinitely,
 * before each iteration of the until statement, check if any enclosing
 * catch clauses have become true.
 */
  switch(catch_script_events(sc)) {
  case CAUGHT_EVENT:
    return 0;
  case CAUGHT_NOTHING:
    break;
  case CAUGHT_ERROR:
    return 1;
  };
/*
 * Evaluate the clause of the until statement.
 */
  if(exe_Expr(sc, stmt->clause))
    return 1;
/*
 * Get the boolean value of the clause.
 */
  var = pop_ExprStack(sc, stmt->clause);
  if(!var)
    return 1;
/*
 * When the clause evaluates to false, reschedule the until statement
 * to be run again.
 */
  if(!BOOL_VARIABLE(var)->boolvar && requeue_ExeFrame_Statement(sc))
    return 1;
  return 0;
}

/*.......................................................................
 * Execute a while() {} statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt       WhileStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_WhileStmt(Script *sc, WhileStmt *stmt)
{
  Variable *var;    /* The value of the while clause */
/*
 * Evaluate the clause of the while loop.
 */
  if(exe_Expr(sc, stmt->clause))
    return 1;
/*
 * Get the boolean value of the clause.
 */
  var = pop_ExprStack(sc, stmt->clause);
  if(!var)
    return 1;
/*
 * If the clause is true, schedule the statement list of the while-loop
 * to be executed next, and reschedule the while statement to be run
 * again when its statement-list has completed.
 */
  if(BOOL_VARIABLE(var)->boolvar) {
    ListNode *head = stmt->stmts->head;
    if(requeue_ExeFrame_Statement(sc) ||
       (head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute a command-invocation statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  log     OutputStream *  If you want commands to be logged before they
 *                          are executed, specify an open logging stream
 *                          here. This will also be used by the "log"
 *                          command.
 *  stmt        CallStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_CallStmt(Script *sc, OutputStream *log, CallStmt *stmt)
{
  Command *cmd = stmt->cmd;       /* The command to be executed */
/*
 * Evaluate the arguments of the command. Note that this not only
 * evaluates the arguments but also copies them into the cmd->args
 * argument list.
 */
  if(exe_Expr(sc, stmt->expr))
    return 1;
/*
 * Display the command to stdout?
 */
  if(log) {
    if(write_OutputStream(log, cmd->name) ||
       print_ArgumentList(sc, log, 0, cmd->args) ||
       write_OutputStream(log, "\n"))
      return 1;
  };
/*
 * If the command is implemented with a C function execute it
 * in place.
 */
  if(cmd->is_builtin) {
    if(cmd->body.builtin.cmd_fn(sc, cmd->args))
      return 1;
/*
 * If the command is a user-provided list of statements, queue the
 * first of these for execution.
 */
  } else {
    ListNode *head = cmd->body.user.stmts->head;
    if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute a print statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  output  OutputStream *  The stream to write to.
 *  stmt       PrintStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_PrintStmt(Script *sc, OutputStream *output, PrintStmt *stmt)
{
  VariableList *args;   /* The list of arguments to print */
  ListNode *node;       /* A node of the list of arguments */
/*
 * Evaluate the arguments of the statement.
 */
  if(exe_Expr(sc, stmt->expr))
    return 1;
/*
 * Print each result variable that is on the expression stack.
 */
  args = pop_ExprStackArgs(sc, stmt->expr, stmt->narg);
  if(!args)
    return 1;
  for(node=args->head; node; node=node->next) {
    Variable *arg = (Variable* )node->data;
    int is_string = arg->type->dt==sc->builtin.string_dt && !arg->type->is_list;

    if(is_string ? write_OutputStream(output, STRING_VARIABLE(arg)->string) :
                   print_variable(sc, output, arg)) {
      clr_List(args);
      return 1;
    };
  };
/*
 * Discard the argument list.
 */
  clr_List(args);
/*
 * Terminate the output.
 */
  return write_OutputStream(output, "\n");
}

/*.......................................................................
 * Execute a if() {} statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt          IfStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_IfStmt(Script *sc, IfStmt *stmt)
{
  VariableList *args;   /* The list of boolean clause values */
  ListNode *stmt_node;  /* A node of the list of statement lists */
  ListNode *boolvar_node;  /* A node of the list of clause values */
/*
 * Evaluate the clause expressions.
 */
  if(exe_Expr(sc, stmt->expr))
    return 1;
/*
 * Get the list of clauses.
 */
  args = pop_ExprStackArgs(sc, stmt->expr, -1);
  if(!args)
    return 1;
/*
 * Evaluate each of the if-statement clauses until one evaluates to
 * true. If and when this happens record the if node and break out
 * of the loop.
 */
  for(stmt_node = stmt->blocks->head, boolvar_node = args->head;
      stmt_node && boolvar_node;
      stmt_node=stmt_node->next, boolvar_node=boolvar_node->next) {
/*
 * Get the variable that holds the value of the next boolean clause.
 */
    Variable *var = (Variable* )boolvar_node->data;
/*
 * Was the clause satisfied?
 */
    if(BOOL_VARIABLE(var)->boolvar) {
      StatementList *stmts = (StatementList* )stmt_node->data;
      ListNode *head = stmts->head;
      if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL) {
	clr_List(args);
	return 1;
      };
      break;
    };
  };
/*
 * Discard the argument list.
 */
  clr_List(args);
  return 0;
}

/*.......................................................................
 * Execute an ifhost hostname {} statement.
 *
 * Input:
 *  sc            Script *  The host scripting environment.
 *  stmt      IfHostStmt *  The statement to be executed.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int exe_IfhostStmt(Script *sc, IfhostStmt *stmt)
{
  if(stmt->stmts) {
    ListNode *head = stmt->stmts->head;
    if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute a break statement.
 *
 * Input:
 *  sc     Script *  The host scripting environment.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
static int exe_BreakStmt(Script *sc)
{
  ExeFrame *top;    /* The execution frame at the top of the stack */
/*
 * Unwind the execution-frame stack until a breakable statement is
 * found, then mark that loop statement as complete.
 */
  for(top=top_ExeFrame(sc); top; top=top_ExeFrame(sc)) {
    if(top->stmt->loop) {
      top->whatnext = EXE_STMT_FINISH;
      return 0;
    };
/*
 * Cleanup and remove the completed statement from the stack.
 */
    pop_ExeFrame(sc);
  };
/*
 * No breakable frame found.
 */
  lprintf(stderr, "exe_BreakStmt: No target-loop found.\n");
  return 1;
}

/*.......................................................................
 * Execute a next (aka continue) statement.
 *
 * Input:
 *  sc     Script *  The host scripting environment.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
static int exe_NextStmt(Script *sc)
{
  ExeFrame *top;    /* The execution frame at the top of the stack */
/*
 * Unwind the execution-frame stack until a loop statement is
 * found.
 */
  for(top=top_ExeFrame(sc); top; top=top_ExeFrame(sc)) {
    if(top->stmt->loop)
      return 0;
/*
 * Cleanup and remove the completed statement from the stack.
 */
    pop_ExeFrame(sc);
  };
/*
 * No loop found.
 */
  lprintf(stderr, "exe_NextStmt: No target-loop found.\n");
  return 1;
}

/*.......................................................................
 * Execute an "exit" statement.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 *  stmt    ExitStmt *  The statement to be executed.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int exe_ExitStmt(Script *sc, ExitStmt *stmt)
{
  char *reason = "exit";   /* The reason for the exit */
/*
 * If a reason-for-exit argument was provided, evaluate it.
 */
  if(stmt->expr) {
    Variable *arg;   /* The reason-for-exit argument */
    if(exe_Expr(sc, stmt->expr) || !(arg=pop_ExprStack(sc, stmt->expr)))
      return 1;
    reason = STRING_VARIABLE(arg)->string;
  };
/*
 * Ask the script to exit with the given reason.
 */
  switch(exit_script(sc, reason)) {
  case SCRIPT_END:
  case SCRIPT_EXITING:
    break;
  default:
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Execute a "return" statement.
 *
 * Input:
 *  sc        Script *  The host scripting environment.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int exe_ReturnStmt(Script *sc)
{
  ExeFrame *top;    /* The execution frame at the top of the stack */
/*
 * Unwind the execution stack until the innermost enclosing command
 * statement (if any) is found.
 */
  for(top=top_ExeFrame(sc); top && top->stmt->type != STMT_CALL;
      top=top_ExeFrame(sc)) {
/*
 * Cleanup and remove the completed statement from the stack.
 */
    pop_ExeFrame(sc);
  };
/*
 * If an enclosing command statement was found, mark it as finished.
 * Otherwise leave the script to end on the next call to step_script().
 */
  if(top)
    top->whatnext = EXE_STMT_FINISH;
  return 0;
}

/*.......................................................................
 * Execute a "catch" statement.
 *
 * Input:
 *  sc         Script *  The host scripting environment.
 *  stmt    CatchStmt *  The statement to be executed.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int exe_CatchStmt(Script *sc, CatchStmt *stmt)
{
  Variable *var;       /* The evaluated list of signal variables */
  StatementList *stmts=NULL; /* The statements to be executed next */
/*
 * Evaluate the catch clause.
 */
  if(exe_Expr(sc, stmt->clause))
    return 1;
/*
 * Get the boolean result of the clause.
 */
  var = pop_ExprStack(sc, stmt->clause);
  if(!var)
    return 1;
/*
 * If the catch clause is already true, stage the action statements
 * to be run, if any. Otherwise stage the first statement of the
 * enclosed statement list to be run next, and append the catch clause
 * to the list of clauses to be evaluated before each statement.
 */
  if(BOOL_VARIABLE(var)->boolvar) {
    stmts = stmt->action;
  } else {
    stmts = stmt->stmts;
    stmt->node = append_ListNode(sc->script.catch_list, stmt);
    if(!stmt->node)
      return 1;
  };
/*
 * Are there any statements to be run, or should we allow the script
 * to continue with the first statement that follows the catch statement?
 */
  if(stmts) {
    ListNode *head = stmts->head;
    if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Push a statement onto the stack of statements that are being
 * parsed.
 *
 * Input:
 *  sc        Script *   The host scripting environment.
 *  stmt   Statement *   The loop statement.
 * Output:
 *  return Statement *   The same as 'stmt', or NULL on error.
 */
Statement *push_CompStatement(Script *sc, Statement *stmt)
{
  if(prepend_ListNode(sc->script.comp_stack, stmt) == NULL)
    return NULL;
  return stmt;
}

/*.......................................................................
 * Pop the top statement from the stack of statements that are being
 * compiled.
 *
 * Input:
 *  sc        Script *   The host scripting environment.
 * Output:
 *  return Statement *   The liberated statement, or NULL on error.
 */
Statement *pop_CompStatement(Script *sc)
{
/*
 * Is there a statement to be removed?
 */
  if(!sc->script.comp_stack->head) {
    lprintf(stderr, "pop_CompStatement: There is no loop to pop.\n");
    return NULL;
  };
/*
 * Remove the hash table from the top of the stack and
 * delete it.
 */
  return (Statement* )del_ListNode(sc->script.comp_stack, sc->script.comp_stack->head, NULL);
}

/*.......................................................................
 * Find the innermost loop-statement that is currently being compiled.
 *
 * Input:
 *  sc         Script *   The host scripting environment.
 * Output:
 *  return  Statement *   The located loop statement, or NULL if not
 *                        found.
 */
static Statement *find_CompLoopStmt(Script *sc)
{
  ListNode *node;   /* The stack node being searched */
  for(node=sc->script.comp_stack->head; node; node=node->next) {
    Statement *stmt = (Statement* )node->data;
    if(stmt->loop)
      return stmt;
    else if(stmt->type == STMT_CALL)
      return NULL;
  };
  return NULL;
}

/*.......................................................................
 * Skip over a statement list in the input stream.
 *
 * Input:
 *  sc           Script *   The host scripting environment.
 *  stream  InputStream *   The stream to read from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
static int skip_StatementList(Script *sc, InputStream *stream)
{
  int nbrace = 0;   /* The number of unclosed braces */
  int escaped = 0;  /* True when the next character should be escaped */
  int string = 0;   /* True when traversing the contents of a string */
/*
 * Locate the opening brace of the statement list.
 */
  if(input_skip_white(stream, 0, 0) ||
     stream->nextc != '{') {
    return input_error(stream, 1,
		       "Unable to find the start '{' of the statement list.\n");
  };
/*
 * Starting with the already detected open brace, discard all characters
 * until the open-brace that starts the following statement list, has
 * been matched by a close-brace.
 */
  do {
/*
 * Count unclosed braces outside strings.
 */
    switch(stream->nextc) {
    case '{':
      if(!string && !escaped)
	nbrace++;
      break;
    case '}':
      if(!string && !escaped)
	nbrace--;
      break;
    case '"':
      if(!escaped)
	string = !string;
      break;
    };
/*
 * The special meanings of '{', '}', '"' can be escaped by preceding them
 * with '\', so keep an account of whether the next character is marked
 * to be escaped.
 */
    escaped = stream->nextc == '\\' && !escaped;
/*
 * Read the next character.
 */
    if(read_InputStream(stream, 0)) {
      input_error(stream, 1,
		  "Unable to find the end '}' of the statment list.\n");
      return 1;
    };
/*
 * Stop when all open braces have been matched.
 */
  } while(nbrace > 0);
/*
 * Skip any spaces that follow the close brace.
 */
  if(input_skip_space(stream, 1, 0))
    return 1;
  return 0;
}

/*.......................................................................
 * Send a signal to the specified script.
 *
 * Input:
 *  sc        Script *   The script to send the signal to.
 *  sym       Symbol *   The symbol table entry of the signal, as
 *                       returned by lookup_script_signal().
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int signal_script(Script *sc, Symbol *sym)
{
/*
 * Check the arguments.
 */
  if(!sc || !sym) {
    lprintf(stderr, "signal_script: NULL argument(s).\n");
    return 1;
  };
/*
 * Increment the count of the number of times this signal has been
 * received and not cleared.
 */
  sym->code++;
/*
 * See if this triggers a catch statement.
 */
  if(catch_script_events(sc)==CAUGHT_ERROR)
    return 1;
  return 0;
}

/*.......................................................................
 * Evaluate the clauses of nested catch statements, from the outermost
 * inwards. On evaluating a clause that turns out to be true, unwind
 * the execution stack to the associated catch statement, and continue
 * execution from there.
 *
 * Input:
 *  sc          Script *  The parent script.
 * Output:
 *  return  CatchState    The status of the catch clause perusal, from:
 *                         CAUGHT_EVENT    - An event was caught and
 *                                           the statement that was about
 *                                           to be executed has been aborted.
 *                         CAUGHT_NOTHING  - All catch clauses were false.
 *                         CAUGHT_ERROR    - A fatal error occured.
 */
CatchState catch_script_events(Script *sc)
{
  ExeFrame *top;             /* The top of the stack of executing frames */
  ListNode *node;            /* A node in the list of catch clauses */
/*
 * Check all catch clauses, starting from the outermost catch
 * statement.
 */
  for(node=sc->script.catch_list->head; node; node=node->next) {
    CatchStmt *catchstmt = (CatchStmt* )node->data;
    Variable *result = exe_Expr(sc, catchstmt->clause) ? NULL :
      pop_ExprStack(sc, catchstmt->clause);
    if(!result)
      return CAUGHT_ERROR;
/*
 * Is the clause true?
 */
    if(BOOL_VARIABLE(result)->boolvar)
      break;
  };
/*
 * If a true catch clause was found, unwind the execution-frame stack until
 * the associated catch statment is found.
 */
  if(node) {
    CatchStmt *catchstmt = (CatchStmt* )node->data;
    for(top=top_ExeFrame(sc); top && &top->stmt->context.catchstmt != catchstmt;
	top=top_ExeFrame(sc)) {
/*
 * Cleanup and remove the completed statement from the stack.
 */
      pop_ExeFrame(sc);
    };
/*
 * Not found?
 */
    if(!top) {
      lprintf(stderr, "Can't match catch-clause with any catch statement.\n");
      bad_script(sc);
      return CAUGHT_ERROR;
    };
/*
 * Remove the catch clause from the list of watched clauses.
 */
    (void) del_ListNode(sc->script.catch_list, catchstmt->node, NULL);
    catchstmt->node = NULL;
/*
 * If the catch statement has an action block, execute it next, otherwise
 * mark the catch statement as complete.
 */
    if(catchstmt->action) {
      ListNode *head = catchstmt->action->head;
      if(head && push_ExeFrame(sc, (Statement* )head->data, head) == NULL)
	return CAUGHT_ERROR;
    } else {
      top->whatnext = EXE_STMT_FINISH;
    };
    return CAUGHT_EVENT;
  };
  return CAUGHT_NOTHING;
}
