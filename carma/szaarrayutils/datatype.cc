#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <limits.h>  /* CHAR_BIT */
#include <float.h>   /* DBL_DIG */
#include <math.h>    /* floor() */
#include <cstring>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"
#include "carma/szaarrayutils/pathname.h"
#include "carma/szaarrayutils/szaconst.h"

#include "carma/szautil/Exception.h"

/*
 * The context member of the boolean DataType will point to an object
 * of the following type.
 */
typedef struct {
  ScriptObj header;    /* The required garbage collection header */
  ScOperator *not_op;    /* The boolean negation operater proc */
  ScOperator *and_op;    /* The boolean AND operater proc */
  ScOperator *or_op;     /* The boolean OR operater proc */
  ScOperator *gt_op;     /* The relational > scoperator */
  ScOperator *lt_op;     /* The relational < scoperator */
  ScOperator *ge_op;     /* The relational >= scoperator */
  ScOperator *le_op;     /* The relational <= scoperator */
  ScOperator *eq_op;     /* The relational == scoperator */
  ScOperator *ne_op;     /* The relational != scoperator */
  ScOperator *in_op;     /* The relational ~ scoperator */
  ScOperator *ex_op;     /* The relational !~ scoperator */
} BoolType;

static DT_PARSE(parse_boolvar);
static DT_CONST(const_boolvar);
static DT_PRINT(print_boolvar);
static DT_RELFN(equal_boolvar);
static OPER_FN(boolvar_not_fn);
static OPER_FN(boolvar_and_fn);
static OPER_FN(boolvar_or_fn);
static OPER_FN(boolvar_gt_fn);
static OPER_FN(boolvar_lt_fn);
static OPER_FN(boolvar_ge_fn);
static OPER_FN(boolvar_le_fn);
static OPER_FN(boolvar_eq_fn);
static OPER_FN(boolvar_ne_fn);
static OPER_FN(boolvar_in_fn);
static OPER_FN(boolvar_ex_fn);
static int parse_relational(Script *sc, DataType *boolvar, InputStream *stream,
			    Expr *e);

/*
 * The context member of the boolean DataType will point to an object
 * of the following type.
 */
typedef struct {
  ScriptObj header;    /* The required garbage collection header */
  ScOperator *not_op;    /* The boolean negation operater proc */
  ScOperator *and_op;    /* The boolean AND operater proc */
  ScOperator *or_op;     /* The boolean OR operater proc */
  ScOperator *gt_op;     /* The relational > scoperator */
  ScOperator *lt_op;     /* The relational < scoperator */
  ScOperator *ge_op;     /* The relational >= scoperator */
  ScOperator *le_op;     /* The relational <= scoperator */
  ScOperator *eq_op;     /* The relational == scoperator */
  ScOperator *ne_op;     /* The relational != scoperator */
  ScOperator *in_op;     /* The relational ~ scoperator */
  ScOperator *ex_op;     /* The relational !~ scoperator */
} WildcardType;

/*
 * Private method functions of add_WildcardDataType().
 */
static DT_PARSE(parse_wildcardvar);
static DT_CONST(const_wildcardvar);
static DT_PRINT(print_wildcardvar);
static DT_RELFN(equal_wildcardvar);

/*
 * Private method functions of add_UintDataType().
 */
static DT_CONST(sc_const_uint);
static DT_PRINT(sc_print_uint);

/*
 * Private method functions of add_IntDataType().
 */
static DT_PARSE(sc_parse_int);
static DT_CONST(sc_const_int);
static DT_PRINT(sc_print_int);
static OPER_FN(sc_negate_int);
static OPER_FN(sc_add_int);
static OPER_FN(sc_sub_int);

typedef struct {
  ScriptObj header;           /* The required garbage collection header */
  ScOperator *neg_op;         /* The unity minus scoperator */
  ScOperator *add_op;         /* The addition operator */
  ScOperator *sub_op;         /* The subtraction operator */
  DataType   *add_dt;         /* The Integer datatype used in addition */
} IntContext;

/*
 * Private method functions of add_DoubleDataType().
 */
static DT_PARSE(sc_parse_double);
static DT_CONST(sc_const_double);
static DT_PRINT(sc_print_double);
static OPER_FN(sc_negate_double);
static OPER_FN(sc_add_double);
static OPER_FN(sc_sub_double);


typedef struct {
  ScriptObj header;           /* The required garbage collection header */
  ScOperator *neg_op;         /* The unity minus scoperator */
  ScOperator *add_op;         /* The addition operator */
  ScOperator *sub_op;         /* The subtraction operator */
  DataType   *add_dt;         /* The Double datatype used in addition */
} DoubleContext;

/*
 * Private method functions of add_SexagesimalDataType().
 */
static DT_PARSE(sc_parse_sexagesimal);
static DT_CONST(sc_const_sexagesimal);
static DT_PRINT(sc_print_sexagesimal);

typedef struct {
  ScriptObj header;           /* The required garbage collection header */
  ScOperator *neg_op;         /* The unity minus scoperator */
  ScOperator *add_op;         /* The addition operator */
  ScOperator *sub_op;         /* The subtraction operator */
  DataType   *add_dt;         /* The Double datatype used in addition */
} SexagesimalContext;

static DT_CONST(const_choice);
static DT_PRINT(print_choice);
static DT_RELFN(equal_choice);

static DT_PARSE(parse_set);
static DT_CONST(const_set);
static DT_PRINT(print_set);
static DT_RELFN(equal_set);
static DT_RELFN(in_set);
static OPER_FN(set_inc_fn);
static OPER_FN(set_exc_fn);

/*
 * The context member of each string DataType will point to a
 * different object of the following type. It determines whether
 * that particular datatype requires users to enclose strings with
 * quotes.
 */
typedef struct {
  ScriptObj header;   /* The required garbage collection header */
  ScOperator *add_op; /* The addition operator */
  DataType   *add_dt; /* The string datatype used in addition */
  int quoted;         /* True if string values must be enclosed in quotes */
} StringType;

/*
 * Private functions of add_StringDataType().
 */
static DT_CONST(sc_const_string);
static DT_PRINT(sc_print_string);
static DT_CONST(sc_parse_string);
static OPER_FN(sc_add_string);
static int not_separator_char(int c);

/*
 * The context member of each keyword DataType will point to a
 * different object of the following type. It determines whether
 * that particular datatype wants keyword names to be folded to
 * lower case.
 */
typedef struct {
  ScriptObj header;  /* The required garbage collection header */
  int fold;          /* True to fold keyword strings to lower case */
} KeywordType;

static DT_CONST(const_keyword);
static DT_PRINT(print_keyword);
static DT_RELFN(equal_keyword);

static DT_CONST(const_path);
static DT_PRINT(print_path);

static DT_CONST(const_group);
static DT_PRINT(print_group);
static DT_RELFN(equal_group);

static DT_RELFN(equal_list);

static DT_PARSE(parse_constant);

static DT_CONST(const_symbol);
static DT_PRINT(print_symbol);
static DT_RELFN(equal_symbol);

static DT_CONST(const_signal);
static DT_PRINT(print_signal);
static DT_RELFN(equal_signal);

/*.......................................................................
 * Create a new datatype. Note that it is the duty of the caller to add
 * the datatype to the symbol table.
 *
 * Input:
 *  sc          Script *   The script environment in which to create
 *                         the datatype.
 *  name          char *   The name of the datatype.
 *  dataclass    TypeClass     The dataclassification of the datatype, from:
 *                           DT_BUILTIN - A builtin type.
 *                           DT_GROUP   - A user-defined group datatype.
 *  context       void *   Optional external context data. Pass NULL if
 *                         not required. If supplied, the context object
 *                         must have a lifetime at least as long as the
 *                         script. If its structure is sufficiently simple,
 *                         this can be achieved by allocating from a suitable
 *                         freelist returned by new_ScriptFreeList().
 *  vsize       size_t     The freelist node size required for allocating
 *                         variables of the new datatype.
 *  inc_dt    DataType *   The datatype that the user should use when
 *                         specifying do-loop increments. To disallow use
 *                         of the new datatype in do-loops, pass NULL here.
 *  parse_fn  PARSE_FN(*)  A function to call to parse expressions of
 *                         the new datatype. Send 0 for datatypes that
 *                         don't implement expressions.
 *  const_fn  DT_CONST(*)  The function that parse_operand() calls to
 *                         parse a constant of the datatype. Note that
 *                         parse_operand() positions the stream at the
 *                         start of the constant. Note that if check_fn()
 *                         is non-zero, then it is the responsibility of
 *                         const_fn() to call it.
 *  check_fn  CHECK_FN(*)  A function to call to check a value parsed by
 *                         parse_fn(). This is kept separate from const_fn()
 *                         so as to cater for datatypes that differ only in
 *                         the legal ranges of values that they support.
 *                         For other types, where the value is validated
 *                         by const_fn(), pass 0.
 *  print_fn  DT_PRINT(*)  A function to call to render a textual version
 *                         of the value of a variable of the datatype.
 *  eq_fn     DT_RELFN(*)  A function to call to test whether two variables
 *                         of the new datatype have the same value.
 *  gt_fn     DT_RELFN(*)  An optional function to call to test whether
 *                         the first of two variables of the new datatype
 *                         is less than the second. If not meaningful
 *                         pass 0.
 *  in_fn     DT_RELFN(*)  An optional function to call to test whether
 *                         the value of the first of two variables of the
 *                         new datatype contains the value of the second.
 *                         If not meaningful pass 0. This is aimed at set
 *                         types.
 *  iter_fn    DT_ITER(*)  If you want to be able to use variables of
 *                         the new datatype as loop variables, you must
 *                         provide an iterator function here. See the
 *                         description of DT_ITER() in script.h. If you
 *                         don't want this facility, pass 0.
 *  incr_name     char *   If iter_fn() is provided, also provide the
 *                         the name of the datatype used to specify the
 *                         do-loop increment. This can be the name of
 *                         the new datatype if it can be used to increment
 *                         itself.
 * Output:
 *  return    DataType *   A pointer to the new data-type description,
 *                         or NULL on error.
 */
DataType *new_DataType(Script *sc, char *name, TypeClass dataclass,
		       void *context, size_t vsize,
		       DT_CHECK(*check_fn), DT_PARSE(*parse_fn),
		       DT_CONST(*const_fn), DT_PRINT(*print_fn),
		       DT_RELFN(*eq_fn), DT_RELFN(*gt_fn), DT_RELFN(*in_fn),
		       DT_ITER(*iter_fn), char *incr_name,
		       DataTypeId id)
{
  DataType *dt;      /* The object to be returned */
  /*
   * Check arguments.
   */
  if(!sc || !name || !const_fn || !print_fn || !eq_fn ||
     (iter_fn && !incr_name)) {
    lprintf(stderr, "new_DataType: Invalid arguments.\n");
    return NULL;
  };
  switch(dataclass) {
  case DT_BUILTIN:
  case DT_GROUP:
    break;
  default:
    lprintf(stderr, "new_DataType: Unknown type-dataclass.\n");
    return NULL;
  };
  if(vsize < sizeof(Variable)) {
    lprintf(stderr, "new_DataType: Variable smaller than mandatory header.\n");
    return NULL;
  };
  /*
   * Allocate the datatype container.
   */
  dt = (DataType* )new_ScriptObject(sc, sc->memory.datatype, 0);
  if(!dt)
    return NULL;
  /*
   * Initialize the container.
   */
  dt->name = NULL;
  dt->id   = id;
  dt->parse_fn = parse_fn ? parse_fn : parse_constant;
  dt->const_fn = const_fn;
  dt->check_fn = check_fn;
  dt->print_fn = print_fn;
  dt->eq_fn = eq_fn;
  dt->gt_fn = gt_fn;
  dt->in_fn = in_fn;
  dt->iter_fn = iter_fn;
  dt->incr_dt = NULL;
  dt->context = context;
  dt->vsize = vsize;
  dt->vmemory = NULL;
  dt->atom_reg = NULL;
  dt->list_reg = NULL;
  dt->null_atom = NULL;
  dt->null_list = NULL;
  dt->dataclass = dataclass;
  /*
   * Record the name of the datatype.
   */
  dt->name = new_ScriptString(sc, name);
  if(!dt->name)
    return NULL;
  /*
   * Find a suitable freelist to allocate variables from.
   */
  dt->vmemory = new_ScriptFreeList(sc, dt->vsize);
  if(!dt->vmemory)
    return NULL;
  /*
   * Create atom and list declarations for use in declaring unnamed
   * intermediate variables.
   */
  dt->atom_reg = new_TypeSpec(sc, NULL, dt, 0);
  if(!dt->atom_reg)
    return NULL;
  dt->list_reg = new_TypeSpec(sc, NULL, dt, 1);
  if(!dt->list_reg)
    return NULL;
  /*
   * Create null atom and list variables to substitute as values for
   * the omitted optional arguments of procedures.
   */
  dt->null_atom = new_Variable(sc, dt->atom_reg);
  if(!dt->null_atom)
    return NULL;
  dt->null_atom->flags |= VAR_IS_NUL;
  dt->null_list = new_Variable(sc, dt->list_reg);
  if(!dt->null_list)
    return NULL;
  dt->null_list->flags |= VAR_IS_NUL;
  /*
   * Look up the data-type to use for do-loop increment arguments.
   */
  if(incr_name) {
    if(strcmp(name, incr_name)==0)
      dt->incr_dt = dt;
    else
      dt->incr_dt = find_DataType(sc, NULL, incr_name);
    if(!dt->incr_dt) {
      lprintf(stderr,
	      "new_DataType(%s): Incremental datatype '%s' not found.\n",
	      name, incr_name);
      return NULL;
    };
  };
  return dt;
}

/*.......................................................................
 * The following parsing function is used by datatypes that don't implement
 * expressions.
 */
static DT_PARSE(parse_constant)
{
  return parse_operand(sc, dt, 0, stream, e);
}

/*.......................................................................
 * Create the boolean datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_BooleanDataType(Script *sc)
{
  DataType *dt;        /* The datatype to be returned */
  BoolType *context;   /* Type-specific datatype context */
  if(!sc) {
    lprintf(stderr, "add_BooleanDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (BoolType* )new_ScriptObject(sc, NULL, sizeof(BoolType));
  if(!context)
    return NULL;
  context->not_op = NULL;
  context->and_op = NULL;
  context->or_op = NULL;
  context->gt_op = NULL;
  context->lt_op = NULL;
  context->ge_op = NULL;
  context->le_op = NULL;
  context->eq_op = NULL;
  context->ne_op = NULL;
  context->in_op = NULL;
  context->ex_op = NULL;
  /*
   * Create the datatype.
   */
  dt = new_DataType(sc, "Boolean", DT_BUILTIN, context,
		    sizeof(BoolVariable),
		    0, parse_boolvar, const_boolvar, print_boolvar, equal_boolvar, 0, 0,
		    0, NULL);
  if(!dt)
    return NULL;
  /*
   * Create the boolean scoperators.
   */
  context->not_op = new_ScOperator(sc, dt, 1, boolvar_not_fn);
  if(!context->not_op)
    return NULL;
  context->and_op = new_ScOperator(sc, dt, 2, boolvar_and_fn);
  if(!context->and_op)
    return NULL;
  context->or_op = new_ScOperator(sc, dt, 2, boolvar_or_fn);
  if(!context->or_op)
    return NULL;
  context->gt_op = new_ScOperator(sc, dt, 2, boolvar_gt_fn);
  if(!context->gt_op)
    return NULL;
  context->lt_op = new_ScOperator(sc, dt, 2, boolvar_lt_fn);
  if(!context->lt_op)
    return NULL;
  context->ge_op = new_ScOperator(sc, dt, 2, boolvar_ge_fn);
  if(!context->ge_op)
    return NULL;
  context->le_op = new_ScOperator(sc, dt, 2, boolvar_le_fn);
  if(!context->le_op)
    return NULL;
  context->eq_op = new_ScOperator(sc, dt, 2, boolvar_eq_fn);
  if(!context->eq_op)
    return NULL;
  context->ne_op = new_ScOperator(sc, dt, 2, boolvar_ne_fn);
  if(!context->ne_op)
    return NULL;
  context->in_op = new_ScOperator(sc, dt, 2, boolvar_in_fn);
  if(!context->in_op)
    return NULL;
  context->ex_op = new_ScOperator(sc, dt, 2, boolvar_ex_fn);
  if(!context->ex_op)
    return NULL;
  /*
   * Install the data-type in the current scope.
   */
  if(!add_ScriptSymbol(sc, dt->name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(dt->name, DT_BOOL, context));
  
  return dt;
}

/*.......................................................................
 * Parse a 'boolean' expression.
 */
static DT_PARSE(parse_boolvar)
{
  BoolType *bt = (BoolType* )dt->context;  /* The boolvar-specific
					      members of the
					      datatype */
  int term;                    /* The operand number */
  int doand = 0;               /* True for & expressions, false for | */
  /*
   * Parse one or more operands separated by & or | scoperators.
   */
  for(term=0; ;term++) {
    int negate = 0;   /* The number of '!' scoperators seen */
    /*
     * Locate the start of the expression.
     */
    if(input_skip_white(stream, 1, 0))
      return 1;
    /*
     * Check for one or more boolean negation scoperator.
     */
    while(stream->nextc == '!') {
      negate++;
      if(input_skip_white(stream, 1, 1))
	return 1;
    };
    /*
     * If there were any negation scoperators, then the next operand must be
     * boolean.
     */
    if(negate) {
      if(parse_operand(sc, dt, 0, stream, e))
	return 1;
      /*
       * If the operand was preceded by an odd number of negation scoperators,
       * push the negation function onto the expression stack.
       */
      if((negate%2) && !add_OpFnOper(sc, e, bt->not_op))
	return 1;
      /*
       * Parse a relational or boolean operand expression.
       */
    } else {
      if(parse_relational(sc, dt, stream, e))
	return 1;
    };
    /*
     * Locate the start of the next term.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    /*
     * After parsing two operands separated by an & or an | scoperator,
     * stack the scoperator.
     */
    if(term > 0) {
      if(!add_OpFnOper(sc, e, doand ? bt->and_op : bt->or_op))
	return 1;
    };
    /*
     * Now see if there is another & or | scoperator preceding a new
     * operand.
     */
    switch(stream->nextc) {
    case '&':
      doand = 1;
      break;
    case '|':
      doand = 0;
      break;
    default:      /* No more operands */
      return 0;
      break;
    };
    /*
     * Skip the scoperator.
     */
    if(input_skip_white(stream, 1, 1))
      return 1;
  };
}

/*.......................................................................
 * Parse a relational or boolean operand expression.
 *
 * Input:
 *  sc          Script *   The host script environment.
 *  boolvar      DataType *   The boolean datatype.
 *  stream InputStream *   The stream to parse the expression from.
 *  e             Expr *   The expression to append to.
 * Output:
 *  return         int      0 - OK.
 *                          1 - Error.
 */
static int parse_relational(Script *sc, DataType *boolvar, InputStream *stream,
			    Expr *e)
{
  BoolType *bt = (BoolType* )boolvar->context;  /* The
						   boolvar-specific
						   members of the
						   datatype */
  TypeSpec *type;         /* The type of variables in a relational expression */
  /*
   * Locate the start of the first operand.
   */
  if(input_skip_white(stream, 1, 0))
    return 1;
  /*
   * The next operand must either start with a $ or be a boolean constant.
   */
  switch(stream->nextc) {
  case '$':
    type = parse_dollar_expr(sc, NULL, 0, stream, e);
    if(!type)
      return 1;
    break;
  default:
    type = boolvar->atom_reg;
    if(parse_operand(sc, boolvar, 0, stream, e))
      return 1;
    break;
  };
  /*
   * Parse 1 or more relational expressions.
   */
  do {
    ScOperator *op=NULL;  /* The relational scoperator to use next */
    int ca, cb;         /* The first and second potential scoperator characters */
    /*
     * Locate the next character.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    /*
     * Now see if the next one or two characters constitute a supported
     * relational scoperator.
     */
    switch(stream->nextc) {
    case '>': case '<':
      if(!type->dt->gt_fn || type->is_list) {
	return input_error(stream, 1,
			   "The < and > scoperators aren't supported by '%s' datatypes.\n",
			   type->is_list ? "list" : type->dt->name);
      };
      break;
    case '=': case '!': case '~':
      break;
    default:	/* Not a relational scoperator - must be a boolean operand */
      if(type->dt != boolvar || type->is_list) {
	return input_error(stream, 1,
			   "Missing relational scoperator after non-boolean value.\n");
      };
      return 0;
      break;
    };
    /*
     * Skip the first scoperator character.
     */
    ca = stream->nextc;
    if(read_InputStream(stream, 1))
      return 1;
    /*
     * Get the following character. If it is '=' then it is the
     * second scoperator character, so record and consume it.
     */
    cb = stream->nextc;
    switch(cb) {
    case '=':
      if(read_InputStream(stream, 1))
	return 1;
      switch(ca) {
      case '>':            /* >= */
	op = bt->ge_op;
	break;
      case '<':            /* <= */
	op = bt->le_op;
	break;
      case '=':            /* == */
	op = bt->eq_op;
	break;
      case '!':            /* != */
	op = bt->ne_op;
	break;
      default:
	op = NULL;
	break;
      };
      break;
    case '~':
      if(read_InputStream(stream, 1))
	return 1;
      switch(ca) {
      case '!':            /* !~ (use != where ~ isn't supported) */
	op = (type->dt->in_fn && !type->is_list) ? bt->ex_op : bt->ne_op;
	break;
      default:
	op = NULL;
	break;
      };
      break;
    default:
      switch(ca) {
      case '>':            /* > */
	op = bt->gt_op;
	break;
      case '<':            /* < */
	op = bt->lt_op;
	break;
      case '~':            /* ~ (use == where ~ isn't supported) */
	op = (type->dt->in_fn && !type->is_list) ? bt->in_op : bt->eq_op;
	break;
      default:
	op = NULL;
	break;
      };
    };
    /*
     * Unknown scoperator?
     */
    if(!op)
      return input_error(stream, 1, "Unknown relational scoperator '%c%c'.\n",
			 ca, cb==EOF ? ' ':cb);
    /*
     * Find the start of the next operand.
     */
    if(input_skip_white(stream, 1, 0))
      return 1;
    /*
     * The next operand must be of the same type as the first.
     */
    if(parse_argument(sc, type, stream, e))
      return 1;
    /*
     * Add the relational scoperator to the instruction list.
     */
    if(!add_OpFnOper(sc, e, op))
      return 1;
    /*
     * The result of the relational expression now potentially becomes
     * the first operand of a boolean relational expression.
     */
    type = boolvar->atom_reg;
  } while(1);
}

/*.......................................................................
 * Parse a 'boolvar' constant.
 */
static DT_CONST(const_boolvar)
{
  Variable *var;        /* A boolean-constant */
  int boolvar;             /* The value to be recorded */
  /*
   * Read the boolean enumerator-name from the input stream.
   */
  if(input_keyword(stream, 0, 1))
    return input_error(stream, 1, "Missing boolean value.\n");
  /*
   * Identify the boolean enumerator.
   */
  if(strcmp(stream->work, "false")==0) {
    boolvar = 0;
  } else if(strcmp(stream->work, "true")==0) {
    boolvar = 1;
  } else {
    return input_error(stream, 1, "Unknown boolean value: \"%s\"\n",
		       stream->work);
  };
  /*
   * Record the boolean in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the constant.
   */
  BOOL_VARIABLE(var)->boolvar = boolvar;
  return 0;
}

/*.......................................................................
 * Print a boolean value.
 */
static DT_PRINT(print_boolvar)
{
  return write_OutputStream(output, BOOL_VARIABLE(var)->boolvar ?
			    "true":"false");
}

/*.......................................................................
 * Test two boolean variables for equality.
 */
static DT_RELFN(equal_boolvar)
{
  return !BOOL_VARIABLE(va)->boolvar == !BOOL_VARIABLE(vb)->boolvar;
}

/*.......................................................................
 * Define the boolean-not scoperator function.
 */
static OPER_FN(boolvar_not_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1;        /* The input argument */
  /*
   * Get the single argument.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  /*
   * Record the boolean NOT of the argument.
   */
  BOOL_VARIABLE(result)->boolvar = ! BOOL_VARIABLE(v1)->boolvar;
  return 0;
}

/*.......................................................................
 * Define the boolean-and scoperator function.
 */
static OPER_FN(boolvar_and_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Record the boolean AND of the two arguments.
   */
  BOOL_VARIABLE(result)->boolvar =
    BOOL_VARIABLE(v1)->boolvar && BOOL_VARIABLE(v2)->boolvar;
  return 0;
}

/*.......................................................................
 * Define the boolean-or scoperator function.
 */
static OPER_FN(boolvar_or_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Record the boolean OR of the two arguments.
   */
  BOOL_VARIABLE(result)->boolvar =
    BOOL_VARIABLE(v1)->boolvar || BOOL_VARIABLE(v2)->boolvar;
  return 0;
}

/*.......................................................................
 * Define the relational > scoperator function.
 */
static OPER_FN(boolvar_gt_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*gt_fn);    /* The > scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the > method of the variables being compared.
   */
  gt_fn = v1->type->dt->gt_fn;
  /*
   * Use the > method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = gt_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational < scoperator function.
 */
static OPER_FN(boolvar_lt_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*gt_fn);    /* The > scoperator of the variables being compared */
  DT_RELFN(*eq_fn);    /* The == scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the relational primatives of the variables being compared.
   */
  gt_fn = v1->type->dt->gt_fn;
  eq_fn = v1->type->dt->eq_fn;
  /*
   * Perform the < operation using the > and == methods of the variables.
   */
  BOOL_VARIABLE(result)->boolvar = !gt_fn(v1, v2) && !eq_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational >= scoperator function.
 */
static OPER_FN(boolvar_ge_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*gt_fn);    /* The > scoperator of the variables being compared */
  DT_RELFN(*eq_fn);    /* The == scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the relational primatives of the variables being compared.
   */
  gt_fn = v1->type->dt->gt_fn;
  eq_fn = v1->type->dt->eq_fn;
  /*
   * Perform the >= operation using the > and == methods of the variables.
   */
  BOOL_VARIABLE(result)->boolvar = gt_fn(v1, v2) || eq_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational <= scoperator function.
 */
static OPER_FN(boolvar_le_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*gt_fn);    /* The > scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the > method of the variables being compared.
   */
  gt_fn = v1->type->dt->gt_fn;
  /*
   * Use the > method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = !gt_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational == scoperator function.
 */
static OPER_FN(boolvar_eq_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*eq_fn);    /* The == scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the equality method of the variables being compared.
   */
  eq_fn = v1->type->is_list ? equal_list : v1->type->dt->eq_fn;
  /*
   * Use the equality method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = eq_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational != scoperator function.
 */
static OPER_FN(boolvar_ne_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*eq_fn);    /* The == scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the equality method of the variables being compared.
   */
  eq_fn = v1->type->is_list ? equal_list : v1->type->dt->eq_fn;
  /*
   * Use the equality method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = !eq_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational =~ scoperator function.
 */
static OPER_FN(boolvar_in_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*in_fn);    /* The =~ scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the "contains" method of the variables being compared.
   */
  in_fn = v1->type->dt->in_fn;
  /*
   * Use the "contains" method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = in_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define the relational !~ scoperator function.
 */
static OPER_FN(boolvar_ex_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  DT_RELFN(*in_fn);    /* The =~ scoperator of the variables being compared */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Get the "contains" method of the variables being compared.
   */
  in_fn = v1->type->dt->in_fn;
  /*
   * Use the "contains" method of the variables to compute the boolean result.
   */
  BOOL_VARIABLE(result)->boolvar = !in_fn(v1, v2);
  return 0;
}

/*.......................................................................
 * Define a choice-menu datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  choices Enumerator *   The array of choice name/value pairs to be
 *                         installed. This MUST be a statically allocated
 *                         array that musn't be modified while the
 *                         new datatype exists.
 *  nchoice   unsigned     The number of elements in choices[].
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_ChoiceDataType(Script *sc, char *name,
			     Enumerator *choices, int nchoice, bool allow_bit_mask)
{
  ChoiceType *context; /* Type-specific context data */
  DataType *dt;        /* The datatype to be returned */
  if(!sc || !name || !choices || nchoice < 1) {
    lprintf(stderr, "add_ChoiceDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (ChoiceType* )new_ScriptObject(sc, NULL, sizeof(ChoiceType));
  if(!context)
    return NULL;
  /*
   * Initialize the associated data of the data-type.
   */
  context->choices = choices;
  context->nchoice = nchoice;
  
  context->allow_bit_mask = allow_bit_mask;
  
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(ChoiceVariable),
		    0, 0, const_choice, print_choice, equal_choice, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(name, DT_CHOICE, context));
  
  return dt;
}

/*.......................................................................
 * Parse a 'choice' constant.
 */
static DT_CONST(const_choice)
{
  Variable *var;        /* The variable that will contain the choice */
  ChoiceType *ct;       /* The choice-specific datatype members */
  int i;
  /*
   * Get the choice-specific members of the data-type.
   */
  ct = (ChoiceType* )dt->context;
  
  /*
   * Read the choice name from the input stream.
   */
  if(inputEnumKeyword(stream, 0, 0))
    return input_error(stream, 1, "Missing '%s' value.\n", dt->name);
  /*
   * Perform a case-insensitive comparison of the keyword with the
   * option names of the current datatype.
   */
  for(i=0; i < (int)ct->nchoice; i++) {
    if(strcasecmp(stream->work, ct->choices[i].name) == 0)
      break;
  };
  if(i >= (int)ct->nchoice) {
    return input_error(stream, 1, "Unknown '%s' option: \"%s\"\n", dt->name,
		       stream->work);
  };
  /*
   * Record the choice in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the value of the constant.
   */
  CHOICE_VARIABLE(var)->choice = ct->choices[i].value;
  return 0;
}

/*.......................................................................
 * Print a choice value.
 */
static DT_PRINT(print_choice)
{
  ChoiceType *ct = (ChoiceType* )var->type->dt->context;
  int i;
  /*
   * Find the choice member that has the specified value.
   */
  for(i=0; i < (int)ct->nchoice; i++) {
    if(ct->choices[i].value == CHOICE_VARIABLE(var)->choice) {
      return write_OutputStream(output, ct->choices[i].name);
    };
  };
  lprintf(stderr, "print_choice: Variable contains an invalid value.\n");
  return 1;
}

/*.......................................................................
 * Test two choice variables (of the same datatype) for equality.
 */
static DT_RELFN(equal_choice)
{
  return CHOICE_VARIABLE(va)->choice == CHOICE_VARIABLE(vb)->choice;
}

/*.......................................................................
 * Define a set datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  allow_bit_mask int     True to allow set values to be specified using
 *                         numeric bit masks as well as textual enumerators.
 *  members Enumerator *   The array of member name/value pairs to be
 *                         installed. This MUST be a statically allocated
 *                         array that isn't to be modified while the
 *                         new datatype exists. Note that the member
 *                         name "all" shouldn't be used.
 *  nmember   unsigned     The number of elements in members[].
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_SetDataType(Script *sc, char *name, int allow_bit_mask,
			  Enumerator *members, unsigned nmember, DT_ITER(*iter_fn), char *incr_name)
{
  SetType *context;    /* The type-specific context data */
  DataType *dt;        /* The object to be returned */
  int i;
  /*
   * Check arguments.
   */
  if(!sc || !name || !members || nmember < 1) {
    lprintf(stderr, "add_SetDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (SetType* )new_ScriptObject(sc, NULL, sizeof(SetType));
  if(!context)
    return NULL;
  /*
   * Initialize the associated data of the data-type.
   */
  context->members = members;
  context->nmember = nmember;
  context->all = 0;
  context->nbit = 0;
  context->allow_bit_mask = allow_bit_mask;
  context->inc_op = NULL;
  context->exc_op = NULL;
  for(i=0; i < (int)nmember; i++)
    context->all |= members[i].value;
  /*
   * Count the number of bits that are set in 'all'.
   */
  for(i=0; i < (int)(sizeof(context->all) * CHAR_BIT); i++) {
    if(context->all & (1U<<i))
      context->nbit++;
  };
  /*
   * Create the datatype.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(SetVariable),
		    0, parse_set, const_set, print_set, equal_set, 0, in_set,
		    iter_fn, incr_name, DT_SEXAGESIMAL);
  if(!dt)
    return NULL;
  /*
   * Create the set scoperators.
   */
  context->inc_op = new_ScOperator(sc, dt, 2, set_inc_fn);
  if(!context->inc_op)
    return NULL;
  context->exc_op = new_ScOperator(sc, dt, 2, set_exc_fn);
  if(!context->inc_op)
    return NULL;
  /*
   * Add the data-type to the symbol table.
   */
  if(!add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(name, DT_SET, context));
  
  return dt;
}

/*.......................................................................
 * Parse a 'set' expression.
 */
static DT_PARSE(parse_set)
{
  int term;             /* This is incremented by one for each parsed term */
  SetType *st;          /* The set specific datatype members */
  Variable *var;        /* A set-constant */
  /*
   * Get the set specific members of the data-type.
   */
  st = (SetType* )dt->context;
  /*
   * Push an empty set onto the stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  SET_VARIABLE(var)->set = 0;
  /*
   * Parse an expression of the form:
   *
   * {+|-} _opt_ [set_operand] _chain_ {+|-}
   */
  for(term=0; ; term++) {
    int doadd = 1;    /* Default to adding operands to the set */
    /*
     * Find the start of the next term.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    /*
     * Check for and skip an instance of one of the + or - scoperators.
     * These scoperators are optional for the first term only.
     */
    switch(stream->nextc) {
    case '+':
      doadd = 1;
      if(input_skip_white(stream, 1, 1))
	return 1;
      break;
    case '-':
      doadd = 0;
      if(input_skip_white(stream, 1, 1))
	return 1;
      break;
    default:
      /*
       * Is this the end of the expression, or is it simply the first
       * term without the optional +- scoperator?
       */
      if(term > 0)
	return 0;
    };
    /*
     * Read a set operand.
     */
    if(parse_operand(sc, dt, 0, stream, e))
      return 1;
    /*
     * Push the specified inclusion/exclusion scoperator onto the
     * stack. This takes the current value of the set and the
     * new operand as its arguments.
     */
    if(!add_OpFnOper(sc, e, doadd ? st->inc_op : st->exc_op))
      return 1;
  };
}

/*.......................................................................
 * Parse a set-member constant.
 */
static DT_CONST(const_set)
{
  SetType *st;          /* The set specific datatype members */
  Variable *var;        /* A set-constant */
  unsigned long value;  /* The value of the constant */
  int i;
  /*
   * Get the set specific members of the data-type.
   */
  st = (SetType* )dt->context;
  /*
   * Allow numeric bit-masks?
   */
  if(st->allow_bit_mask && isdigit(stream->nextc)) {
    if(input_ulong(stream, 0, 1, &value))
      return input_error(stream, 1, "Malformed '%s' bit-mask.\n", dt->name);
  } else {
    /*
     * Read the set name from the input stream.
     */
    if(inputEnumKeyword(stream, 0, 1))
      return input_error(stream, 1, "Missing '%s' value.\n", dt->name);
    /*
     * Perform a case-insensitive comparison of the keyword with the
     * option names of the current datatype.
     */
    for(i=0; i < (int)st->nmember; i++) {
      if(strcasecmp(stream->work, st->members[i].name) == 0)
	break;
    };
    if(i >= (int)st->nmember) {
      return input_error(stream, 1, "Unknown '%s' member: \"%s\"\n", dt->name,
			 stream->work);
    } else {
      value = st->members[i].value;
    };
  };
  /*
   * Record the set in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the value of the constant.
   */
  SET_VARIABLE(var)->set = value;
  return 0;
}

/*.......................................................................
 * Print a set value.
 */
static DT_PRINT(print_set)
{
  SetType *st = (SetType* )var->type->dt->context;/* The set datatype members */
  unsigned set = SET_VARIABLE(var)->set;  /* The set to be printed */
  unsigned nset = 0;                      /* The number of 1 bits in 'set' */
  unsigned i;
  /*
   * Count the number of bits that are set in 'set'.
   */
  for(i=0; i<sizeof(set) * CHAR_BIT; i++) {
    if(set & (1U << i))
      nset++;
  };
  /*
   * If more than half the possible number of bits are set, represent
   * the value as 'all - members'.
   */
  if(nset > st->nbit/2) {
    /*
     * Write the name of the full-set member.
     */
    if(write_OutputStream(output, "all"))
      return 1;
    /*
     * Subtract the names of bits of 'all' that are not set in value.
     */
    for(i=0; i<st->nmember; i++) {
      Enumerator *member = st->members + i;
      unsigned mask = member->value;
      if((mask & ~set) && (mask & ~set) == mask) {
	/*
	 * Prepend the element with a '-' exclusion scoperator.
	 */
	write_OutputStream(output, "-");
	/*
	 * Write the excluded set element.
	 */
	if(write_OutputStream(output, member->name))
	  return 1;
	/*
	 * Add the newly represented bits to the set value so that
	 * subsequent aliases don't get written.
	 */
	set |= mask;
      };
    };
    /*
     * Output the set as a set of members to be added to an implicit empty set.
     */
  } else {
    int first = 1;  /* True until any set member has been matched */
    for(i=0; i<st->nmember; i++) {
      Enumerator *member = st->members + i;
      unsigned mask = member->value;
      /*
       * Are all the bits of the set member also set in the input set?
       */
      if((mask & set) && (mask & set) == mask) {
	/*
	 * Prepend all but the first element with a '+' inclusion scoperator.
	 */
	if(first) {
	  first = 0;
	} else {
	  if(write_OutputStream(output, "+"))
	    return 1;
	};
	/*
	 * Write the set element.
	 */
	if(write_OutputStream(output, member->name))
	  return 1;
	/*
	 * Remove the newly represented bits from the set so that
	 * subsequent aliases don't get written.
	 */
	set &= ~mask;
      };
    };
    /*
     * If the set was empty, write the empty-set member name.
     */
    if(first && write_OutputStream(output, "none"))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Test two set variables (of the same datatype) for equality.
 */
static DT_RELFN(equal_set)
{
  return SET_VARIABLE(va)->set == SET_VARIABLE(vb)->set;
}

/*.......................................................................
 * Test two set variables (of the same datatype) to see if the first
 * contains the members of the seconds.
 */
static DT_RELFN(in_set)
{
  return (SET_VARIABLE(va)->set & SET_VARIABLE(vb)->set) ==
    SET_VARIABLE(vb)->set;
}

/*.......................................................................
 * Define the set-inclusion scoperator function.
 */
static OPER_FN(set_inc_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * The result is the union of the two sets.
   */
  SET_VARIABLE(result)->set = SET_VARIABLE(v1)->set | SET_VARIABLE(v2)->set;
  return 0;
}

/*.......................................................................
 * Define the set-exclusion scoperator function.
 */
static OPER_FN(set_exc_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * The result is the first set minus the elements of the second set.
   */
  SET_VARIABLE(result)->set = SET_VARIABLE(v1)->set & ~SET_VARIABLE(v2)->set;
  return 0;
}


/*.......................................................................
 * Define a wildcard datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  DT_CHECK(*)  The function to call to check whether the
 *                         value of an unsigned int variable is within
 *                         the domain of the datatype.
 *  
 *  iter_fn    DT_ITER(*)  An optional iterator function for use by
 *                         the 'do' command.
 *  incr_name     char *   The name of the datatype that the user should
 *                         use to specify do-loop increments to iter_fn().
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_WildcardDataType(Script *sc)
{
  DataType *dt;        /* The datatype to be returned */
  WildcardType *context;   /* Type-specific datatype context */

  if(!sc) {
    lprintf(stderr, "add_BooleanDataType: Invalid argument(s).\n");
    return NULL;
  };

  /*
   * Allocate an external context object for the datatype.
   */
  context = (WildcardType* )new_ScriptObject(sc, NULL, sizeof(WildcardType));

  if(!context)
    return NULL;

  context->not_op = NULL;
  context->and_op = NULL;
  context->or_op = NULL;
  context->gt_op = NULL;
  context->lt_op = NULL;
  context->ge_op = NULL;
  context->le_op = NULL;
  context->eq_op = NULL;
  context->ne_op = NULL;
  context->in_op = NULL;
  context->ex_op = NULL;

  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, "Wildcard", DT_BUILTIN, NULL, sizeof(Variable),
		    0, parse_wildcardvar, const_wildcardvar, print_wildcardvar, equal_wildcardvar, 0, 0, 0, 0);

  if(!dt || !add_ScriptSymbol(sc, dt->name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(dt->name, DT_WILDCARD));
  
  return dt;
}

/*.......................................................................
 * Parse a wildcard variable
 */
static DT_CONST(const_wildcardvar)
{
  return 0;
}

/*.......................................................................
 * Print an unsigned int variable.
 */
static DT_PRINT(print_wildcardvar)
{
  return 0;
}

/*.......................................................................
 * Parse a wildcard variable.
 */
static DT_PARSE(parse_wildcardvar)
{
  return 0;
}

/*.......................................................................
 * Parse a wildcard variable
 */
static DT_RELFN(equal_wildcardvar)
{
  return 0;
}

/*.......................................................................
 * Define an unsigned int datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  DT_CHECK(*)  The function to call to check whether the
 *                         value of an unsigned int variable is within
 *                         the domain of the datatype.
 *  
 *  iter_fn    DT_ITER(*)  An optional iterator function for use by
 *                         the 'do' command.
 *  incr_name     char *   The name of the datatype that the user should
 *                         use to specify do-loop increments to iter_fn().
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_UintDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			   DT_ITER(*iter_fn), char *incr_name)
{
  DataType *dt;    /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_UintDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(UintVariable),
		    check_fn, 0, sc_const_uint, sc_print_uint, sc_equal_uint,
		    sc_gt_uint, 0, iter_fn, incr_name);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(name, DT_UINT));
  
  return dt;
}

/*.......................................................................
 * Parse an unsigned integer constant.
 */
static DT_CONST(sc_const_uint)
{
  Variable *var;        /* The variable that will contain the number */
  unsigned long ul;     /* The literal value */
  /*
   * Read the number from the input stream.
   */
  if(input_ulong(stream, 0, 1, &ul))
    return input_error(stream, 1, "Missing unsigned '%s' value.\n", dt->name);
  /*
   * Record the number in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  UINT_VARIABLE(var)->uint = ul;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print an unsigned int variable.
 */
static DT_PRINT(sc_print_uint)
{
  return output_ulong(output, OUT_DECIMAL, "", 0, 0,
		      UINT_VARIABLE(var)->uint);
}

/*.......................................................................
 * Test two uint variables for equality.
 */
DT_RELFN(sc_equal_uint)
{
  return UINT_VARIABLE(va)->uint == UINT_VARIABLE(vb)->uint;
}

/*.......................................................................
 * Test whether the value of one uint variable is greater than a second.
 */
DT_RELFN(sc_gt_uint)
{
  return UINT_VARIABLE(va)->uint > UINT_VARIABLE(vb)->uint;
}

/*.......................................................................
 * This is a do-loop iterator function suitable for use with datatypes
 * in which the datatype was created by add_UintDataType() and its
 * incrementing datatype was created by add_IntDataType().
 */
DT_ITER(sc_iterate_uint)
{
  unsigned a = UINT_VARIABLE(first)->uint;
  unsigned b = UINT_VARIABLE(last)->uint;
  int inc = INT_VARIABLE(step)->i;
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    if(a < b && inc > 0)
      return (b-a) / inc + 1;
    else if(a > b && inc < 0)
      return (a-b) / abs(inc) + 1;
    else
      return 0;
    /*
     * Return the value for the latest iteration.
     */
  } else {
    if(inc > 0)
      UINT_VARIABLE(value)->uint = a + (unsigned) multiplier * (unsigned)inc;
    else
      UINT_VARIABLE(value)->uint = a - (unsigned) multiplier * (unsigned)(-inc);
    return 0;
  };
}

/*.......................................................................
 * Define a signed int datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         value of a signed int variable is within
 *                         the domain of the datatype.
 *  iter_fn    DT_ITER(*)  An optional iterator function for use by
 *                         the 'do' command.
 *  incr_dt       char *   The name of the datatype that the user should
 *                         use to specify do-loop increments to iter_fn().
 *  allow_negation int     If true, allow the use of the unary-minus
 *                         scoperator. This should only be specified as
 *                         true if the range of allowed values has the same
 *                         extends the same amount above and below zero.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_IntDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			  DT_ITER(*iter_fn), char *incr_name,
			  int allow_negation)
{
  DataType *dt;              /* The object to be returned */
  IntContext *context=NULL;  /* The type-specific context data */
  /*
   * Check the arguments.
   */
  if(!sc || !name) {
    lprintf(stderr, "add_IntDataType: Invalid argument(s).\n");
    return NULL;
  };

  // If the unity minus scoperator is desired, allocate a callback
  // object for recording a unary-minus scoperator for use in
  // sc_parse_intp().

  context = (IntContext* )new_ScriptObject(sc, NULL, sizeof(IntContext));
  if(!context)
    return NULL;

  context->add_op = NULL;
  context->sub_op = NULL;
  context->add_dt = NULL;

  // Create the datatype and add it to the symbol table.

  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(IntVariable),
		    check_fn, sc_parse_int, sc_const_int, sc_print_int,
		    sc_equal_int, sc_gt_int, 0, iter_fn, incr_name);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;

  // We have to add the addition context after the variable has been
  // created, or find_DataType() will fail

  context->add_dt = find_DataType(sc, NULL, "Integer");
  if(!context->add_dt) {
    lprintf(stderr, "new_DataType(%s): Can't find the Integer datatype.\n",
	    name);
    return NULL;
  };

  // Create the unary-minus scoperator?

  if(allow_negation) {
    context->neg_op = new_ScOperator(sc, dt, 1, sc_negate_int);
    if(!context->neg_op)
      return NULL;
  };

  // Create the addition and subtraction operators.

  context->add_op = new_ScOperator(sc, dt, 2, sc_add_int);
  if(!context->add_op)
    return NULL;

  context->sub_op = new_ScOperator(sc, dt, 2, sc_sub_int);
  if(!context->sub_op)
    return NULL;

  sc->insert(ScriptDataType(name, DT_INT, context));
  
  return dt;
}

/*.......................................................................
 * Parse a simple integer expression.
 */
static DT_PARSE(sc_parse_int)
{
  IntContext *ic = (IntContext* )dt->context;   /* Type-specific data */
  int negate = 0;                               /* The number of '-' scoperators seen */
  
  // Locate the start of the expression.

  if(input_skip_white(stream, 1, 0))
    return 1;
  
  // Check for one or more unary minus scoperators if allowed for this
  // datatype.

  if(ic && ic->neg_op) {
    while(stream->nextc == '-') {
      negate++;
      if(input_skip_space(stream, 1, 1))
	return 1;
    };
  };
  
  // Parse the following int operand.

  if(parse_operand(sc, dt, 0, stream, e))
    return 1;
  
  // If the operand was preceded by an odd number of unary-minus
  // scoperators, push the negation function onto the expression
  // stack.

  if((negate%2) && !add_OpFnOper(sc, e, ic->neg_op))
    return 1;

  // Now read the optional addition and/or subtraction expression.

  while(1) {
    int doadd;    /* True for '+', false for '-' */
    
    // Find the start of the next term.

    if(input_skip_space(stream, 1, 0))
      return 1;
    
    // Check for and skip any following + or - operator.

    switch(stream->nextc) {
    case '+':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 1;
      break;
    case '-':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 0;
      break;
    default:
      return 0;   /* End of expression */
      break;
    };
    
    // Read the following Double operand.

    if(parse_operand(sc, ic->add_dt, 0, stream, e))
      return 1;
    
    // Push the specified addition/subtraction operator onto the
    // stack. This takes the current value of the date and the new
    // operand as its arguments.

    if(!add_OpFnOper(sc, e, doadd ? ic->add_op : ic->sub_op))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Parse a signed integer constant.
 */
static DT_CONST(sc_const_int)
{
  Variable *var;        /* The variable that will contain the number */
  long sl;              /* The literal value */
  /*
   * Read the number from the input stream.
   */
  if(input_long(stream, 0, 1, &sl))
    return input_error(stream, 1, "Missing integer '%s' value.\n", dt->name);
  /*
   * Record the number in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  INT_VARIABLE(var)->i = sl;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a signed int variable.
 */
static DT_PRINT(sc_print_int)
{
  return output_long(output, OUT_DECIMAL, "", 0, 0,
		     INT_VARIABLE(var)->i);
}

/*.......................................................................
 * Test two int variables for equality.
 */
DT_RELFN(sc_equal_int)
{
  return INT_VARIABLE(va)->i == INT_VARIABLE(vb)->i;
}

/*.......................................................................
 * Test whether the value of one int variable is greater than a second.
 */
DT_RELFN(sc_gt_int)
{
  return INT_VARIABLE(va)->i > INT_VARIABLE(vb)->i;
}

/*.......................................................................
 * This is a do-loop iterator function suitable for use with datatypes
 * in which both the datatype itself, and its incrementing datatype,
 * were created by add_IntDataType().
 */
DT_ITER(sc_iterate_int)
{
  int a = INT_VARIABLE(first)->i;
  int b = INT_VARIABLE(last)->i;
  int inc = INT_VARIABLE(step)->i;
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    if(inc==0 || a==b || (b-a)/inc < 0.0)
      return 0;
    else
      return (b-a)/inc + 1;
    /*
     * Return the value for the latest iteration.
     */
  } else {
    INT_VARIABLE(value)->i = a + multiplier * inc;
    return 0;
  };
}

/*.......................................................................
 * Define the int-negation scoperator function.
 */
static OPER_FN(sc_negate_int)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1;        /* The input argument */
  /*
   * Get the single argument.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  /*
   * Switch the signn of the argument.
   */
  INT_VARIABLE(result)->i = - INT_VARIABLE(v1)->i;
  return 0;
}

/*.......................................................................
 * Define the date addition operator function. This is a binary operator
 * that expects a date value on its left and a an interval value on its
 * right. The result is a new date.
 */
static OPER_FN(sc_add_int)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  
  // Get the two arguments.

  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  
  // Add the ints, and record the result for return.

  INT_VARIABLE(result)->i = INT_VARIABLE(v1)->i + INT_VARIABLE(v2)->i;
  return 0;
}

/*.......................................................................
 * Define the date subtraction operator function. This is a binary
 * operator that expects a date value on its left and an interval value
 * on its right. The result is a new date.
 */
static OPER_FN(sc_sub_int)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  
  // Get the two arguments.

  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  
  // Add the ints, and record the result for return.

  INT_VARIABLE(result)->i = INT_VARIABLE(v1)->i - INT_VARIABLE(v2)->i;
  return 0;
}

/*.......................................................................
 * Define a double-precision datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         value of an unsigned int variable is within
 *                         the domain of the datatype.
 *  iter_fn    DT_ITER(*)  An optional iterator function for use by
 *                         the 'do' command.
 *  incr_name     char *   The name of the datatype that the user should
 *                         use to specify do-loop increments to iter_fn().
 *  allow_negation int     If true, allow the use of the unary-minus
 *                         scoperator. This should only be specified as
 *                         true if the range of allowed values has the same
 *                         extends the same amount above and below zero.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_DoubleDataType(Script *sc, char *name, DT_CHECK(*check_fn),
			     DT_ITER(*iter_fn), char *incr_name,
			     int allow_negation)
{
  DataType *dt;                 /* The object to be returned */
  DoubleContext *context=NULL;  /* The type-specific context data */
  
  // Check the arguments.

  if(!sc || !name) {
    lprintf(stderr, "add_DoubleDataType: Invalid argument(s).\n");
    return NULL;
  };
  
  // If the unity minus scoperator is desired, allocate a callback
  // object for recording a unary-minus scoperator for use in
  // sc_parse_double().

  context = (DoubleContext*)new_ScriptObject(sc, NULL, sizeof(DoubleContext));
  if(!context)
    return NULL;

  context->add_op = NULL;
  context->sub_op = NULL;
  context->add_dt = NULL;

  // Create the datatype and add it to the symbol table.

  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(DoubleVariable),
		    check_fn, sc_parse_double, sc_const_double, sc_print_double,
		    sc_equal_double, sc_gt_double, 0, iter_fn, incr_name);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;

  // We have to add the addition context after the variable has been
  // created, or find_DataType() will fail

  context->add_dt = find_DataType(sc, NULL, "Double");
  if(!context->add_dt) {
    lprintf(stderr, "new_DataType(%s): Can't find the Double datatype.\n",
	    name);
    return NULL;
  };

  // Create the unary-minus scoperator?

  if(allow_negation) {
    context->neg_op = new_ScOperator(sc, dt, 1, sc_negate_double);
    if(!context->neg_op)
      return NULL;
  };

  // Create the addition and subtraction operators.

  context->add_op = new_ScOperator(sc, dt, 2, sc_add_double);
  if(!context->add_op)
    return NULL;

  context->sub_op = new_ScOperator(sc, dt, 2, sc_sub_double);
  if(!context->sub_op)
    return NULL;

  sc->insert(ScriptDataType(name, DT_DOUBLE, context));
  
  return dt;
}

/*.......................................................................
 * Parse a simple double expression.
 */
static DT_PARSE(sc_parse_double)
{
  DoubleContext *dble = (DoubleContext* )dt->context; /* Type-specific data */
  int negate = 0;                    /* The number of '-' scoperators seen */
  
  // Locate the start of the expression.

  if(input_skip_white(stream, 1, 0))
    return 1;
  
  // Check for one or more unary minus scoperators if allowed for this
  // datatype.

  if(dble && dble->neg_op) {
    while(stream->nextc == '-') {
      negate++;
      if(input_skip_space(stream, 1, 1))
	return 1;
    };
  };
  
  // Parse the following double operand.

  if(parse_operand(sc, dt, 0, stream, e))
    return 1;
  
  // If the operand was preceded by an odd number of unary-minus
  // scoperators, push the negation function onto the expression
  // stack.

  if((negate%2) && !add_OpFnOper(sc, e, dble->neg_op))
    return 1;

  // Now read the optional addition and/or subtraction expression.

  while(1) {
    int doadd;    /* True for '+', false for '-' */
    
    // Find the start of the next term.

    if(input_skip_space(stream, 1, 0))
      return 1;
    
    // Check for and skip any following + or - operator.

    switch(stream->nextc) {
    case '+':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 1;
      break;
    case '-':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 0;
      break;
    default:
      return 0;   /* End of expression */
      break;
    };
    
    // Read the following Double operand.

    if(parse_operand(sc, dble->add_dt, 0, stream, e))
      return 1;
    
    // Push the specified addition/subtraction operator onto the
    // stack. This takes the current value of the date and the new
    // operand as its arguments.

    if(!add_OpFnOper(sc, e, doadd ? dble->add_op : dble->sub_op))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Parse a double-precision constant.
 */
static DT_CONST(sc_const_double)
{
  Variable *var;        /* The variable that will contain the number */
  double d;             /* The floating point read from the input stream */
  /*
   * Read the number from the input stream.
   */
  if(input_double(stream, 0, &d))
    return input_error(stream, 1, "Missing floating-point '%s' value.\n",
		       dt->name);
  /*
   * Record the number in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  DOUBLE_VARIABLE(var)->d = d;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a double-precision variable.
 */
static DT_PRINT(sc_print_double)
{
  return output_double(output, "", 0, DBL_DIG, 'g', DOUBLE_VARIABLE(var)->d);
}

/*.......................................................................
 * Test two double variables for equality.
 */
DT_RELFN(sc_equal_double)
{
  return DOUBLE_VARIABLE(va)->d == DOUBLE_VARIABLE(vb)->d;
}

/*.......................................................................
 * Test whether the value of one double variable is greater than a second.
 */
DT_RELFN(sc_gt_double)
{
  return DOUBLE_VARIABLE(va)->d > DOUBLE_VARIABLE(vb)->d;
}

/*.......................................................................
 * This is a do-loop iterator function suitable for use with datatypes
 * in which both the datatype itself, and its incrementing datatype,
 * were created by add_DoubleDataType().
 */
DT_ITER(sc_iterate_double)
{
  double a = DOUBLE_VARIABLE(first)->d;
  double b = DOUBLE_VARIABLE(last)->d;
  double inc = DOUBLE_VARIABLE(step)->d;
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    if(inc==0.0 || a==b || (b-a)/inc < 0.0)
      return 0;
    else
      return (int)(floor((b-a)/inc) + 1);
    /*
     * Return the value for the latest iteration.
     */
  } else {
    DOUBLE_VARIABLE(value)->d = a + multiplier * inc;
    return 0;
  };
}

/*.......................................................................
 * Define the double-negation scoperator function.
 */
static OPER_FN(sc_negate_double)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1;        /* The input argument */
  /*
   * Get the single argument.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  /*
   * Switch the signn of the argument.
   */
  DOUBLE_VARIABLE(result)->d = - DOUBLE_VARIABLE(v1)->d;
  return 0;
}

/*.......................................................................
 * Define the date addition operator function. This is a binary operator
 * that expects a date value on its left and a an interval value on its
 * right. The result is a new date.
 */
static OPER_FN(sc_add_double)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  
  // Get the two arguments.

  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  
  // Add the doubles, and record the result for return.

  DOUBLE_VARIABLE(result)->d = DOUBLE_VARIABLE(v1)->d + DOUBLE_VARIABLE(v2)->d;
  return 0;
}

/*.......................................................................
 * Define the date subtraction operator function. This is a binary
 * operator that expects a date value on its left and an interval value
 * on its right. The result is a new date.
 */
static OPER_FN(sc_sub_double)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */

  // Get the two arguments.

  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  
  // Add the doubles, and record the result for return.

  DOUBLE_VARIABLE(result)->d = DOUBLE_VARIABLE(v1)->d - DOUBLE_VARIABLE(v2)->d;
  return 0;
}

/*.......................................................................
 * Define a sexagesimal datatype and install it in the current scope.
 * The units of the returned number are those of the first sexagesimal
 * component. For example if one entered -12:30:36.0, then the recorded
 * value would be -12.51.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         resulting double value is within the domain
 *                         of the datatype.
 *  iter_fn    DT_ITER(*)  An optional iterator function for use by
 *                         the 'do' command.
 *  incr_name     char *   The name of the datatype that the user should
 *                         use to specify do-loop increments to iter_fn().
 *  allow_negation int     If true, allow the use of the unary-minus
 *                         scoperator. This should only be specified as
 *                         true if the range of allowed values has the same
 *                         extends the same amount above and below zero.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_SexagesimalDataType(Script *sc, char *name, DT_CHECK(*check_fn),
				  DT_ITER(*iter_fn), char *incr_name,
				  int allow_negation)
{
  DataType *dt;                 /* The object to be returned */
  SexagesimalContext *context=NULL;  /* The type-specific context data */
  
  // Check the arguments.

  if(!sc || !name) {
    lprintf(stderr, "add_SexagesimalDataType: Invalid argument(s).\n");
    return NULL;
  };
  
  // If the unity minus scoperator is desired, allocate a callback
  // object for recording a unary-minus scoperator for use in
  // sc_parse_sexagesimal().

  context = (SexagesimalContext* )new_ScriptObject(sc, NULL, sizeof(SexagesimalContext));
  if(!context)
    return NULL;
  
  context->add_op = NULL;
  context->sub_op = NULL;
  context->add_dt = NULL;

  // Create the datatype and add it to the symbol table.

  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(DoubleVariable),
		    check_fn, sc_parse_sexagesimal, sc_const_sexagesimal,
		    sc_print_sexagesimal, sc_equal_double, sc_gt_double, 0,
		    iter_fn, incr_name);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;

  // We have to add the addition context after the variable has been
  // created, or find_DataType() will fail

  context->add_dt = find_DataType(sc, NULL, "Sexagesimal");
  if(!context->add_dt) {
    lprintf(stderr, "new_DataType(%s): Can't find the Sexagesimal datatype.\n",
	    name);
    return NULL;
  };

  // Create the unary-minus scoperator?

  if(allow_negation) {
    context->neg_op = new_ScOperator(sc, dt, 1, sc_negate_double);
    if(!context->neg_op)
      return NULL;
  };

  // Create the addition and subtraction operators.

  context->add_op = new_ScOperator(sc, dt, 2, sc_add_double);
  if(!context->add_op)
    return NULL;

  context->sub_op = new_ScOperator(sc, dt, 2, sc_sub_double);
  if(!context->sub_op)
    return NULL;

  sc->insert(ScriptDataType(name, DT_SEXAGESIMAL, context));
  
  return dt;
}

/*.......................................................................
 * Parse a simple sexagesimal expression.
 */
static DT_PARSE(sc_parse_sexagesimal)
{
  SexagesimalContext *s = (SexagesimalContext* )dt->context;/* Type-specific
							       data */
  int negate = 0;                      /* The number of '-' scoperators seen */
  
  // Locate the start of the expression.

  if(input_skip_white(stream, 1, 0))
    return 1;
  
  // Check for one or more unary minus scoperators if allowed for this
  // datatype.

  if(s && s->neg_op) {
    while(stream->nextc == '-') {
      negate++;
      if(input_skip_space(stream, 1, 1))
	return 1;
    };
  };
  
  // Parse the following double operand.

  if(parse_operand(sc, dt, 0, stream, e))
    return 1;
  
  // If the operand was preceded by an odd number of unary-minus
  // scoperators, push the negation function onto the expression
  // stack.

  if((negate%2) && !add_OpFnOper(sc, e, s->neg_op))
    return 1;

  // Now read the optional addition and/or subtraction expression.

  while(1) {
    int doadd;    /* True for '+', false for '-' */
    
    // Find the start of the next term.

    if(input_skip_space(stream, 1, 0))
      return 1;
    
    // Check for and skip any following + or - operator.

    switch(stream->nextc) {
    case '+':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 1;
      break;
    case '-':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 0;
      break;
    default:
      return 0;   /* End of expression */
      break;
    };
    
    // Read the following Double operand.

    if(parse_operand(sc, s->add_dt, 0, stream, e))
      return 1;
    
    // Push the specified addition/subtraction operator onto the
    // stack. This takes the current value of the date and the new
    // operand as its arguments.

    if(!add_OpFnOper(sc, e, doadd ? s->add_op : s->sub_op))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Parse a sexagesimal constant.
 */
static DT_CONST(sc_const_sexagesimal)
{
  Variable *var;        /* The variable that will contain the constant */
  double d;             /* The sexagesimal number read from the input stream */
  /*
   * Read the angle from the input stream.
   */
  if(input_sexagesimal(stream, 1, &d))
    return 1;
  /*
   * Record the angle in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  DOUBLE_VARIABLE(var)->d = d;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a sexagesimal variable.
 */
static DT_PRINT(sc_print_sexagesimal)
{
  return output_sexagesimal(output, "#", 0, 0, 4, DOUBLE_VARIABLE(var)->d);
}

/*.......................................................................
 * Define an immutable-string datatype.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  quoted         int     If true, strings must be enclosed in
 *                         double quotes. 
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         value of the string is acceptable.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_StringDataType(Script *sc, char *name, int quoted,
			     DT_CHECK(*check_fn))
{
  DataType *dt;        /* The object to be returned */
  StringType *context; /* The parsing attributes of the string */
  if(!sc || !name) {
    lprintf(stderr, "add_StringDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (StringType* )new_ScriptObject(sc, NULL, sizeof(StringType));
  if(!context)
    return NULL;

  context->add_op = NULL;
  context->add_dt = NULL;

  // Record the parsing attributes of the string.

  context->quoted = quoted;
  
  // Create the datatype and add it to the symbol table.

  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(StringVariable),
		    check_fn, sc_parse_string, sc_const_string, sc_print_string,
		    sc_equal_string, 0, sc_in_string, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;

  // We have to add the addition context after the variable has been
  // created, or find_DataType() will fail

  context->add_dt = find_DataType(sc, NULL, "String");
  if(!context->add_dt) {
    lprintf(stderr, "new_DataType(%s): Can't find the String datatype.\n",
	    name);
    return NULL;
  };
  
  // Create the addition operator

  context->add_op = new_ScOperator(sc, dt, 2, sc_add_string);
  if(!context->add_op)
    return NULL;

  sc->insert(ScriptDataType(name, DT_STRING, context));
  
  return dt;
}

/*.......................................................................
 * Parse a string constant.
 */
static DT_CONST(sc_const_string)
{
  Variable *var;        /* The variable that will contain the constant */
  char *s;              /* The string read from the input stream */
  StringType *string = (StringType* )dt->context;
  /*
   * Are quotes mandatory?
   */
  if(string->quoted && stream->nextc != '"') {
    input_error(stream, 1, "Missing string expression.\n");
    return 1;
  };
  /*
   * Read the string from the input stream.
   */
  if((string->quoted || stream->nextc=='"') ?
     input_quoted_string(stream, 1) :
     input_literal(stream,  1, "([{",  ")]}",  not_separator_char, ""))
    return 1;
  /*
   * Allocate a copy of the string from the string-segment of the
   * program.
   */
  s = new_ScriptString(sc, stream->work);
  if(!s)
    return 1;
  /*
   * Record the string in a new variable.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var)
    return 1;
  /*
   * Initialize the variable.
   */
  STRING_VARIABLE(var)->string = s;
  /*
   * Push the variable onto the expression stack.
   */
  if(!add_LoadOper(sc, e, var))
    return 1;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a string variable.
 */
static DT_PRINT(sc_print_string)
{
  StringType *context = (StringType* )var->type->dt->context;
  char *string = STRING_VARIABLE(var)->string;
  /*
   * Enclose the string between quotes if it was created as a quoted
   * string or if it contains special characters.
   */
  if(context->quoted || strcspn(string, "()[]{} \t,\n") != strlen(string)) {
    char *sptr, *eptr; /* The start and end of a string segment to be output */
    printf("%s\n", string);
    if(write_OutputStream(output, "\""))
      return 1;
    /*
     * Escape any '"' and '\' characters found in the string.
     */
    for(sptr=string; *(eptr=sptr + strcspn(sptr, "\"\\")); sptr=eptr+1) {
      if(nwrite_OutputStream(output, sptr, (eptr - sptr)) ||
	 write_OutputStream(output, *eptr=='"' ? "\\\"" : "\\\\"))
	return 1;
    };
    if(write_OutputStream(output, sptr) ||
       write_OutputStream(output, "\""))
      return 1;
  } else {
    if(write_OutputStream(output, string))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Test two string variables for equality.
 */
DT_RELFN(sc_equal_string)
{
  return strcmp(STRING_VARIABLE(va)->string, STRING_VARIABLE(vb)->string)==0;
}

/*.......................................................................
 * See if the second of two string variables contains the first.
 */
DT_RELFN(sc_in_string)
{
  return strstr(STRING_VARIABLE(va)->string, STRING_VARIABLE(vb)->string)!=NULL;
}

/*.......................................................................
 * Parse a simple string expression.
 */
static DT_PARSE(sc_parse_string)
{
  StringType* str = (StringType* )dt->context; /* Type-specific data */
  
  // Locate the start of the expression.

  if(input_skip_white(stream, 1, 0))
    return 1;
  
  // Parse the following double operand.

  if(parse_operand(sc, dt, 0, stream, e))
    return 1;
  
  // Now read the optional addition and/or subtraction expression.

  while(1) {
    int doadd;    /* True for '+', false for '-' */
    
    // Find the start of the next term.

    if(input_skip_space(stream, 1, 0))
      return 1;
    
    // Check for and skip any following + or - operator.

    switch(stream->nextc) {
    case '+':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 1;
      break;
    case '-':
      if(input_skip_white(stream, 1, 1))
	return 1;
      doadd = 0;
      break;
    default:
      return 0;   /* End of expression */
      break;
    };
    
    // Read the following String operand.

    if(parse_operand(sc, str->add_dt, 0, stream, e))
      return 1;
    
    // Push the specified addition/subtraction operator onto the
    // stack. This takes the current value of the date and the new
    // operand as its arguments.

    if(!add_OpFnOper(sc, e, str->add_op))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Define the string addition operator function. This is a binary
 * operator that expects a string value on its left and a string value
 * on its right. The result is a new string
 */
static OPER_FN(sc_add_string)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  
  // Get the two arguments.

  node = args->head;
  v1 = (Variable* )node->data;

  node = node->next;
  v2 = (Variable* )node->data;
  
  // Cancatenate the strings, and record the result for return.

  std::ostringstream os;
  os << STRING_VARIABLE(v1)->string << STRING_VARIABLE(v2)->string;

  // Allocate a copy of the string from the string-segment of the
  // program.

  char* s = new_ScriptString(sc, (char*)os.str().c_str());

  if(!s)
    return 1;

  STRING_VARIABLE(result)->string = s;

  return 0;
}

/*.......................................................................
 * Define a keyword datatype.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  fold           int     If true, fold characters to lower case.
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         value of the keyword-string is acceptable.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_KeywordDataType(Script *sc, char *name, int fold,
			      DT_CHECK(*check_fn))
{
  DataType *dt;         /* The object to be returned */
  KeywordType *context; /* The parsing attributes of the keyword */
  if(!sc || !name) {
    lprintf(stderr, "add_KeywordDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (KeywordType* )new_ScriptObject(sc, NULL, sizeof(KeywordType));
  if(!context)
    return NULL;
  /*
   * Record the parsing attributes of the string.
   */
  context->fold = fold;
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(StringVariable),
		    check_fn, 0, const_keyword, print_keyword,
		    equal_keyword, 0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(name, DT_STRING, context));
  
  return dt;
}

/*.......................................................................
 * Parse a keyword constant.
 */
static DT_CONST(const_keyword)
{
  Variable *var;        /* The variable that will contain the constant */
  char *s;              /* The string read from the input stream */
  KeywordType *keyword = (KeywordType* )dt->context;
  /*
   * Read the keyword from the input stream.
   */
  if(input_keyword(stream, 1, keyword->fold))
    return 1;
  /*
   * Allocate a copy of the string from the string-segment of the
   * program.
   */
  s = new_ScriptString(sc, stream->work);
  if(!s)
    return 1;
  /*
   * Record the string in a new variable.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var)
    return 1;
  /*
   * Initialize the variable.
   */
  STRING_VARIABLE(var)->string = s;
  /*
   * Push the variable onto the expression stack.
   */
  if(!add_LoadOper(sc, e, var))
    return 1;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a keyword variable.
 */
static DT_PRINT(print_keyword)
{
  return write_OutputStream(output, STRING_VARIABLE(var)->string);
}

/*.......................................................................
 * Test two string variables for equality.
 */
static DT_RELFN(equal_keyword)
{
  return strcmp(STRING_VARIABLE(va)->string, STRING_VARIABLE(vb)->string)==0;
}

/*.......................................................................
 * Define a pathname datatype.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 *  check_fn  CHECK_FN(*)  The function to call to check whether the
 *                         value of the string is acceptable.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_PathDataType(Script *sc, char *name, DT_CHECK(*check_fn))
{
  DataType *dt;     /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_PathDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(StringVariable),
		    check_fn, 0, const_path, print_path, sc_equal_string, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(name, DT_STRING));
  
  return dt;
}

/*.......................................................................
 * Parse a string constant.
 */
static DT_CONST(const_path)
{
  Variable *var;        /* The variable that will contain the constant */
  char *pathname;       /* The expanded path name */
  char *string;         /* The script copy of pathname[] */
  /*
   * Read the path as a literal string.
   */
  if(stream->nextc=='"' ?
     input_quoted_string(stream, 0) : 
     input_literal(stream,  0, "([{",  ")]}",  not_separator_char, ""))
    return input_error(stream, 1, "Missing file name.\n");
  /*
   * Handle home-directory substitution.
   */
  pathname = new_pathname("", stream->work);
  if(!pathname)
    return 1;
  /*
   * Allocate a copy of the pathname from the string-segment of the
   * program.
   */
  string = new_ScriptString(sc, pathname);
  free(pathname);
  if(!string)
    return 1;
  /*
   * Record the pathname string in a new variable.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var)
    return 1;
  /*
   * Initialize the variable.
   */
  STRING_VARIABLE(var)->string = string;
  /*
   * Push the variable onto the expression stack.
   */
  if(!add_LoadOper(sc, e, var))
    return 1;
  /*
   * Validate the domain of the variable.
   */
  if(dt->check_fn && dt->check_fn(sc, var, stream))
    return 1;
  return 0;
}

/*.......................................................................
 * Print a string variable.
 */
static DT_PRINT(print_path)
{
  return write_OutputStream(output, STRING_VARIABLE(var)->string);
}

/*.......................................................................
 * This is a wrapper around add_GroupDataType() suitable to be called
 * from C with a string containing the declaration.
 *
 * Input:
 *  sc         Script *  The host script.
 *  declaration  char *  The group datatype declaration.
 * Output:
 *  return   DataType *  The requested datatype, or NULL on error.
 */
DataType *add_BuiltinGroupDataType(Script *sc, char *declaration)
{
  DataType *dt;    /* The datatype to be returned */
  /*
   * Check arguments.
   */
  if(!sc || !declaration) {
    lprintf(stderr, "add_BuiltinGroupDataType: NULL argument(s).\n");
    return NULL;
  };
  /*
   * Place an input-stream wrapper around the declaration string.
   */
  if(open_StringInputStream(sc->input, 0, declaration))
    return NULL;
  /*
   * Parse the declaration.
   */
  dt = add_GroupDataType(sc, sc->input);
  /*
   * Discard the stream wrapper.
   */
  close_InputStream(sc->input);
  if(!dt)
    return NULL;
  return dt;
}

/*.......................................................................
 * Define a user-defined group datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_GroupDataType(Script *sc, InputStream *stream)
{
  GroupType *context;  /* The group-specific context of the datatype */
  DataType *dt;        /* The object to be returned */
  if(!sc || !stream) {
    lprintf(stderr, "add_GroupDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (GroupType* )new_ScriptObject(sc, NULL, sizeof(GroupType));
  if(!context)
    return NULL;
  /*
   * Initialize the group-type context data.
   */
  context->name = NULL;
  context->fields = NULL;
  context->nfield = 0;
  /*
   * Create a list in which to record the group-field variable declarations.
   */
  context->fields = new_TypeSpecList(sc);
  if(!context->fields)
    return NULL;
  /*
   * Read the name of the new type.
   */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_keyword(stream, 0, 0)) {
    input_error(stream, 1, "Missing group name.\n");
    return NULL;
  };
  /*
   * Record the name.
   */
  context->name = new_ScriptString(sc, stream->work);
  if(!context->name)
    return NULL;
  /*
   * Group field-declarations are delimited by braces.
   */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc != '{') {
    input_error(stream, 1, "Group declarations must be enclosed in braces.\n");
    return NULL;
  };
  /*
   * Read one of more field declarations separated by spaces.
   */
  do {
    TypeSpec *field;   /* The declaration of a group field */
    /*
     * Skip the '{' before the first declaration and the comma preceding
     * subsequent declarations.
     */
    if(input_skip_white(stream, 1, 1))
      return NULL;
    /*
     * Parse the declaration of the next field and add it to the
     * list of group fields.
     */
    field = parse_TypeSpec(sc, stream, NULL);
    if(!field || append_TypeSpec(sc, context->fields, field)==NULL)
      return NULL;
    /*
     * Count group fields.
     */
    context->nfield++;
    /*
     * If there are any further declarations then the next token should
     * be a comma.
     */
    if(input_skip_white(stream, 1, 0))
      return NULL;
  } while(stream->nextc == ',');
  /*
   * Make sure that the declaration is correctly terminated.
   */
  if(stream->nextc != '}') {
    input_error(stream, 1, "Missing , or } character in group declaration.\n");
    return NULL;
  };
  /*
   * Skip the terminator.
   */
  if(input_skip_space(stream, 1, 1))
    return NULL;
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, context->name, DT_GROUP, context, sizeof(ListVariable),
		    0, 0, const_group, print_group, equal_group, 0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, dt->name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(context->name, DT_LIST));
  
  return dt;
}

/*.......................................................................
 * Parse a group constant.
 */
static DT_CONST(const_group)
{
  ListNode *node;       /* A node in the list of group-field declarations */
  /*
   * Get the group-specific fields of the datatype.
   */
  GroupType *gt = (GroupType* )dt->context;
  /*
   * A group value is formed of a brace-enclosed list of comma-separated
   * argument expressions. Check-for and skip the opening brace.
   */
  if(stream->nextc != '{') {
    input_error(stream, 1, "Missing { before group value.\n");
    return 1;
  };
  if(input_skip_white(stream, 1, 1))
    return 1;
  /*
   * Parse each declared field.
   */
  for(node=gt->fields->head; node; node=node->next) {
    TypeSpec *field = (TypeSpec* )node->data;
    /*
     * Record the current end of the instruction list in *e.
     */
    ListNode *old_end = get_ExprEnd(e);
    /*
     * Parse the argument.
     */
    if(parse_argument(sc, field, stream, e) ||
       input_skip_space(stream, 1, 0))
      return 1;
    /*
     * If the argument is an alias of a user variable, arrange for it
     * to be replaced with a copy. If this weren't done and the user
     * subsequently changed the aliased variable, the group member would
     * also change.
     */
    if(remove_alias(sc, e, old_end))
      return 1;
    /*
     * Skip the inter-argument comma.
     */
    if(node->next) {
      if(stream->nextc != ',') {
	input_error(stream, 1,
		    "A comma was expected before the next group field.\n");
	return 1;
      };
      if(input_skip_white(stream, 1, 1))
	return 1;
    };
  };
  /*
   * Check for and skip the closing brace.
   */
  if(stream->nextc != '}') {
    input_error(stream, 1,
		"A } character is needed to terminate a group definition.\n");
    return 1;
  };
  if(input_skip_space(stream, 1, 1))
    return 1;
  /*
   * Add a group-assembly scoperator to the instruction list.
   */
  if(!add_GroupOper(sc, e, dt))
    return 1;
  return 0;
}

/*.......................................................................
 * Print the value of a group variable.
 */
static DT_PRINT(print_group)
{
  ListNode *node;   /* A node of the list of group-field variables */
  /*
   * Enclose the list of fields in braces.
   */
  if(write_OutputStream(output, "{"))
    return 1;
  for(node=LIST_VARIABLE(var)->list->head; node; node=node->next) {
    Variable *field = (Variable* )node->data;
    if(print_variable(sc, output, field))
      return 1;
    if(node->next && write_OutputStream(output, ","))
      return 1;
  };
  if(write_OutputStream(output, "}"))
    return 1;
  return 0;
}

/*.......................................................................
 * Test two group variables (of the same type) for equality.
 */
static DT_RELFN(equal_group)
{
  /*
   * Get the heads of the member-variable lists of the two group variables.
   */
  ListNode *na = LIST_VARIABLE(va)->list->head;
  ListNode *nb = LIST_VARIABLE(vb)->list->head;
  /*
   * Traverse the two lists of group members in parallel, applying
   * appropriate equality functions to each pair of members. Stop at
   * the first difference, if any.
   */
  for( ; na; na=na->next, nb=nb->next) {
    Variable *vna = (Variable* )na->data;
    Variable *vnb = (Variable* )nb->data;
    if(!vna->type->dt->eq_fn(vna, vnb))
      return 0;
  };
  /*
   * No differences found, so the variables have equal values.
   */
  return 1;
}

/*.......................................................................
 * Return true if two list variables have equal values.
 */
static DT_RELFN(equal_list)
{
  /*
   * Get the heads of the lists of the two variables.
   */
  ListNode *na = LIST_VARIABLE(va)->list->head;
  ListNode *nb = LIST_VARIABLE(vb)->list->head;
  /*
   * Traverse the two lists of list members in parallel, applying
   * appropriate equality functions to each pair of members. Stop at
   * the first difference, if any.
   */
  for( ; na && nb; na=na->next, nb=nb->next) {
    Variable *vna = (Variable* )na->data;
    Variable *vnb = (Variable* )nb->data;
    if(!vna->type->dt->eq_fn(vna, vnb))
      return 0;
  };
  /*
   * Do the lists differ in length? If not then no differences were
   * found.
   */
  return !na && !nb;
}

/*.......................................................................
 * Create a free-list from which to allocate DataType objects.
 */
DataTypeMem *new_DataTypeMem(Script *sc)
{
  return new_ScriptFreeList(sc, sizeof(DataType));
}

/*.......................................................................
 * Return non-zero if a character isn't a separator character.
 */
static int not_separator_char(int c)
{
  return c!='\n' && c!=',' && c!=')' && c!='}';
}

/*.......................................................................
 * Define the symbol datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_SymbolDataType(Script *sc)
{
  DataType *dt;     /* The object to be returned */
  if(!sc) {
    lprintf(stderr, "add_SymbolDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, "Symbol", DT_BUILTIN, NULL, sizeof(SymbolVariable),
		    0, 0, const_symbol, print_symbol, equal_symbol, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, dt->name, SYM_DATATYPE, dt))
    return NULL;
  
  sc->insert(ScriptDataType(dt->name, DT_SYMBOL));
  
  return dt;
}

/*.......................................................................
 * Parse a symbol constant.
 */
static DT_CONST(const_symbol)
{
  Variable *var;        /* The variable that will contain the symbol */
  SymbolVariable *sv;   /* The derived type of *var */
  Symbol *sym;          /* The symbol to be recorded */
  /*
   * Read the symbol name from the input stream.
   */
  if(input_keyword(stream, 0, 0))
    return input_error(stream, 1, "Missing symbol name.\n");
  /*
   * Lookup the symbol.
   */
  sym = find_ScriptSymbol(sc, stream, stream->work);
  if(!sym)
    return 1;
  /*
   * Push a new symbol variable onto the expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  sv = SYMBOL_VARIABLE(var);
  sv->type = (SymbolType)sym->code;
  switch(sym->code) {
  case SYM_VARIABLE:
    sv->data.var = (Variable* )sym->data;
    break;
  case SYM_FUNCTION:
    sv->data.func = (Function* )sym->data;
    break;
  case SYM_COMMAND:
    sv->data.cmd = (Command* )sym->data;
    break;
  case SYM_DATATYPE:
    sv->data.dt = (DataType* )sym->data;
    break;
  default: /* Record the names of global keywords - their symbols live */
           /*  longer than any user variables that could refer to them. */
    sv->data.keyword = sym->name;
    break;
  };
  return 0;
}

/*.......................................................................
 * Print a symbol variable.
 */
static DT_PRINT(print_symbol)
{
  SymbolVariable *sv = SYMBOL_VARIABLE(var);
  char *name;  /* The name of the symbol */
  /*
   * Get the name of the symbol.
   */
  switch(sv->type) {
  case SYM_VARIABLE:
    name = sv->data.var->type->name;
    break;
  case SYM_FUNCTION:
    name = sv->data.func->return_type->name;
    break;
  case SYM_COMMAND:
    name = sv->data.cmd->name;
    break;
  case SYM_DATATYPE:
    name = sv->data.dt->name;
    break;
  default:
    name = sv->data.keyword;
    break;
  };
  /*
   * Display the symbol name.
   */
  return write_OutputStream(output, name);
}

/*.......................................................................
 * Test two symbol variables for equality.
 */
static DT_RELFN(equal_symbol)
{
  SymbolVariable *sa = SYMBOL_VARIABLE(va);
  SymbolVariable *sb = SYMBOL_VARIABLE(vb);
  /*
   * If the two variables refer to symbols of different types then they
   * can't be equal.
   */
  if(sa->type != sb->type)
    return 0;
  /*
   * Compare the context pointers of the symbol - where relevant.
   */
  switch(sa->type) {
  case SYM_VARIABLE:
    return sa->data.var == sb->data.var;
    break;
  case SYM_FUNCTION:
    return sa->data.func == sb->data.func;
    break;
  case SYM_COMMAND:
    return sa->data.cmd == sb->data.cmd;
    break;
  case SYM_DATATYPE:
    return sa->data.dt == sb->data.dt;
    break;
  default:
    return 1;
    break;
  };
}

/*-----------------------------------------------------------------------*
 * Define the Signal datatype.
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a signal-name specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_SignalDataType(Script *sc, char *name)
{
  /*
   * Create the datatype and add it to the symbol table.
   */
  DataType *dt = new_DataType(sc, name, DT_BUILTIN, NULL,
			      sizeof(SignalVariable), 0, 0, const_signal,
			      print_signal, equal_signal, 0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a signal constant.
 */
static DT_CONST(const_signal)
{
  Variable *var;        /* The variable that will contain the specification */
  Symbol *sym;          /* The symbol table entry of the signal */
  /*
   * Read the signal name from the input stream.
   */
  if(input_keyword(stream, 0, 1))
    return input_error(stream, 1, "Missing signal name.\n");
  /*
   * Lookup the signal by name.
   */
  sym = lookup_script_signal(sc, stream->work);
  if(!sym) {
    input_error(stream, 1, "Unknown signal name: %s\n", stream->work);
    return 1;
  };
  /*
   * Record the specification in a new variable and push it onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Inititialize the variable.
   */
  SIGNAL_VARIABLE(var)->sym = sym;
  return 0;
}

/*.......................................................................
 * Print the contents of a signal specification variable.
 */
static DT_PRINT(print_signal)
{
  return write_OutputStream(output, SIGNAL_VARIABLE(var)->sym->name);
}

/*.......................................................................
 * Return true if two register-board variables have the same values.
 */
static DT_RELFN(equal_signal)
{
  return SIGNAL_VARIABLE(va)->sym == SIGNAL_VARIABLE(vb)->sym;
}
