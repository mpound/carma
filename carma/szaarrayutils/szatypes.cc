#include <float.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "carma/szautil/AntNum.h"
#include "carma/szautil/CalPos.h"
#include "carma/szautil/CanModule.h"
#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/CalTertTypes.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/DDSChannel.h"
#include "carma/szautil/IFAtten.h"
#include "carma/szautil/IFLevel.h"
#include "carma/szautil/LoOsc.h"
#include "carma/szautil/LoStage.h"
#include "carma/szautil/PointingMode.h"
#include "carma/szautil/Rx.h"

#include "carma/szaarrayutils/szacontrol.h"
#include "carma/szaarrayutils/szascript.h"
#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/pathname.h"
#include "carma/szaarrayutils/scanner.h" /* SCAN_xxx_HARDWARE_INTERVAL */
#include "carma/szaarrayutils/navigator.h"
#include "carma/szaarrayutils/szatypes.h"
#include "carma/szaarrayutils/arcfile.h"
#include "carma/szaarrayutils/scheduler.h"

#include "carma/szaarrayutils/TransactionManager.h"

#include "carma/szautil/Directives.h"

#include "carma/szautil/Thermal.h"

#include <sstream>

// Using namespace declarations.  Note that I cannot at present
// include sza::util because of a namespace conflict with DataType,
// which is most satisfactorily resolved by fully qualifying all
// sza::util types in this file.

using namespace sza::array;
using namespace std;

/*
 * Provide a macro that returns the number of elements of an array.
 */
#define DIMENSION(array) (sizeof(array)/sizeof(array[0]))

static Enumerator OptCamEn[] = {
  {"grab",    OPTCAM_GRAB},
  {"id",      OPTCAM_ID},
  {"rbc",     OPTCAM_RBC},
  {"open",    OPTCAM_OPEN},
  {"close",   OPTCAM_CLOSE},
  {"preset",  OPTCAM_PRESET},
  {"stop",    OPTCAM_STOP},
  {"on",      OPTCAM_ON},
  {"off",     OPTCAM_OFF},
  {"inc",     OPTCAM_INC},
  {"dec",     OPTCAM_DEC},
  {"low",     OPTCAM_LOW},
  {"mid",     OPTCAM_MID},
  {"high",    OPTCAM_HIGH},
  {"s100",    OPTCAM_100},
  {"s250",    OPTCAM_250},
  {"s500",    OPTCAM_500},
  {"s1000",   OPTCAM_1000},
  {"s2000",   OPTCAM_2000},
  {"s4000",   OPTCAM_4000},
  {"s10000",  OPTCAM_10000},
  {"center",  OPTCAM_CENTER}
};
int OptCamNen = DIMENSION(OptCamEn);

/*-----------------------------------------------------------------------
 * The Date datatype uses astrom.h::input_utc() to read UTC date and
 * time values like:
 *
 *  dd-mmm-yyyy hh:mm:ss.s
 *
 * where mmm is a 3-letter month name. It stores the date as a Modified
 * Julian Date in DoubleVariable's. It provides + and - operators for
 * the addition and subtraction of Time values, along with all of the
 * relational operators except the ~ operator.
 */

/*
 * An object of following type is stored with the Date datatype symbol.
 */
typedef struct {
  ScOperator *add_op;  /* An operator proc that adds a time to a date */
  ScOperator *sub_op;  /* An operator proc that subtracts a time from a date */
  DataType *add_dt;  /* The Interval datatype used in addition */
} DateContext;

static DT_PARSE(parse_date);
static DT_CONST(const_date);
static DT_PRINT(print_date);
static OPER_FN(date_add_fn);
static OPER_FN(date_sub_fn);
static DT_ITER(date_iterate);

/*-----------------------------------------------------------------------
 * The Register datatype uses regmap.h::input_RegMapReg() to read
 * standard SZA archive-register specifications (eg. board.name[3-4]).
 *
 * It supports the == and ~ relational operators.
 */
static DT_CONST(const_reg);
static DT_PRINT(print_reg);
static DT_RELFN(equal_reg);
static DT_RELFN(in_reg);

/*-----------------------------------------------------------------------
 * The Wdir datatype is used to record writable directory names.
 * The path name is stored in a StringVariable. It supports
 * tilde expansion of home directories.
 */
static DT_CHECK(check_wdir);

/*-----------------------------------------------------------------------
 * The Dir datatype is used to record the names of accessible directories.
 *
 * The path name is stored in a StringVariable. It supports
 * tilde expansion of home directories.
 */
static DT_CHECK(check_dir);

/*-----------------------------------------------------------------------
 * The IntTime datatype is used to specify the hardware integration time
 * of the SZA electronics, as a power-of-2 exponent to be used to scale
 * the basic sample interval of 4.096e-4 seconds.
 */
static DT_CHECK(check_inttime);

/*-----------------------------------------------------------------------
 * The Board datatype is used to specify an archive register board by
 * name.
 */
static DT_CONST(const_board);
static DT_PRINT(print_board);

/*-----------------------------------------------------------------------
 * The Time datatype is used to specify a time of day (UTC or LST), or
 * a time interval.
 */
static DT_CHECK(check_time);
static DT_ITER(time_iterate);

/*-----------------------------------------------------------------------
 * The Interval datatype is used to specify a time interval.
 */
static DT_CONST(const_interval);
static DT_PRINT(print_interval);

/*-----------------------------------------------------------------------
 * The WalshFunction datatype is used to select one of 32 walsh functions.
 */
static DT_CHECK(check_walsh_fn);

/*-----------------------------------------------------------------------
 * The WalshStep datatype is used to select one step of a 16-step
 * walsh function, used to control the polarization state of the receivers.
 */
static DT_CHECK(check_walshstep_fn);

/*-----------------------------------------------------------------------
 * The HeaterVoltage datatype is used to specify the target sensor voltage
 * in the feedback loop of a receiver heater.
 */
static DT_CONST(const_htr_v);
static DT_PRINT(print_htr_v);

/*-----------------------------------------------------------------------
 * The PhaseShift datatype is used to specify the target encoder position
 * of a phase-shifter motor.
 */
static DT_CONST(const_pshift);
static DT_PRINT(print_pshift);
static DT_RELFN(gt_pshift);

/*-----------------------------------------------------------------------
 * The QuadPhase datatype is used to select between the 4 possible states
 * of a quadrature phase shift network. The 4 allowed phases are
 * 0,90,180 and 270 degrees. These are stored as unsigned integers.
 */
static DT_CHECK(check_quad_phase);

/*-----------------------------------------------------------------------
 * The Source datatype is used for specification of a source by its name
 * in the source catalog.
 */
static DT_CONST(const_source);
static DT_PRINT(print_source);
static DT_RELFN(equal_source);

/*-----------------------------------------------------------------------
 * The Scan datatype is used for specification of a scan by its name
 * in the scan catalog.
 */
static DT_CONST(const_scan);
static DT_PRINT(print_scan);
static DT_RELFN(equal_scan);

/*-----------------------------------------------------------------------
 * The Latitude datatype is used to specify the latitude of a location
 * on the surface of the Earth.
 */
static DT_CHECK(check_latitude);

/*-----------------------------------------------------------------------
 * The Longitude datatype is used to specify the longitude of a location
 * on the surface of the Earth.
 */
static DT_CHECK(check_longitude);

/*-----------------------------------------------------------------------
 * The Azimuth datatype is used to specify a target azimuth for the
 * telescope. The azimuths of the compass points are N=0, E=90, S=180
 * and W=270 degrees.
 */
static DT_CHECK(check_azimuth);

/*-----------------------------------------------------------------------
 * The DeckAngle datatype is used to specify the position of the rotating
 * platform on which the dishes are mounted.
 */
static DT_CHECK(check_deckangle);

/*-----------------------------------------------------------------------
 * The Elevation datatype is used to specify the target elevation angle
 * of the telescope.
 */
static DT_CHECK(check_elevation);

/*-----------------------------------------------------------------------
 * The PointingOffset datatype is used to specify a temporary offset of
 * one of the telescope axes from the current pointing.
 */
static DT_CHECK(check_pointingoffset);

/*-----------------------------------------------------------------------
 * The Flexure datatype is used to specify the degree of gravitational
 * drooping of the telescope as a function of elevation.
 */
static DT_CHECK(check_flexure);

/*-----------------------------------------------------------------------
 * The LoFrequency datatype is used to specify the desired frequency
 * of a tunable oscillator.
 */
static DT_CHECK(check_LoFrequency);

/*-----------------------------------------------------------------------
 * The IFAtten datatype is used to specify the desired frequency
 * of a tunable oscillator.
 */
static DT_CHECK(check_IFAttenuation);
static DT_CHECK(check_IFLevel);

/*-----------------------------------------------------------------------
 * The Tilt datatype is used to specify the misalignment tilt of a
 * telescope axis.
 */
static DT_CHECK(check_tilt);

/*-----------------------------------------------------------------------
 * The Altitude datatype is used to specify the height of the telescope
 * above the standard geodetic spheroid.
 */
static DT_CHECK(check_altitude);

/*-----------------------------------------------------------------------
 * The GpibDev datatype is used to select a device on the GPIB bus.
 */
static DT_CHECK(check_gpib_dev);

/*-----------------------------------------------------------------------
 * The GpibCmd datatype is used to specify a command string to be sent
 * to a device on the GPIB bus.
 */
static DT_CHECK(check_gpib_cmd);

/*-----------------------------------------------------------------------
 * The Attenuation datatype is used to specify attenuation levels over
 * the range 0..31db.
 */
static DT_CHECK(check_attn_fn);

/*-----------------------------------------------------------------------
 * The TotalPower datatype is used to specify the target sensor voltage
 * of a set of total power detectors.
 */
static DT_CHECK(check_total_power);

/*-----------------------------------------------------------------------
 * The Board datatype is used to specify a digital I/O board by name.
 */
static DT_CONST(const_dio_board);

/*-----------------------------------------------------------------------
 * The OptCamCount datatype is used to specify a stepper motor count, or
 * on/off for the stepper motor.
 */
static DT_CONST(const_optcam);
static DT_PRINT(print_optcam);
static DT_RELFN(equal_optcam);
static DT_RELFN(gt_optcam);

/*-----------------------------------------------------------------------
 * The Schedule datatype is used for entering the name and arguments
 * of a schedule.
 */
static DT_CONST(const_script);
static DT_PRINT(print_script);
static int is_script_file(int c);
static DT_RELFN(equal_script);

/*-----------------------------------------------------------------------
 * The SlewRate datatype is used for specifying the percentage slew rate
 * of a telescope drive axis.
 */
static DT_CHECK(check_slew_rate);

/**-----------------------------------------------------------------------
 * Iterators for enumerated types
 */

static DT_ITER(rx_iterate);
static DT_ITER(ant_iterate);
static DT_ITER(dds_iterate);

/*-----------------------------------------------------------------------
 * The DcPower datatype is used to specify the target power for the
 * downconverter
 */
static DT_CONST(const_dcpower);
static DT_PRINT(print_dcpower);

/*-----------------------------------------------------------------------
 * The TransDev datatype is used for specification of a device by its name
 * in the transaction catalog.
 */
static DT_CONST(const_transdev);
static DT_PRINT(print_transdev);
static DT_RELFN(equal_transdev);


/*-----------------------------------------------------------------------
 * Create a new datatype for specifying stepper tertiary positions
 */
static DT_CONST(const_tertpos);
static DT_PRINT(print_tertpos);
static DT_RELFN(equal_tertpos);
static DT_RELFN(gt_tertpos);

//-----------------------------------------------------------------------
// Date                                                                  
//-----------------------------------------------------------------------

/*.......................................................................
 * Define a date datatype and install it in the current scope.
 *
 * Input:
 *  sc          Script *   The script environment in which to install
 *                         the datatype.
 *  name          char *   The name to give to the datatype.
 * Output:
 *  return    DataType *   The new datatype.
 */
DataType *add_DateDataType(Script *sc, char *name)
{
  DateContext *context;    /* The type-specific context data */
  DataType *dt;            /* The object to be returned */
  /*
   * Check arguments.
   */
  if(!sc || !name) {
    lprintf(stderr, "add_DateDataType: NULL argument(s).\n");
    return NULL;
  };
  /*
   * Allocate an external context object for the datatype.
   */
  context = (DateContext* )new_ScriptObject(sc, NULL, sizeof(DateContext));
  if(!context)
    return NULL;
  /*
   * Initialize the context data of the data-type.
   */
  context->add_op = NULL;
  context->sub_op = NULL;
  context->add_dt = find_DataType(sc, NULL, "Interval");
  if(!context->add_dt) {
    lprintf(stderr, "new_DataType(%s): Can't find the Interval datatype.\n",
	    name);
    return NULL;
  };
  /*
   * Create the datatype.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, context, sizeof(DoubleVariable),
		    0, parse_date, const_date, print_date, sc_equal_double,
		    sc_gt_double, 0, date_iterate, "Interval");
  if(!dt)
    return NULL;
  /*
   * Create the addition and subtraction operators.
   */
  context->add_op = new_ScOperator(sc, dt, 2, date_add_fn);
  if(!context->add_op)
    return NULL;

  context->sub_op = new_ScOperator(sc, dt, 2, date_sub_fn);
  if(!context->sub_op)
    return NULL;
  /*
   * Add the data-type to the symbol table.
   */
  if(!add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a date expression.
 */
static DT_PARSE(parse_date)
{
  DateContext *dc;      /* The date specific datatype members */
  /*
   * Get the date-specific members of the data-type.
   */
  dc = (DateContext* )dt->context;
  /*
   * Parse an expression of the form:
   *
   *   date_operand {+ time_operand} _clos_
   *
   * Eg. 15-apr-1995
   * or  15-apr-1995 + 12:34
   * or  15-apr-1995 + 12:00 + 0:5
   *
   * Start by reading the mandatory date operand.
   */
  if(parse_operand(sc, dt, 0, stream, e))
    return 1;
  /*
   * Now read the optional addition and/or subtraction expression.
   */
  while(1) {
    int doadd;    /* True for '+', false for '-' */
    /*
     * Find the start of the next term.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    /*
     * Check for and skip any following + or - operator.
     */
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
    /*
     * Read the following Time operand.
     */
    if(parse_operand(sc, dc->add_dt, 0, stream, e))
      return 1;
    /*
     * Push the specified addition/subtraction operator onto the
     * stack. This takes the current value of the date and the
     * new operand as its arguments.
     */
    if(!add_OpFnOper(sc, e, doadd ? dc->add_op : dc->sub_op))
      return 1;
  };
}

/*.......................................................................
 * Parse a date constant.
 */
static DT_CONST(const_date)
{
  Variable *var;        /* The new date-constant */
  double utc;           /* The UTC as a Modified Julian Date */
  /*
   * Parse the date and time and convert it to a Modified Julian Date.
   */
  if(input_utc(stream, 1, 0, &utc))
    return 1;
  /*
   * Record the date in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the value of the constant.
   */
  DOUBLE_VARIABLE(var)->d = utc;
  return 0;
}

/*.......................................................................
 * Print a date value.
 */
static DT_PRINT(print_date)
{
  return output_utc(output, "", 0, 0, DOUBLE_VARIABLE(var)->d);
}

/*.......................................................................
 * Define the date addition operator function. This is a binary operator
 * that expects a date value on its left and a an interval value on its
 * right. The result is a new date.
 */
static OPER_FN(date_add_fn)
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
   * Add the seconds-interval to the date, and record the result for return.
   */
  DOUBLE_VARIABLE(result)->d = DOUBLE_VARIABLE(v1)->d +
    DOUBLE_VARIABLE(v2)->d / 86400.0;
  return 0;
}

/*.......................................................................
 * Define the date subtraction operator function. This is a binary
 * operator that expects a date value on its left and an interval value
 * on its right. The result is a new date.
 */
static OPER_FN(date_sub_fn)
{
  ListNode *node;      /* A node of the argument list */
  Variable *v1, *v2;   /* The input arguments */
  double utc;          /* The result of the subtraction */
  /*
   * Get the two arguments.
   */
  node = args->head;
  v1 = (Variable* )node->data;
  node = node->next;
  v2 = (Variable* )node->data;
  /*
   * Subtract the seconds-interval from the date.
   */
  utc = DOUBLE_VARIABLE(v1)->d - DOUBLE_VARIABLE(v2)->d / 86400.0;
  /*
   * Record the result for return after applying limits.
   */
  DOUBLE_VARIABLE(result)->d = utc >= 0 ? utc : 0.0;
  return 0;
}

/*.......................................................................
 * This is a private do-loop iterator function used by Date datatypes.
 */
static DT_ITER(date_iterate)
{
  double a = DOUBLE_VARIABLE(first)->d;
  double b = DOUBLE_VARIABLE(last)->d;
  double inc = DOUBLE_VARIABLE(step)->d / 86400.0;
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

/*-----------------------------------------------------------------------*
 * Register                                                              *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new register-specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 *  regmap      RegMap *  The register map to lookup register
 *                        specifications in.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_RegisterDataType(Script *sc, char *name, ArrayMap *arraymap)
{
  DataType *dt;                   /* The object to be returned */
  
  // Create the datatype and add it to the symbol table.

  dt = new_DataType(sc, name, DT_BUILTIN, arraymap, sizeof(RegisterVariable),
		    0, 0, const_reg, print_reg, equal_reg, 0, in_reg, 0, NULL);

  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;

  return dt;
}

/*.......................................................................
 * Parse a register specification constant.
 */
static DT_CONST(const_reg)
{
  ArrayMap* arrayMap = (ArrayMap* )dt->context;  // The array map 
  RegisterVariable *regvar;       // The new variable 
  ArrRegMapReg reg;               // The register specification 
  
  // Read the register specification from the input stream.

  if(input_ArrRegMapReg(stream, 1, arrayMap, REG_INPUT_RANGE, 0, &reg))
    return 1;
  
  // Record the specification in a new variable and push it onto the
  // expression stack.

  regvar = (RegisterVariable *) new_Variable(sc, dt->atom_reg);

  if(!regvar || !add_LoadOper(sc, e, &regvar->v))
    return 1;

  regvar->regmap = reg.regmap;
  regvar->board  = reg.reg.board;
  regvar->block  = reg.reg.block;
  regvar->index  = reg.reg.index;
  regvar->nreg   = reg.reg.nreg;

  return 0;
}

/*.......................................................................
 * Print a register specification variable.
 */
static DT_PRINT(print_reg)
{
  RegisterVariable *regvar;       // The register variable to be printed 
  ArrRegMapReg reg;               // A register specification 
  
  // The register map was passed to new_DataType() to be recorded in
  // the context member of the Register datatype. Retrieve it.

  ArrayMap *arrayMap = (ArrayMap* )var->type->dt->context;
  
  // Get the derived register variable.

  regvar = REGISTER_VARIABLE(var);
  
  // Convert the register specification into the form expected by
  // output_ArrRegMapReg(), then display it.

  if(init_ArrRegMapReg(arrayMap, regvar->regmap, regvar->board, regvar->block, 
		       regvar->index, regvar->nreg, REG_PLAIN, &reg) ||
     output_ArrRegMapReg(output, arrayMap, REG_OUTPUT_RANGE, &reg))
    return 1;

  return 0;
}

/*.......................................................................
 * Return true if two register variables have the same values.
 */
static DT_RELFN(equal_reg)
{
  // Get the register variables to be compared.

  RegisterVariable *rva = REGISTER_VARIABLE(va);
  RegisterVariable *rvb = REGISTER_VARIABLE(vb);
  
  // Compare the variables.

  return 
    rva->regmap == rvb->regmap && rva->board == rvb->board && 
    rva->block  == rvb->block  && rva->index == rvb->index && 
    rva->nreg   == rvb->nreg;
}

/*.......................................................................
 * Return true if the second of two register specifications refers to
 * register elements that are covered by the first.
 */
static DT_RELFN(in_reg)
{
  // Get the register variables to be compared.

  RegisterVariable *rva = REGISTER_VARIABLE(va);
  RegisterVariable *rvb = REGISTER_VARIABLE(vb);

  // Compare the variables.

  return rva->regmap==rvb->regmap && rva->board==rvb->board && 
    rva->block == rvb->block &&
    rva->index <= rvb->index &&
    rva->index + rva->nreg >= rvb->index + rvb->nreg;
}

/*-----------------------------------------------------------------------*
 * Wdir                                                                  *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype to contain path names of writable directories,
 * and add it to the specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_WdirDataType(Script *sc, char *name)
{
  return add_PathDataType(sc, name, check_wdir);
}

/*.......................................................................
 * Check whether the value of a wdir datatype specifies a writable
 * directory.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  var        Variable *  The variable who's value is to be checked.
 *  stream  InputStream *  The stream from which the directory name was
 *                         read.
 * Output:
 *  return       int    0 - Value ok.
 *                      1 - The string does not contain the name of a
 *                          directory that we have write access to.
 */
static DT_CHECK(check_wdir)
{
  char *path;         /* The path name in var */
  /*
   * Get the pathname to be checked.
   */
  path = STRING_VARIABLE(var)->string;
  /*
   * Check that the pathname refers to a directory.
   */
  if(test_pathname(path, PATH_IS_DIR, PATH_EXE | PATH_WRITE) != NULL) {
    input_error(stream, 1, "Error: '%s' is not a writable directory.\n", path);
    return 1;
  };
  return 0;
}
/*-----------------------------------------------------------------------*
 * Dev                                                                   *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype to contain path names of devices
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DevDataType(Script *sc, char *name)
{
  return add_PathDataType(sc, name, NULL);
}

/*-----------------------------------------------------------------------*
 * Dir                                                                   *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype to contain path names of directories to which
 * we have execute permission, and add it to the specified script
 * environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DirDataType(Script *sc, char *name)
{
  return add_PathDataType(sc, name, check_dir);
}

/*.......................................................................
 * Check whether the value of a dir datatype specifies an accessible
 * directory.
 *
 * Input:
 *  sc           Script *  The host scripting environment.
 *  var        Variable *  The variable who's value is to be checked.
 *  stream  InputStream *  The stream from which the directory name was
 *                         read.
 * Output:
 *  return       int    0 - Value ok.
 *                      1 - The string does not contain the name of a
 *                          directory that we have write access to.
 */
static DT_CHECK(check_dir)
{
  char *path;         /* The path name in var */
  /*
   * Get the pathname to be checked.
   */
  path = STRING_VARIABLE(var)->string;
  /*
   * Check that the pathname refers to a directory.
   */
  if(test_pathname(path, PATH_IS_DIR, PATH_EXE) != NULL) {
    input_error(stream, 1, "Error: '%s' is not an accessible directory.\n",
		path);
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * IntTime
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying hardware integration times.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_IntTimeDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_inttime, sc_iterate_uint, "Integer");
}

/*.......................................................................
 * Check whether the value of a "IntTime" datatype specifies a legal
 * hardware accumulation interval. The interval is represented as a
 * power of 2 exponent, and this must lie between
 * SCAN_MIN_HARDWARE_INTERVAL and SCAN_MAX_HARDWARE_INTERVAL. These
 * values are enumerated in scanner.h.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_inttime)
{
  unsigned interval = UINT_VARIABLE(var)->uint;
  if(interval < SCAN_MIN_HARDWARE_INTERVAL ||
     interval > SCAN_MAX_HARDWARE_INTERVAL) {
    input_error(stream, 1,
		"Legal hardware intervals are %u..%u. You asked for %u.\n",
		SCAN_MIN_HARDWARE_INTERVAL,
		SCAN_MAX_HARDWARE_INTERVAL,
		interval);
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Board
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new register-board specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 *  regmap      RegMap *  The register map to use to lookup board
 *                        names.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_BoardDataType(Script *sc, char *name, ArrayMap *arraymap)
{
  DataType *dt;                   /* The object to be returned */
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, arraymap, sizeof(UintVariable),
		    0, 0, const_board, print_board, sc_equal_uint,
		    0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a register specification constant.
 */
static DT_CONST(const_board)
{
  Variable *var;        /* The variable that will contain the specification */
  RegMapBoard *brd;     /* The board description container */
  /*
   * The register map was passed to new_DataType() to be recorded in
   * the context member of the Board datatype. Retrieve it.
   */
  RegMap *regmap = (RegMap* )dt->context;
  /*
   * Read the board name from the input stream.
   */
  if(input_keyword(stream, 0, 1))
    return input_error(stream, 1, "Missing register-map board name.\n");
  /*
   * Look up the named board.
   */
  brd = find_RegMapBoard(regmap, stream->work);
  if(!brd) {
    return input_error(stream, 1,
		       "Error: '%s' is not the name of a known register-map board.\n",
		       stream->work);
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
  UINT_VARIABLE(var)->uint = brd->number;
  return 0;
}

/*.......................................................................
 * Print a register specification variable.
 */
static DT_PRINT(print_board)
{
  /*
   * The register map was passed to new_DataType() to be recorded in
   * the context member of the Board datatype. Retrieve it.
   */
  RegMap *regmap = (RegMap* )var->type->dt->context;
  /*
   * Get the board number.
   */
  int brd = UINT_VARIABLE(var)->uint;
  /*
   * Print the name of the board.
   */
  return write_OutputStream(output, regmap->boards_[brd]->name);
}

/*-----------------------------------------------------------------------*
 * Time
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying times.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TimeDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_time, time_iterate,
				 "Interval", 0);
}

/*.......................................................................
 * Check the validity of a time variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_time)
{
  double t = DOUBLE_VARIABLE(var)->d;
  if(t >= 24 || t < 0.0) {
    input_error(stream, 1, "Invalid time-of-day. Should be 0 <= t < 24.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * This is a do-loop iterator function used by Antenna datatypes
 *
 * These are stored as unsigned bitmasks.  We will allow only
 * increments between single-element beginning states and end states,
 * otherwise it's not clear how you'd define increments between bitmasks.
 */
static DT_ITER(ant_iterate)
{
  unsigned a = SET_VARIABLE(first)->set;
  unsigned b = SET_VARIABLE(last)->set;
  unsigned incset = UINT_VARIABLE(step)->uint;
  int ibit,nbit = 32,anset=0,bnset=0,abit=0,bbit=0,incnset=0,inc=0;
#ifdef TEST
  inc = (int)incset;
#else
  for(ibit=0;ibit < nbit;ibit++) {
    
    if(incset & 1U<<ibit) 
      inc = ibit;
    
    if(a & 1U<<ibit) 
      abit = ibit;
  }
#endif
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    /*
     * Check that the beginning and end states have only one bit set.
     */
    for(ibit=0;ibit < nbit;ibit++) {
#ifndef TEST
      if(incset & 1U<<ibit) 
	incnset++;
#endif
      if(a & 1U<<ibit) 
	anset++;
      if(b & 1U<<ibit) {
	bnset++;
	bbit = ibit;
      }
      
#ifdef TEST
      if(anset > 1 || bnset > 1 || incnset > 1) {
	lprintf(stderr,"Set variable iterators cannot have more than one bit set in the bitmask.\n");
	return 1;
      }
#else
      if(anset > 1 || bnset > 1) {
	lprintf(stderr,"Set variable iterators cannot have more than one bit set in the bitmask.\n");
	return 1;
      }
#endif
    }
    if(inc==0 || abit==bbit || (bbit-abit)/inc < 0.0)
      return 0;
    else
      return (int)(floor((double)(bbit-abit)/inc) + 1);
    /*
     * Return the value for the latest iteration.
     */
  } else {
    SET_VARIABLE(value)->set = 1U<<(abit + (multiplier * inc));
    return 0;
  };
}

/*.......................................................................
 * This is a do-loop iterator function used by DDS datatypes
 *
 * These are stored as unsigned bitmasks.  We will allow only
 * increments between single-element beginning states and end states,
 * otherwise it's not clear how you'd define increments between bitmasks.
 */
static DT_ITER(dds_iterate)
{
  unsigned a = SET_VARIABLE(first)->set;
  unsigned b = SET_VARIABLE(last)->set;
  unsigned incset = UINT_VARIABLE(step)->uint;
  int ibit,nbit = 32,anset=0,bnset=0,abit=0,bbit=0,incnset=0,inc=0;
  
  for(ibit=0;ibit < nbit;ibit++) {
    
    if(incset & 1U<<ibit) 
      inc = ibit;
    
    if(a & 1U<<ibit) 
      abit = ibit;
  }
  
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    /*
     * Check that the beginning and end states have only one bit set.
     */
    for(ibit=0;ibit < nbit;ibit++) {
      
      if(incset & 1U<<ibit) 
	incnset++;
      
      if(a & 1U<<ibit) 
	anset++;
      if(b & 1U<<ibit) {
	bnset++;
	bbit = ibit;
      }
      
      if(anset > 1 || bnset > 1) {
	lprintf(stderr,"Set variable iterators cannot have more than one bit set in the bitmask.\n");
	return 1;
      }
      
    }
    if(inc==0 || abit==bbit || (bbit-abit)/inc < 0.0)
      return 0;
    else
      return (int)(floor((double)(bbit-abit)/inc) + 1);
    /*
     * Return the value for the latest iteration.
     */
  } else {
    SET_VARIABLE(value)->set = 1U<<(abit + (multiplier * inc));
    return 0;
  };
}

/*.......................................................................
 * This is a do-loop iterator function used by Receiver datatypes
 *
 * These are stored as unsigned bitmasks.  We will allow only
 * increments between single-element beginning states and end states,
 * otherwise it's not clear how you'd define increments between bitmasks.
 */
static DT_ITER(rx_iterate)
{
  unsigned a = SET_VARIABLE(first)->set;
  unsigned b = SET_VARIABLE(last)->set;
  unsigned incset = UINT_VARIABLE(step)->uint;
  int ibit,nbit = 32,anset=0,bnset=0,abit=0,bbit=0,incnset=0,inc=0;
#ifdef TEST
  inc = (int)incset;
#else
  for(ibit=0;ibit < nbit;ibit++) {
    
    if(incset & 1U<<ibit) 
      inc = ibit;
    
    if(a & 1U<<ibit) 
      abit = ibit;
  }
#endif
  /*
   * Compute the number of steps required?
   */
  if(!value) {
    /*
     * Check that the beginning and end states have only one bit set.
     */
    for(ibit=0;ibit < nbit;ibit++) {
#ifndef TEST
      if(incset & 1U<<ibit) 
	incnset++;
#endif
      if(a & 1U<<ibit) 
	anset++;
      if(b & 1U<<ibit) {
	bnset++;
	bbit = ibit;
      }
      
#ifdef TEST
      if(anset > 1 || bnset > 1 || incnset > 1) {
	lprintf(stderr,"Set variable iterators cannot have more than one bit set in the bitmask.\n");
	return 1;
      }
#else
      if(anset > 1 || bnset > 1) {
	lprintf(stderr,"Set variable iterators cannot have more than one bit set in the bitmask.\n");
	return 1;
      }
#endif
    }
    if(inc==0 || abit==bbit || (bbit-abit)/inc < 0.0)
      return 0;
    else
      return (int)(floor((double)(bbit-abit)/inc) + 1);
    /*
     * Return the value for the latest iteration.
     */
  } else {
    SET_VARIABLE(value)->set = 1U<<(abit + (multiplier * inc));
    return 0;
  };
}

/*.......................................................................
 * This is a private do-loop iterator function used by Time datatypes.
 */
static DT_ITER(time_iterate)
{
  double a = DOUBLE_VARIABLE(first)->d;
  double b = DOUBLE_VARIABLE(last)->d;
  double inc = DOUBLE_VARIABLE(step)->d / 3600.0;  /* Seconds -> hours */
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

/*-----------------------------------------------------------------------*
 * Interval
 *-----------------------------------------------------------------------*/

/*-----------------------------------------------------------------------
 * The Interval datatype is used to specify a time interval.
 *
 * It is stored as decimal days in a DoubleVariable. It supports the
 * standard arithmentic != == <= >= < > operators.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_IntervalDataType(Script *sc, char *name)
{
  DataType *dt;    /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_DayIntervalDataType: Invalid argument(s).\n");
    return NULL;
  };
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(DoubleVariable),
		    0, 0, const_interval, print_interval, sc_equal_double,
		    sc_gt_double, 0, sc_iterate_double, name);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse an interval constant.
 */
static DT_CONST(const_interval)
{
  Variable *var;        /* The new day-interval constant */
  double secs;          /* The interval to record */
  /*
   * Parse the interval.
   */
  if(input_interval(stream, 1, &secs))
    return 1;
  /*
   * Record the interval in a new variable and push the variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the value of the constant.
   */
  DOUBLE_VARIABLE(var)->d = secs;
  return 0;
}

/*.......................................................................
 * Print an interval variable.
 */
static DT_PRINT(print_interval)
{
  return output_interval(output, "-#", 0, 2, DOUBLE_VARIABLE(var)->d);
}

//-----------------------------------------------------------------------
// Outlet Data Type
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying outlet selection 
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_OutletDataType(Script *sc, char *name)
{
  static Enumerator en[] = {

    {"1",       1},
    {"2",       2},
    {"3",       3},
    {"4",       4},

    {"sidecab", 1},
    {"ebox",    2},
    {"chiller", 3},
    {"heater",  4},

    {"c1",      1},
    {"c2",      3},

  };

  return add_ChoiceDataType(sc, name, en, DIMENSION(en), true);
}

/*-----------------------------------------------------------------------*
 * Antennas
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying antenna selections.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_AntennasDataType(Script *sc, char *name)
{
  static Enumerator ant[] = {
    {"none",  sza::util::AntNum::ANTNONE},
    {"ant0",  sza::util::AntNum::ANT0}, 
    {"ant1",  sza::util::AntNum::ANT1}, 
    {"ant2",  sza::util::AntNum::ANT2}, 
    {"ant3",  sza::util::AntNum::ANT3},
    {"ant4",  sza::util::AntNum::ANT4},
    {"ant5",  sza::util::AntNum::ANT5},
    {"ant6",  sza::util::AntNum::ANT6},
    {"ant7",  sza::util::AntNum::ANT7},
    {   "0",  sza::util::AntNum::ANT0}, 
    {   "1",  sza::util::AntNum::ANT1}, 
    {   "2",  sza::util::AntNum::ANT2}, 
    {   "3",  sza::util::AntNum::ANT3},
    {   "4",  sza::util::AntNum::ANT4},
    {   "5",  sza::util::AntNum::ANT5},
    {   "6",  sza::util::AntNum::ANT6},
    {   "7",  sza::util::AntNum::ANT7},
    {"spare", sza::util::AntNum::SPARE},
    {   "ac", sza::util::AntNum::SPARE},
    {  "all", sza::util::AntNum::ANTALL},
  };
  return add_SetDataType(sc, name, 0, ant, DIMENSION(ant), ant_iterate, 
			 "Antennas");
}

/*-----------------------------------------------------------------------*
 * DDSChannel
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying antenna selections.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DDSChannelDataType(Script *sc, char *name)
{
  // Note if you add antennas to this, the last entry should be the
  // logical OR of dds0 through ddsmax
  
  static Enumerator dds[] = {
    { "none",  sza::util::DDSChannel::DDSNONE}, 
    { "dds0",  sza::util::DDSChannel::DDS0}, 
    { "dds1",  sza::util::DDSChannel::DDS1}, 
    { "dds2",  sza::util::DDSChannel::DDS2}, 
    { "dds3",  sza::util::DDSChannel::DDS3},
    { "dds4",  sza::util::DDSChannel::DDS4},
    { "dds5",  sza::util::DDSChannel::DDS5},
    { "dds6",  sza::util::DDSChannel::DDS6},
    { "dds7",  sza::util::DDSChannel::DDS7},
    { "dds8",  sza::util::DDSChannel::DDS8},
    { "dds9",  sza::util::DDSChannel::DDS9},
    {"dds10",  sza::util::DDSChannel::DDS10},
    {"dds11",  sza::util::DDSChannel::DDS11}, 
    {"dds12",  sza::util::DDSChannel::DDS12}, 
    {"dds13",  sza::util::DDSChannel::DDS13},
    {"dds14",  sza::util::DDSChannel::DDS14},
    {"dds15",  sza::util::DDSChannel::DDS15},
    {"dds16",  sza::util::DDSChannel::DDS16},
    {"dds17",  sza::util::DDSChannel::DDS17},
    {"dds18",  sza::util::DDSChannel::DDS18},
    {"dds19",  sza::util::DDSChannel::DDS19},
    {"dds20",  sza::util::DDSChannel::DDS20},
    {"dds21",  sza::util::DDSChannel::DDS21},
    {"dds22",  sza::util::DDSChannel::DDS22},
    {  "all",  sza::util::DDSChannel::DDSALL}
  };
  
  return add_SetDataType(sc, name, 0, dds, DIMENSION(dds), dds_iterate, 
			 "DDSChannel");
}

/*-----------------------------------------------------------------------*
 * Receivers
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying receiver selections.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ReceiversDataType(Script *sc, char *name)
{
  static Enumerator rx[] = {
    {"none",    0},
    {"rx0",     1}, {"rx1",     2}, {"rx2",     4}, {"rx3",     8},
    {"rx4",    16}, {"rx5",    32}, {"rx6",    64}, {"rx7",   128},
    {"rx8",   256}, {"rx9",   512}, {"rx10", 1024}, {"rx11", 2048},
    {"rx12", 4096}, {"all",  8191},
  };
  return add_SetDataType(sc, name, 0, rx, DIMENSION(rx), rx_iterate, "Receivers");
}

/*-----------------------------------------------------------------------*
 * RxBand
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the receiver to select
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_RxBandDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    
    // A keyword which can be used to select the unused position of
    // the IF switch
    
    {"spare",    sza::util::Rx::RXNONE},
    
    // The 1cm receiver
    
    {"rx1cm",    sza::util::Rx::RX1CM},
    {"rx30ghz",  sza::util::Rx::RX30GHZ},
    
    // The 90 GHz receiver
    
    {"rx3mm",    sza::util::Rx::RX3MM},
    {"rx90ghz",  sza::util::Rx::RX90GHZ},
    
    // The 230 GHz receiver
    
    {"rx1mm",    sza::util::Rx::RX1MM},
    {"rx230ghz", sza::util::Rx::RX230GHZ},
  };
  
  return add_SetDataType(sc, name, 0, type, DIMENSION(type), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * RxStage
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the receiver to select
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_RxStageDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    
    // 30 GHz Amp codes
    
    {"Amp30GHzRFStage1Vg", sza::util::Rx::Amp30GHzRFStage1Vg, "Amp30GHzRFStage1Vg"},
    {"Amp30GHzRFStage2Vg", sza::util::Rx::Amp30GHzRFStage2Vg, "Amp30GHzRFStage2Vg"},
    {"Amp30GHzRFStage3Vg", sza::util::Rx::Amp30GHzRFStage3Vg, "Amp30GHzRFStage3Vg"},
    {"Amp30GHzRFStage4Vg", sza::util::Rx::Amp30GHzRFStage4Vg, "Amp30GHzRFStage4Vg"},
    
    {"Amp30GHzRFStage1Id", sza::util::Rx::Amp30GHzRFStage1Id, "Amp30GHzRFStage1Id"},
    {"Amp30GHzRFStage2Id", sza::util::Rx::Amp30GHzRFStage2Id, "Amp30GHzRFStage2Id"},
    {"Amp30GHzRFStage3Id", sza::util::Rx::Amp30GHzRFStage3Id, "Amp30GHzRFStage3Id"},
    {"Amp30GHzRFStage4Id", sza::util::Rx::Amp30GHzRFStage4Id, "Amp30GHzRFStage4Id"},
    
    {"Amp30GHzIF1",        sza::util::Rx::Amp30GHzIF1, "Amp30GHzIF1"},
    
    // 90 GHz Amp codes
    
    {"Amp90GHzRF1Stage1Vg",sza::util::Rx::Amp90GHzRF1Stage1Vg, "Amp90GHzRF1Stage1Vg"},
    {"Amp90GHzRF1Stage2Vg",sza::util::Rx::Amp90GHzRF1Stage2Vg, "Amp90GHzRF1Stage2Vg"},
    {"Amp90GHzRF2Stage1Vg",sza::util::Rx::Amp90GHzRF2Stage1Vg, "Amp90GHzRF2Stage1Vg"},
    {"Amp90GHzRF2Stage2Vg",sza::util::Rx::Amp90GHzRF2Stage2Vg, "Amp90GHzRF2Stage2Vg"},
    
    {"Amp90GHzRF1Vd",      sza::util::Rx::Amp90GHzRF1Vd, "Amp90GHzRF1Vd"},
    {"Amp90GHzRF2Vd",      sza::util::Rx::Amp90GHzRF2Vd, "Amp90GHzRF2Vd"},
    
    {"Amp90GHzIFVd",       sza::util::Rx::Amp90GHzIFVd, "Amp90GHzIFVd"},
    {"Amp90GHzIFVg",       sza::util::Rx::Amp90GHzIFVg, "Amp90GHzIFVg"},

    // 30 GHz Amp codes
    
    {"0", sza::util::Rx::Amp30GHzRFStage1Vg, "Amp30GHzRFStage1Vg"},
    {"1", sza::util::Rx::Amp30GHzRFStage2Vg, "Amp30GHzRFStage2Vg"},
    {"2", sza::util::Rx::Amp30GHzRFStage3Vg, "Amp30GHzRFStage3Vg"},
    {"3", sza::util::Rx::Amp30GHzRFStage4Vg, "Amp30GHzRFStage4Vg"},
    
    {"4", sza::util::Rx::Amp30GHzRFStage1Id, "Amp30GHzRFStage1Id"},
    {"5", sza::util::Rx::Amp30GHzRFStage2Id, "Amp30GHzRFStage2Id"},
    {"6", sza::util::Rx::Amp30GHzRFStage3Id, "Amp30GHzRFStage3Id"},
    {"7", sza::util::Rx::Amp30GHzRFStage4Id, "Amp30GHzRFStage4Id"},
	  
    {"8", sza::util::Rx::Amp30GHzIF1,        "Amp30GHzIF1"},
  
    // 90 GHz Amp "Amp codes

    {"9", sza::util::Rx::Amp90GHzRF1Stage1Vg, "Amp90GHzRF1Stage1Vg"},
    {"10",sza::util::Rx::Amp90GHzRF1Stage2Vg, "Amp90GHzRF1Stage2Vg"},
    {"11",sza::util::Rx::Amp90GHzRF2Stage1Vg, "Amp90GHzRF2Stage1Vg"},
    {"12",sza::util::Rx::Amp90GHzRF2Stage2Vg, "Amp90GHzRF2Stage2Vg"},
    
    {"13",sza::util::Rx::Amp90GHzRF1Vd,       "Amp90GHzRF1Vd"},
    {"14",sza::util::Rx::Amp90GHzRF2Vd,       "Amp90GHzRF2Vd"},
    
    {"15",sza::util::Rx::Amp90GHzIFVd,        "Amp90GHzIFVd"},
    {"16",sza::util::Rx::Amp90GHzIFVg,        "Amp90GHzIFVg"},
  };
  
  return add_ChoiceDataType(sc, name, en, DIMENSION(en), true);
}


/*-----------------------------------------------------------------------*
 * WalshStages
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the module(s) that a walsh function
 * is to be used by.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_WalshStagesDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    {"mod",    WALSH_MOD},
    {"demod",  WALSH_DEMOD},
    {"both", WALSH_MOD | WALSH_DEMOD}
  };
  return add_SetDataType(sc, name, 0, type, DIMENSION(type), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * WalshFunction
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for selecting a walsh function.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_WalshFunctionDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_walsh_fn, sc_iterate_uint, "Integer");
}

/*.......................................................................
 * Check the validity of a walsh-function variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_walsh_fn)
{
  unsigned u = UINT_VARIABLE(var)->uint;
  if(u > 31) {
    input_error(stream, 1, "Invalid walsh-function. Should be 0 <= f <= 31.\n");
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * WalshStep
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for selecting one step in a Walsh cycle
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_WalshStepDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_walshstep_fn, sc_iterate_uint, "Integer");
}

/*.......................................................................
 * Check the validity of a WalshStep variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_walshstep_fn)
{
  unsigned u = UINT_VARIABLE(var)->uint;
  if(u > 15) {
    input_error(stream, 1, "Invalid walsh-function. Should be 0 <= i <= 15.\n");
    return 1;
  };
  return 0;
}
/*-----------------------------------------------------------------------*
 * SysType
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the target systems of reboots
 * and shutdowns.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_SysTypeDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"cpu",        SYS_CPU},
    {"rtc",        SYS_RTC},
    {"szacontrol", SYS_SZACONTROL},
    {"pmac",       SYS_PMAC},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * TimeScale
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying time scales.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TimeScaleDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    {"utc", TIME_UTC},
    {"UTC", TIME_UTC},
    {"lst", TIME_LST},
    {"LST", TIME_LST},
  };
  return add_ChoiceDataType(sc, name, type, DIMENSION(type));
}

/*-----------------------------------------------------------------------*
 * SwitchState                                                           *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying hardware switch states.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_SwitchStateDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"on",  SWITCH_ON},
    {"off", SWITCH_OFF},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * Heaters
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of receiver heaters.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_HeatersDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"cryo", 1}, {"ambient0", 2}, {"ambient1", 4},
    {"all", 7}
  };
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * HeaterVoltage
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying heater voltages. Heater voltages
 * are stored as double precision numbers between 0 and 10v. In addition,
 * heater voltage variables will contain -1.0 when the user specifies
 * the voltage as "off".
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_HeaterVoltageDataType(Script *sc, char *name)
{
  DataType *dt;    /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_HeaterVoltageDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(DoubleVariable),
		    0, 0, const_htr_v, print_htr_v, sc_equal_double,
		    sc_gt_double, 0, sc_iterate_double, "Double");
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a heater-voltage constant.
 */
static DT_CONST(const_htr_v)
{
  char *usage = "Expected a heater voltage or the word 'off'.\n";
  Variable *var;        /* The variable that will contain the number */
  double d;             /* The floating point read from the input stream */
  /*
   * Read either a heater voltage, or the "off" keyword, from the input
   * stream.
   */
  if(isalpha(stream->nextc)) {
    if(input_keyword(stream, 0, 1))
      return input_error(stream, 1, usage);
    if(strcmp(stream->work, "off")==0) {
      d = -1.0;
    } else {
      return input_error(stream, 1, usage);
    };
  } else {
    if(input_double(stream, 0, &d))
      return input_error(stream, 1, usage);
    /*
     * Check the domain of the variable.
     */
    if(d < 0.0 || d > 10.0) {
      return input_error(stream, 1,
			 "Invalid heater voltage. Should be 0-10v.\n");
    };
  };
  /*
   * Record the number in a new variable and push that variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  DOUBLE_VARIABLE(var)->d = d;
  return 0;
}

/*.......................................................................
 * Print a heater-voltage variable.
 */
static DT_PRINT(print_htr_v)
{
  double d = DOUBLE_VARIABLE(var)->d;
  return d<0.0 ? write_OutputStream(output, "off") :
    output_double(sc->output, "", 0, DBL_DIG, 'g', d);
}

/*-----------------------------------------------------------------------*
 * LoStages
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of local oscillator
 * stages.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_LoStagesDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"if",        sza::util::LoStage::LO_IF}, 
    {"sweep",     sza::util::LoStage::LO_SWEEP}, 
    {"gunn",      sza::util::LoStage::LO_GUNN},
    {"rfmonitor", sza::util::LoStage::LO_RFMONITOR},
    {"ifmonitor", sza::util::LoStage::LO_IFMONITOR},
    {"all",       sza::util::LoStage ::LO_ALL}
  };
  
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), NULL, NULL);
}


/*-----------------------------------------------------------------------*
 * ThermalTarget
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of local oscillator
 * stages.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ThermalTargetDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"rbox",        sza::util::Thermal::RBOX}, 
    {"ebox",        sza::util::Thermal::EBOX}, 
    {"circ",        sza::util::Thermal::CIRC}, 
    {"all",         sza::util::Thermal::ALL}, 
  };
  
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * ThermalMode
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of local oscillator
 * stages.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ThermalModeDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"on",         sza::util::Thermal::ON},
    {"off",        sza::util::Thermal::OFF}, 
    {"manual",     sza::util::Thermal::MANUAL}, 
  };
  
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * AcquireTargets
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the targets of the acquired()
 * function.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DelayTargetDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"corr",     DELAY_CORR},
    {"lr",       DELAY_LR},
    {"both",     DELAY_ALL},
    {"all",      DELAY_ALL}
  };
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), 0x0, "DelayTarget");
}

/*-----------------------------------------------------------------------*
 * AcquireTargets
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the targets of the acquired()
 * function.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_AcquireTargetsDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"caltert",     ACQ_CALTERT},
    {"phase",       ACQ_PHASE},
    {"source",      ACQ_SOURCE},
    {"channelizer", ACQ_CHANNELIZER},
    {"mark",        ACQ_MARK},
    {"grab",        ACQ_GRAB},
    {"setreg",      ACQ_SETREG},
    {"can",         ACQ_CAN},
    {"tv_offset",   ACQ_TV_OFFSET},
    {"frame",       ACQ_FRAME},
    {"noise",       ACQ_NOISE},
    {"all",         ACQ_CALTERT | ACQ_PHASE | ACQ_SOURCE | ACQ_CHANNELIZER | ACQ_MARK | ACQ_GRAB | ACQ_SETREG | ACQ_TV_OFFSET | ACQ_CAN | ACQ_FRAME | ACQ_NOISE}
  };
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * PhaseStep
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the phase-shifter stepper-motor
 * increment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PhaseStepDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"half",  STEP_HALF},
    {"full",  STEP_FULL},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}
/*-----------------------------------------------------------------------*
 * PhaseState
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the desired polarization-state
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PolarStateDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"left",  LEFT},  /* These are defined in rtcnetcoms.h */
    {"right", RIGHT},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * PhaseShift
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying phase shifts. Phase shifts
 * are stored as unsigned integer encoder values between 0 and 1023.
 * In addition, the value of 1024 is used to denote a switched-off motor.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PhaseShiftDataType(Script *sc, char *name)
{
  DataType *dt;    /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_PhaseShiftDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(UintVariable),
		    0, 0, const_pshift, print_pshift, sc_equal_uint,
		    gt_pshift, 0, sc_iterate_uint, "Integer");
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a PhaseShift constant.
 */
static DT_CONST(const_pshift)
{
  char *usage = "Expected a phase shift encoder value or the word 'off'.\n";
  Variable *var;        /* The variable that will contain the number */
  unsigned long ul;     /* The encoder value read from the input stream */
  
  // Read either an encoder value the "off" keyword, or the keywords
  // "left" or "right", from the input stream.
  
  if(isalpha(stream->nextc)) {
    if(input_keyword(stream, 0, 1))
      return input_error(stream, 1, usage);
    if(strcmp(stream->work, "off")==0) {
      ul = 1024;
    } else if(strcmp(stream->work, "right")==0) {
      ul = ENC_RIGHT_REQ;
    } else if(strcmp(stream->work, "left")==0) {
      ul = ENC_LEFT_REQ;
    } else {
      return input_error(stream, 1, usage);
    };
  } else {
    if(input_ulong(stream, 0, 0, &ul))
      return input_error(stream, 1, usage);
    
    // Check the domain of the variable.
    
    if(ul < 49 || ul > 975) {
      return input_error(stream, 1,
			 "Invalid phase shift encoder value. Should be 49-975.\n");
    };
  };
  
  // Record the number in a new variable and push that variable onto
  // the expression stack.
  
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Initialize the variable.
  
  UINT_VARIABLE(var)->uint = ul;
  return 0;
}

/*.......................................................................
 * Print a PhaseShift variable.
 */
static DT_PRINT(print_pshift)
{
  unsigned long ul = UINT_VARIABLE(var)->uint;
  switch (ul) {
  case 1024:
    return write_OutputStream(output, "off");
    break;
  case ENC_RIGHT_REQ:
    return write_OutputStream(output, "right");
    break;
  case ENC_LEFT_REQ:
    return write_OutputStream(output, "left");
    break;
  default:
    return output_ulong(output, OUT_DECIMAL, "", 0, 0, ul);
    break;
  }
  return 0;
}

/**.......................................................................
 * Test whether the value of phase-shift variable is greater than a
 * second. Note that phase-shifter-off is reported as less than any
 * other encoder value.
 */
static DT_RELFN(gt_pshift)
{
  unsigned a = UINT_VARIABLE(va)->uint;
  unsigned b = UINT_VARIABLE(vb)->uint;
  if(a == 1024)      /* If va is off, va can never be > vb */
    return 0;
  else if(b == 1024) /* If vb is off, va is > vb unless vb is also off */
    return b != 1024;
  else
    return UINT_VARIABLE(va)->uint > UINT_VARIABLE(vb)->uint;
}

/*-----------------------------------------------------------------------*
 * QuadPhase
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for selecting between the 4 possible states of
 * a quadrature phase-shift network. The 4 allowed phases are
 * 0,90,180,270 degrees. Internally these are stored in an unsigned
 * integer.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_QuadPhaseDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_quad_phase, 0, NULL);
}

/*.......................................................................
 * Check the validity of a QuadPhase variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_quad_phase)
{
  unsigned u = UINT_VARIABLE(var)->uint;
  /*
   * The phase must be a multiple of 90.
   */
  if(u % 90 != 0 || u > 270) {
    input_error(stream, 1,
		"Invalid quadrature phase - must be 0,90,180 or 270.\n");
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Source
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new source-specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_SourceDataType(Script *sc, char *name)
{
  DataType *dt;                   /* The object to be returned */
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(SourceVariable),
		    0, 0, const_source, print_source, equal_source, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a source specification constant.
 */
static DT_CONST(const_source)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   resource object */
  Variable *var;   /* The new variable */
  SourceId id;     /* The identification header of the source */
  int length;      /* The length of the source name */
  /*
   * Get the resource object of the navigator thread.
   */
  Navigator *nav = (Navigator* )cp_ThreadData(cp, CP_NAVIGATOR);
  int i;
  /*
   * Read the source name from the input stream.
   */
  if(input_literal(stream, 0, "",  "",  valid_source_char, ""))
    return input_error(stream, 1, "Missing source name.\n");
  /*
   * Check that the length of the name doesn't exceed the legal limit for
   * source names.
   */
  length = strlen(stream->work);
  if(length >= SRC_NAME_MAX) {
    return input_error(stream, 1,
		       "Source names can't be longer than %d characters.\n",
		       SRC_NAME_MAX - 1);
  };
  /*
   * Convert the source name to lower case. The use of sc_equal_string()
   * as the equality method relies on this.
   */
  for(i=0; i<length; i++) {
    char *cptr = stream->work + i;
    if(isupper((int) *cptr))
      *cptr = tolower(*cptr);
  };
  
  // Look up the source to see whether it exists, but only if this is
  // not the current source.
  
  if(!navIsCurrent(stream->work))
    if(nav_lookup_source(nav, stream->work, 0, &id)) {
      return input_error(stream, 1,
			 "The source catalog does not contain an entry for"
			 " \"%s\".\n", stream->work);
    };
  
  // Create a new source variable and push it onto the expression
  // stack.
  
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Initialize the variable with the name of the source.
  
  strcpy(SOURCE_VARIABLE(var)->name, stream->work);
  return 0;
}

/*.......................................................................
 * Print a source specification variable.
 */
static DT_PRINT(print_source)
{
  return write_OutputStream(output, SOURCE_VARIABLE(var)->name);
}

/*.......................................................................
 * Test equality of source variables
 */
static DT_RELFN(equal_source)
{
  return strcmp(SOURCE_VARIABLE(va)->name, SOURCE_VARIABLE(vb)->name)==0;
}

/*-----------------------------------------------------------------------*
 * Scan
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new scan-specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ScanDataType(Script *sc, char *name)
{
  DataType *dt;                   /* The object to be returned */
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(ScanVariable),
		    0, 0, const_scan, print_scan, equal_scan, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a scan specification constant.
 */
static DT_CONST(const_scan)
{
  ControlProg *cp = (ControlProg* )sc->project; // The control-program
						// resource object
  Variable *var;   /* The new variable */
  ScanId id;     /* The identification header of the scan */
  int length;      /* The length of the scan name */
  /*
   * Get the resource object of the navigator thread.
   */
  Navigator *nav = (Navigator* )cp_ThreadData(cp, CP_NAVIGATOR);
  int i;
  /*
   * Read the scan name from the input stream.
   */
  if(input_literal(stream, 0, "",  "",  valid_scan_char, ""))
    return input_error(stream, 1, "Missing scan name.\n");
  /*
   * Check that the length of the name doesn't exceed the legal limit for
   * scan names.
   */
  length = strlen(stream->work);
  if(length >= SCAN_NAME_MAX) {
    return input_error(stream, 1,
		       "Scan names can't be longer than %d characters.\n",
		       SCAN_NAME_MAX - 1);
  };
  /*
   * Convert the scan name to lower case. The use of sc_equal_string()
   * as the equality method relies on this.
   */
  for(i=0; i<length; i++) {
    char *cptr = stream->work + i;
    if(isupper((int) *cptr))
      *cptr = tolower(*cptr);
  };
  /*
   * Look up the scan to see wether it exists.
   */
  if(nav_lookup_scan(nav, stream->work, 0, &id)) {
    return input_error(stream, 1,
		       "The scan catalog does not contain an entry for \"%s\".\n",
		       stream->work);
  };
  /*
   * Create a new scan variable and push it onto the expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable with the name of the scan.
   */
  strcpy(SCAN_VARIABLE(var)->name, stream->work);
  return 0;
}

/*.......................................................................
 * Print a scan specification variable.
 */
static DT_PRINT(print_scan)
{
  return write_OutputStream(output, SCAN_VARIABLE(var)->name);
}

/*.......................................................................
 * Test equality of scan variables
 */
static DT_RELFN(equal_scan)
{
  return strcmp(SCAN_VARIABLE(va)->name, SCAN_VARIABLE(vb)->name)==0;
}

/*-----------------------------------------------------------------------*
 * Model
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for selecting between optical and radio
 * pointing models.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ModelDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"radio",     sza::util::PointingMode::RADIO},
    {"optical",   sza::util::PointingMode::OPTICAL},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * Latitude
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a latitude on the Earth.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_LatitudeDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_latitude, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a latitude variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_latitude)
{
  double latitude = DOUBLE_VARIABLE(var)->d;
  if(latitude < -90.0 || latitude > 90.0) {
    return input_error(stream, 1,
		       "Invalid latitude. Should be -90 <= latitude <= 90.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Longitude
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a longitude on the Earth.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_LongitudeDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_longitude, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a longitude variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_longitude)
{
  double longitude = DOUBLE_VARIABLE(var)->d;
  if(longitude < -180.0 || longitude > 180.0) {
    return input_error(stream, 1,
		       "Invalid longitude. Should be -180 <= longitude <= 180.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Azimuth
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the target azimuth of the
 * telescope. The azimuths of the compass points are N=0, E=90, S=180
 * and W=270 degrees.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_AzimuthDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_azimuth, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of an azimuth variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_azimuth)
{
  double azimuth = DOUBLE_VARIABLE(var)->d;
  if(azimuth < -360.0 || azimuth >= 360.0) {
    return input_error(stream, 1,
		       "Invalid azimuth. Should be -360 <= azimuth < 360.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * DeckAngle
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the position of the rotating
 * platform on which the dishes are mounted.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DeckAngleDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_deckangle, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a deck-angle variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_deckangle)
{
  double deck = DOUBLE_VARIABLE(var)->d;
  if(deck < -360.0 || deck > 360.0) {
    return input_error(stream, 1,
		       "Invalid DeckAngle. Should be -360 <= value <= 360.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Elevation
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the target elevation of the
 * telescope.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ElevationDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_elevation, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of an elevation variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_elevation)
{
  double elevation = DOUBLE_VARIABLE(var)->d;
  if(elevation < -90.0 || elevation > 90.0) {
    return input_error(stream, 1,
		       "Invalid elevation. Should be -90 <= el <= 90.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Pointingoffset
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying temporary pointing offsets as
 * angles.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PointingOffsetDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_pointingoffset,
				 sc_iterate_double, "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a pointing-offset variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_pointingoffset)
{
  double offset = DOUBLE_VARIABLE(var)->d;
  if(offset < -360.0 || offset > 360.0) {
    return input_error(stream, 1,
		       "Invalid pointing offset. Should be -360 <= offset <= 360.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * Flexure
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the degree of gravitational
 * drooping of the telescope as a function of elevation.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_FlexureDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_flexure, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a flexure variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_flexure)
{
  double flexure = DOUBLE_VARIABLE(var)->d;
  if(flexure <= -90.0 || flexure >= 90.0) {
    return input_error(stream, 1,
		       "Invalid flexure. Should be -90 < flexure < 90.\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * LoFrequency
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the frequency of a tunable
 * oscillator.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return DataType * The newly added datatype, or NULL on error.  */
DataType *add_LoFrequencyDataType(Script *sc, char *name)
{
  return add_IntDataType(sc, name, check_LoFrequency, sc_iterate_int,
			 "Double", 1);
}

/*.......................................................................
 * Check the validity of a Frequency variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_LoFrequency)
{
  unsigned short frequency = INT_VARIABLE(var)->i;
  
  // Do nothing for now.
  
  if(frequency < 8000 || frequency > 13000)
    return input_error(stream, 1, "Invalid frequency. "
		       "Should be 8000 <= frequency (in MHz) <= 13000.\n");
  
  return 0;
}

/*-----------------------------------------------------------------------*
 * Tilt
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the tilts of the telescope axes.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TiltDataType(Script *sc, char *name)
{
  return add_SexagesimalDataType(sc, name, check_tilt, sc_iterate_double,
				 "Sexagesimal", 1);
}

/*.......................................................................
 * Check the validity of a tilt variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_tilt)
{
  double tilt = DOUBLE_VARIABLE(var)->d;
  if(tilt <= -90.0 || tilt >= 90.0)
    return input_error(stream, 1, "Invalid tilt. Should be -90 < tilt < 90.\n");
  return 0;
}

/*-----------------------------------------------------------------------*
 * Tracking
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the type of tracking
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TrackingDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"both",  sza::util::Tracking::TRACK_BOTH},
    {"point", sza::util::Tracking::TRACK_POINT},
    {"phase", sza::util::Tracking::TRACK_PHASE},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * Altitude
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the height of the telescope above
 * the standard geodetic spheroid.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_AltitudeDataType(Script *sc, char *name)
{
  return add_DoubleDataType(sc, name, check_altitude, sc_iterate_double,
			    "Double", 1);
}

/*.......................................................................
 * Check the validity of an altitude variable. An altitude range is
 * needed so that the altitude can be encoded in a network long for
 * transmission to the real-time control system.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_altitude)
{
  double altitude = DOUBLE_VARIABLE(var)->d;
  if(altitude < -SZA_MAX_ALT || altitude > SZA_MAX_ALT) {
    return input_error(stream, 1,
		       "Invalid altitude. Should be %g <= altitude <= %g.\n",
		       -SZA_MAX_ALT, SZA_MAX_ALT);
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * GpibDev
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for selecting a device on the GPIB bus.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_GpibDevDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_gpib_dev, 0, NULL);
}

/*.......................................................................
 * Check the validity of a gpib-device variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_gpib_dev)
{
  unsigned u = UINT_VARIABLE(var)->uint;
  if(u > 30)
    return input_error(stream, 1, "Invalid gpib device. Should be 0..30.\n");
  return 0;
}

/*-----------------------------------------------------------------------*
 * GpibCmd
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype to specify a command string to be sent to a
 * device on the GPIB bus.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_GpibCmdDataType(Script *sc, char *name)
{
  return add_StringDataType(sc, name, 1, check_gpib_cmd);
}

/*.......................................................................
 * Check the validity of a gpib command string.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_gpib_cmd)
{
  char *s = STRING_VARIABLE(var)->string;
  if(strlen(s) > GPIB_MAX_DATA)
    return input_error(stream, 1, "GPIB command string too long.\n");
  return 0;
}

/*-----------------------------------------------------------------------*
 * Features
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for enumerating frame features.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_FeaturesDataType(Script *sc, char *name)
{
  static Enumerator features[] = {
    {"f0",   1U<<0}, {"f1",   1U<<1}, {"f2",   1U<<2}, {"f3",   1U<<3},
    {"f4",   1U<<4}, {"f5",   1U<<5}, {"f6",   1U<<6}, {"f7",   1U<<7},
    {"f8",   1U<<8}, {"f9",   1U<<9}, {"f10", 1U<<10}, {"f11", 1U<<11},
    {"f12", 1U<<12}, {"f13", 1U<<13}, {"f14", 1U<<14}, {"f15", 1U<<15},
    {"f16", 1U<<16}, {"f17", 1U<<17}, {"f18", 1U<<18}, {"f19", 1U<<19},
    {"f20", 1U<<20}, {"f21", 1U<<21}, {"f22", 1U<<22}, {"f23", 1U<<23},
    {"f24", 1U<<24}, {"f25", 1U<<25}, {"f26", 1U<<26}, {"f27", 1U<<27},
    {"f28", 1U<<28}, {"f29", 1U<<29}, {"f30", 1U<<30}, {"f31", 1U<<31},
    {"all",    ~0U},
  };
  return add_SetDataType(sc, name, 1, features, DIMENSION(features), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * DeckMode
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying how to position the deck axis
 * while tracking a source.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DeckModeDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"track",   DECK_TRACK},
    {"zero",    DECK_ZERO},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * Attenuation
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying attenuations over the range
 * 0..31db, in 1db steps.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_AttenuationDataType(Script *sc, char *name)
{
  return add_IntDataType(sc, name, check_attn_fn, sc_iterate_int, "Integer",1);
}

/*.......................................................................
 * Check the validity of an Attenuation variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_attn_fn)
{
  int i = INT_VARIABLE(var)->i;
  if(abs(i) > 31) {
    input_error(stream, 1, "Invalid attenuation. Should be -31 <= v <= 31.\n");
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * LO Oscillators
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying LO frequency-band selections.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_LoOscDataType(Script *sc, char *name)
{
  static Enumerator bands[] = {
    // Equivalent designations for X-band
    
    {"yig",      sza::util::LoOsc::YIG}, 
    {"varactor", sza::util::LoOsc::VARACTOR}, 
    {"gunn",     sza::util::LoOsc::GUNN}, 
    
    // All -- this should obviously always be updated if new entries
    // are added
    
    {"all",       sza::util::LoOsc::ALL},
  };
  
  return add_SetDataType(sc, name, 0, bands, DIMENSION(bands), NULL, NULL);
}

//-----------------------------------------------------------------------
// CAN Modules
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying CAN modules
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_CanModulesDataType(Script *sc, char *name)
{
  static Enumerator modules[] = {
    {"all",          sza::util::CanModule::ALL},
    
    {"bias",         sza::util::CanModule::BIAS},
    {(char*)sza::util::CanModule::biasApiStr_.str().c_str(),      sza::util::CanModule::BIAS},
    
    {"caltert",      sza::util::CanModule::CALTERT},
    {(char*)sza::util::CanModule::caltertApiStr_.str().c_str(),   sza::util::CanModule::CALTERT},
    
    {"dcon",         sza::util::CanModule::DCON}, 
    
    {"ifmod",        sza::util::CanModule::IFMOD},
    {(char*)sza::util::CanModule::ifmodApiStr_.str().c_str(),     sza::util::CanModule::IFMOD},
    
    {"intmod",       sza::util::CanModule::INTMOD},
    {(char*)sza::util::CanModule::intmodApiStr_.str().c_str(),    sza::util::CanModule::INTMOD},
    
    {"noiseSource",  sza::util::CanModule::NOISE_SOURCE}, 
    {"noise",        sza::util::CanModule::NOISE_SOURCE}, 
    
    {"quadMod",      sza::util::CanModule::QUAD_MOD}, 
    {"quad",         sza::util::CanModule::QUAD_MOD}, 
    
    {"receiver",     sza::util::CanModule::RECEIVER}, 
    {(char*)sza::util::CanModule::receiverApiStr_.str().c_str(),  sza::util::CanModule::RECEIVER}, 
    
    {"thermal",      sza::util::CanModule::THERMAL}, 
    {(char*)sza::util::CanModule::thermalApiStr_.str().c_str(),   sza::util::CanModule::THERMAL}, 
    
    {"varactor",     sza::util::CanModule::VARACTOR},
    {(char*)sza::util::CanModule::varactorApiStr_.str().c_str(),  sza::util::CanModule::VARACTOR},
    
    {"yig",          sza::util::CanModule::YIG},
    {(char*)sza::util::CanModule::yigApiStr_.str().c_str(),       sza::util::CanModule::YIG},
  };
  
  return add_SetDataType(sc, name, 0, modules, DIMENSION(modules), NULL, NULL);
}

//-----------------------------------------------------------------------
// Bands
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying frequency-band selections.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_BandsDataType(Script *sc, char *name)
{
  static Enumerator bands[] = {
    {"none",   sza::util::CorrelatorBand::BANDNONE}, 
    {"band0",  sza::util::CorrelatorBand::BAND0}, 
    {"band1",  sza::util::CorrelatorBand::BAND1},
    {"band2",  sza::util::CorrelatorBand::BAND2},
    {"band3",  sza::util::CorrelatorBand::BAND3},
    {"band4",  sza::util::CorrelatorBand::BAND4},
    {"band5",  sza::util::CorrelatorBand::BAND5},
    {"band6",  sza::util::CorrelatorBand::BAND6},
    {"band7",  sza::util::CorrelatorBand::BAND7},
    {"band8",  sza::util::CorrelatorBand::BAND8},
    {"band9",  sza::util::CorrelatorBand::BAND9},
    {"band10", sza::util::CorrelatorBand::BAND10}, 
    {"band11", sza::util::CorrelatorBand::BAND11},
    {"band12", sza::util::CorrelatorBand::BAND12},
    {"band13", sza::util::CorrelatorBand::BAND13},
    {"band14", sza::util::CorrelatorBand::BAND14},
    {"band15", sza::util::CorrelatorBand::BAND15},
    {"all",    sza::util::CorrelatorBand::BANDALL}
  };
  return add_SetDataType(sc, name, 0, bands, DIMENSION(bands), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * TotalPower
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the target sensor voltage of a
 * set of total power detectors.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TotalPowerDataType(Script *sc, char *name)
{
  return add_DoubleDataType(sc, name, check_total_power, sc_iterate_double,
			    "Double", 0);
}

/*.......................................................................
 * Check the validity of an TotalPower variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_total_power)
{
  double pwr = DOUBLE_VARIABLE(var)->d;
  if(pwr < 0.0) {
    return input_error(stream, 1,
		       "Invalid TotalPower. Should be >= 0 .\n");
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * ArcFileType
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying an archive file type.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ArcFileDataType(Script *sc, char *name)
{
  static Enumerator en[] = {  /* Note that ARC_***_FILE come from arcfile.h */
    {"log",     ARC_LOG_FILE},
    {"archive", ARC_DAT_FILE},
    {"grabber", ARC_GRAB_FILE},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * FeatureChangeType
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying what to do with a set of archive
 * feature markers.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_FeatureChangeDataType(Script *sc, char *name)
{
  static Enumerator en[] = {  /* Note that FEATURE_*** come from rtcnetcoms.h */
    {"add",     FEATURE_ADD},
    {"remove",  FEATURE_REMOVE},
    {"one",     FEATURE_ONE}
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * PowerMeterCommand
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for enumerating power-meter commands.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PowerMeterCommandDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"reset", NCAL_POWER_RESET},
    {"zero",  NCAL_POWER_ZERO},
    {"read",  NCAL_POWER_READ}
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * BitMask
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a bit-mask as a set of 32 bits.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_BitMaskDataType(Script *sc, char *name)
{
  static Enumerator bits[] = {
    {"none",    0},
    {"b0",   1U<<0}, {"b1",   1U<<1}, {"b2",   1U<<2}, {"b3",   1U<<3},
    {"b4",   1U<<4}, {"b5",   1U<<5}, {"b6",   1U<<6}, {"b7",   1U<<7},
    {"b8",   1U<<8}, {"b9",   1U<<9}, {"b10", 1U<<10}, {"b11", 1U<<11},
    {"b12", 1U<<12}, {"b13", 1U<<13}, {"b14", 1U<<14}, {"b15", 1U<<15},
    {"b16", 1U<<16}, {"b17", 1U<<17}, {"b18", 1U<<18}, {"b19", 1U<<19},
    {"b20", 1U<<20}, {"b21", 1U<<21}, {"b22", 1U<<22}, {"b23", 1U<<23},
    {"b24", 1U<<24}, {"b25", 1U<<25}, {"b26", 1U<<26}, {"b27", 1U<<27},
    {"b28", 1U<<28}, {"b29", 1U<<29}, {"b30", 1U<<30}, {"b31", 1U<<31},
    {"all",    ~0U},
  };
  return add_SetDataType(sc, name, 1, bits, DIMENSION(bits), NULL, NULL);
}

/*.......................................................................
 * Create a new datatype for enumerating bit-mask manipulation operators.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_BitMaskOperDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"set",     BIT_MASK_SET},
    {"clear",   BIT_MASK_CLEAR},
    {"assign",  BIT_MASK_ASSIGN}
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * DioBoard
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new digital I/O doard specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 *  regmap      RegMap *  The register map to use to lookup board
 *                        names.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DioBoardDataType(Script *sc, char *name, ArrayMap *arraymap)
{
  DataType *dt;                   /* The object to be returned */
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, arraymap, sizeof(UintVariable),
		    0, 0, const_dio_board, print_board, sc_equal_uint,
		    0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse the name of a digital I/O board.
 */
static DT_CONST(const_dio_board)
{
  Variable *var;        /* The variable that will contain the specification */
  RegMapBoard *brd;     /* The board description container */
  RegMapBlock *blk;     /* The output register of the board */
  
  // The register map was passed to new_DataType() to be recorded in
  // the context member of the Board datatype. Retrieve it.
  
  RegMap *regmap = (RegMap* )dt->context;
  
  // Read the board name from the input stream.
  
  if(input_keyword(stream, 0, 1))
    return input_error(stream, 1, "Missing digital I/O board name.\n");
  
  // Look up the named board.
  
  brd = find_RegMapBoard(regmap, stream->work);
  if(!brd) {
    return input_error(stream, 1,
		       "Error: '%s' is not the name of a known board.\n", stream->work);
  };
  
  // All the target digital I/O boards have an output register with
  // four elements.
  
  blk = find_RegMapBoard_Block(brd, "output");
  if(!blk || blk->nreg_ != 4) {
    return input_error(stream, 1, "Board %s is not a digital I/O board.\n",
		       brd->name);
  };
  
  // Record the specification in a new variable and push it onto the
  // expression stack.
  
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Inititialize the variable.
  
  UINT_VARIABLE(var)->uint = brd->number;
  return 0;
}

/*-----------------------------------------------------------------------*
 * Script
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * The Script datatype is used for entering the name and arguments
 * of a script.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ScriptDataType(Script *sc, char *name)
{
  DataType *dt;    /* The new datatype to be returned */
  /*
   * Check arguments.
   */
  if(!sc || !name) {
    lprintf(stderr, "add_ScriptDataType: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(ScriptVariable),
		    0, 0, const_script, print_script, equal_script,
		    0, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a Script constant.
 */
static DT_CONST(const_script)
{
  Variable *var;   /* The new variable */
  /*
   * Get the resource object of the parent thread.
   */
  Scheduler *sch = (Scheduler* )cp_ThreadData((ControlProg* )sc->project, 
					      CP_SCHEDULER);
  ScheduleData *sd = (ScheduleData* )sc->data;
  /*
   * Create a new variable and push it onto the expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Read the script path name as a literal string.
   */
  if(stream->nextc=='"' ?
     input_quoted_string(stream, 0) : 
     input_literal(stream,  1, "([{",  ")]}",  is_script_file, ""))
    return input_error(stream, 1, "Bad script file name.\n");
  /*
   * Initialize the variable.
   */
  SCRIPT_VARIABLE(var)->sc = sch_compile_schedule(sch, "",stream->work, stream);
  if(!SCRIPT_VARIABLE(var)->sc)
    return 1;
  /*
   * Keep a record of the compiled schedule so that we can delete it when
   * our parent script is discarded.
   */
  if(!append_ListNode(sd->schedules, SCRIPT_VARIABLE(var)->sc)) {
    del_SzaScript(SCRIPT_VARIABLE(var)->sc);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Print the contents of a Script variable.
 */
static DT_PRINT(print_script)
{
  Script *script = SCRIPT_VARIABLE(var)->sc;
  /*
   * Output the script name.
   */
  if(write_OutputStream(output, script->script.name))
    return 1;
  /*
   * Are there any script arguments to display?
   */
  if(script->script.args->head) {
    if(write_OutputStream(output, "(") ||
       print_ArgumentList(script, output, 0, script->script.args) ||
       write_OutputStream(output, ")"))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Return non-zero if a character shouldn't be considered a part of a
 * script file name.
 */
static int is_script_file(int c)
{
  return c!=',' && c!='(' && c!=')' && c!='}' && c!= '\n';
}

/*.......................................................................
 * Test two Script variables for equality.
 */
static DT_RELFN(equal_script)
{
  return SCRIPT_VARIABLE(va)->sc == SCRIPT_VARIABLE(vb)->sc;
}

/*-----------------------------------------------------------------------*
 * DsCommand
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for enumerating thermometer commands.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DsCommandDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"read",      DS_READ_CMD},
    {"readall",   DS_READALL_CMD},
    {"address",   DS_ADDRESS_CMD},
    {"reset",     DS_RESET_CMD},
    {"init",      DS_INIT_CMD},
    {"search",    DS_SEARCH_CMD},
    {"romid",     DS_ROMID_CMD},
    {"flag",      DS_FLAG_CMD},
    {"unflag",    DS_UNFLAG_CMD},
    {"display",   DS_DISPLAY_CMD},
    {"checksum",  DS_CHECK_CMD},
    {"nochecksum",DS_NOCHECK_CMD},
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * OpticalCamera
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the target of an optical camera 
 * command.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_OptCamTargetDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"frame",     OPTCAM_FRAME},
    {"controller",OPTCAM_CONTROLLER},
    {"camera",    OPTCAM_CAMERA},
    {"stepper",   OPTCAM_STEPPER},
    {"focus",     OPTCAM_FOCUS},
    {"iris",      OPTCAM_IRIS},
    {"shutter",   OPTCAM_SHUT},
    {"sens_auto", OPTCAM_SENS_AUTO},
    {"sens_manu", OPTCAM_SENS_MANU},
    {"agc",       OPTCAM_AGC},
    {"alc",       OPTCAM_ALC},
    {"manu_iris", OPTCAM_MANU_IRIS},
    {"superd",    OPTCAM_SUPERD},
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}

/*.......................................................................
 * Create a new datatype for specifying the action of an optical camera 
 * command.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_OptCamActionDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"id",      OPTCAM_ID},
    {"rbc",     OPTCAM_RBC},
    {"open",    OPTCAM_OPEN},
    {"close",   OPTCAM_CLOSE},
    {"preset",  OPTCAM_PRESET},
    {"on",      OPTCAM_ON},
    {"off",     OPTCAM_OFF},
    {"inc",     OPTCAM_INC},
    {"dec",     OPTCAM_DEC},
    {"low",     OPTCAM_LOW},
    {"mid",     OPTCAM_MID},
    {"high",    OPTCAM_HIGH},
    {"s100",    OPTCAM_100},
    {"s250",    OPTCAM_250},
    {"s500",    OPTCAM_500},
    {"s1000",   OPTCAM_1000},
    {"s2000",   OPTCAM_2000},
    {"s4000",   OPTCAM_4000},
    {"s10000",  OPTCAM_10000},
    {"center",  OPTCAM_CENTER}
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}
/*-----------------------------------------------------------------------*
 * FgReg datatype
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for enumerating frame grabber registers
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_FgRegDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"incsr1", FG_INCSR1_REG},
    {"incsr2", FG_INCSR2_REG},
    {"outcsr", FG_OUTCSR_REG},
    {"cursor", FG_CURSOR_REG},
    {"index",  FG_INDEX_REG}, 
    {"inlut",  FG_INLUT_REG}, 
    {"redgrn", FG_REDGRN_REG},
    {"blue",   FG_BLUE_REG},  
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}
/*-----------------------------------------------------------------------*
 * Peak datatype
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for enumerating frame grabber peak offsets
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PeakDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"x", PEAK_X},
    {"y", PEAK_Y},
    {"xabs", PEAK_XABS},
    {"yabs", PEAK_YABS},
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}

/*.......................................................................
 * Create a new datatype for enumerating frame grabber image statistics.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_ImstatDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"peak", IMSTAT_PEAK},
    {"snr", IMSTAT_SNR},
  };
  return add_ChoiceDataType(sc, name,  en, DIMENSION(en));
}
/*-----------------------------------------------------------------------*
 * OptCamCount
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying stepper motor integer steps.
 * These are stored as integers between -10500 and 10500 (supposedly, full 
 * range is ~ +- 5200). In addition, stepper count variables will 
 * contain -20000 when the user specifies the count as "off", and +20000 
 * when the user specifies the count as "on".
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_OptCamCountDataType(Script *sc, char *name)
{
  DataType *dt;    /* The object to be returned */
  if(!sc || !name) {
    lprintf(stderr, "add_OptCamCount: Invalid argument(s).\n");
    return NULL;
  };
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(IntVariable),
		    0, 0, const_optcam, print_optcam, equal_optcam, 
		    gt_optcam, 0, 0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse an optical camera constant.  The stepper motor has full range
 * +-26 turns x 200 steps/turn = +-5200 steps.
 */
static DT_CONST(const_optcam)
{
  char *usage = "Expected a valid step count or a valid camera control keyword.\n";
  Variable *var;        /* The variable that will contain the number */
  long l;               /* The floating point read from the input stream */
  int i;
  /*
   * Read either an integral count, or an alphanumeric keyword, from the input
   * stream.
   */
  if(isalpha(stream->nextc)) {
    if(input_keyword(stream, 0, 1))
      return input_error(stream, 1, usage);
    /*
     * Loop through the enumerated list, looking for a match.
     */
    for(i=0;i < OptCamNen;i++) {
      if(strcmp(stream->work, OptCamEn[i].name)==0) {
	l = (int) OptCamEn[i].value; /* Pass the OptCamAction as an integer. */
	break;
      }
    }
    /*
     * If no match was found, return error.
     */
    if(i==OptCamNen)
      return input_error(stream, 1, usage);
  }
  else {
    if(input_long(stream, 0, 0, &l))
      return input_error(stream, 1, usage);
    /*
     * Check the domain of the variable.
     */
    if(l < -10500 || l > 10500) {
      return input_error(stream, 1,
			 "Invalid count. Should be -10500 - +10500.\n");
    };
  };
  /*
   * Record the number in a new variable and push that variable onto the
   * expression stack.
   */
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  /*
   * Initialize the variable.
   */
  INT_VARIABLE(var)->i = l;
  return 0;
}
/*.......................................................................
 * Print an optical camera count.
 */
static DT_PRINT(print_optcam)
{
  long l = INT_VARIABLE(var)->i;
  int i;
  /*
   * If less than 20000, this is an integral count.
   */
  if(l < 20000)
    return output_long(output, OUT_DECIMAL, "+", 0, 0, l);
  /*
   * Else loop through the Enumerated list, looking for a match.
   */
  else {
    for(i=0; i < OptCamNen; i++)
      if((OptCamAction)l == (OptCamAction)OptCamEn[i].value)
	return write_OutputStream(output, OptCamEn[i].name);
  }
  /*
   * Else this was not a recognized count.
   */
  lprintf(stderr,"Unrecognized OptCamCount value.\n");
  return 1;
}
/*.......................................................................
 * Test two optical camera counts for equality.
 */
static DT_RELFN(equal_optcam)
{
  return INT_VARIABLE(va)->i == INT_VARIABLE(vb)->i;
}
/*.......................................................................
 * Test whether the value of an optical camera count variable is greater than a
 * second. Note that "off" is reported as less than any
 * other optical camera value, and "on" is reported as greater.
 */
static DT_RELFN(gt_optcam)
{
  return INT_VARIABLE(va)->i > INT_VARIABLE(vb)->i;
}

/*-----------------------------------------------------------------------*
 * SlewRate
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a reduced slew rate.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_SlewRateDataType(Script *sc, char *name)
{
  return add_UintDataType(sc, name, check_slew_rate, sc_iterate_uint,"Integer");
}

/*.......................................................................
 * Check the validity of a slew-rate variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_slew_rate)
{
  unsigned u = UINT_VARIABLE(var)->uint;
  if(u < 10 || u > 100) {
    input_error(stream, 1, "Invalid percentage slew-rate. Should be 10 <= r <= 100.\n");
    return 1;
  };
  return 0;
}

/*-----------------------------------------------------------------------*
 * WalshState                                                           *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the state of the Walsh switching
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_WalshStateDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"on",  WALSH_ON},
    {"off", WALSH_OFF},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * EmailAction                                                           *
 *-----------------------------------------------------------------------*/

/*......................................................................
 * Create a new datatype for specifying an action to perform with a
 * pager email address
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_EmailActionDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"add",      EMAIL_ADD},
    {"clear",    EMAIL_CLEAR},
    {"list",     EMAIL_LIST},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * PagerState                                                           *
 *-----------------------------------------------------------------------*/

/*......................................................................
 * Create a new datatype for specifying the state of the pager
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PagerStateDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"on",     PAGER_ON},
    {"off",    PAGER_OFF},
    {"chip",   PAGER_IP},
    {"email",  PAGER_EMAIL},
    {"disable",PAGER_DISABLE},
    {"enable", PAGER_ENABLE},
    {"clear",  PAGER_CLEAR},
    {"list",   PAGER_LIST},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * PagerDev                                                              *
 *-----------------------------------------------------------------------*/

/*......................................................................
 * Create a new datatype for specifying a pager device
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_PagerDevDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"mapo_redlight", PAGER_MAPO_REDLIGHT},
    {"dome_pager",    PAGER_DOME_PAGER},
    {"eldorm_pager",  PAGER_ELDORM_PAGER},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

//-----------------------------------------------------------------------
// DcPower
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying downconverter powers. Powers
 * are stored as double values > 0.  In addition, a value of < 0 is
 * used to denote a preset value.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DcPowerDataType(Script *sc, char *name)
{
  DataType *dt;    // The object to be returned 
  if(!sc || !name) {
    lprintf(stderr, "add_PhaseShiftDataType: Invalid argument(s).\n");
    return NULL;
  };
  
  // Create the datatype and add it to the symbol table.
  
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(DoubleVariable),
		    0, 0, const_dcpower, print_dcpower, sc_equal_double,
		    sc_gt_double, 0, sc_iterate_double, "Double");
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/**.......................................................................
 * Parse a DcPower constant.
 */
static DT_CONST(const_dcpower)
{
  char *usage = "Expected a valid power level, or the word 'preset'.\n";
  Variable *var; // The variable that will contain the number 
  double d;      // The encoder value read from the input stream 
  
  // Read either a power, or the "preset" keyword
  
  if(isalpha(stream->nextc)) {

    if(input_keyword(stream, 0, 1))
      return input_error(stream, 1, usage);

    if(strcmp(stream->work, "preset")==0) {
      d = 10001;
    } else {
      return input_error(stream, 1, usage);
    };

  } else {
    if(input_double(stream, 0, &d))
      return input_error(stream, 1, usage);
  };
  
  // Record the number in a new variable and push that variable
  // onto the expression stack.
  
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Initialize the variable.
  
  DOUBLE_VARIABLE(var)->d = d;
  return 0;
}

/*.......................................................................
 * Print a DcPower variable.
 */
static DT_PRINT(print_dcpower)
{
  double d = DOUBLE_VARIABLE(var)->d;
  
  if(d > 10000) 
    return write_OutputStream(output, "preset");
  else  
    return output_double(output, "", 0, DBL_DIG, 'g', d);
}

/*-----------------------------------------------------------------------*
 * DelayType
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a datatype for specifying a delay
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DelayTypeDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"fixed",        FIXED},
    {"adjustable",   ADJUSTABLE},
    {"geometric",    GEOMETRIC},
    {"thermal",      THERMAL},
    {"ionospheric",  IONOSPHERIC},
    {"tropospheric", TROPOSPHERIC},
    {"offset",       CORR_OFFSET},
    {"nia",          NIA},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * DDSState                                                           *
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying the state of the DDS output
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_DDSStateDataType(Script *sc, char *name)
{
  static Enumerator en[] = {
    {"enable",  DDS_ENABLE},
    {"disable", DDS_DISABLE},
    {"reset",   DDS_RESET},
  };
  return add_ChoiceDataType(sc, name, en, DIMENSION(en));
}

/*-----------------------------------------------------------------------*
 * TransDev
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new transdev-specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TransDevDataType(Script *sc, char *name)
{
  DataType *dt;                   /* The object to be returned */
  /*
   * Create the datatype and add it to the symbol table.
   */
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(TransDevVariable),
		    0, 0, const_transdev, print_transdev, equal_transdev, 0, 0,
		    0, NULL);
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  return dt;
}

/*.......................................................................
 * Parse a transdev specification constant.
 */
static DT_CONST(const_transdev)
{
  ControlProg *cp = (ControlProg* )sc->project; /* The control-program
						   retransdev object */
  Variable *var;   /* The new variable */
  int length;      /* The length of the transdev name */
  
  // Get the resource object of the logger thread.
  
  SzaArrayLogger *log = (SzaArrayLogger* )cp_ThreadData(cp, CP_LOGGER);
  
  // Read the transdev name from the input stream.
  
  if(input_literal(stream, 0, "",  "",  TransactionManager::notSeparatorChar, ""))
    return input_error(stream, 1, "Missing transdev name.\n");
  
  // Check that the length of the name doesn't exceed the legal limit
  // for transdev names.
  
  length = strlen(stream->work);
  if((unsigned)length >= TransactionManager::DEV_NAME_MAX) {
    return input_error(stream, 1,
		       "TransDev names can't be longer than %d characters.\n",
		       TransactionManager::DEV_NAME_MAX);
  };
  
  // Look up the device to see whether it exists, but only if this is
  // not the current transdev.
  
  if(!log_isValidDevice(log, stream->work)) {
    return input_error(stream, 1,
		       "The transaction catalog does not contain an entry for"
		       " \"%s\".\n", stream->work);
  };
  
  // Create a new TransVar variable and push it onto the expression
  // stack.
  
  var = new_Variable(sc, dt->atom_reg);
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Initialize the variable with the name of the device
  
  strcpy(TRANSDEV_VARIABLE(var)->name, stream->work);
  return 0;
}

/*.......................................................................
 * Print a transdev specification variable.
 */
static DT_PRINT(print_transdev)
{
  return write_OutputStream(output, TRANSDEV_VARIABLE(var)->name);
}

/*.......................................................................
 * Test equality of transdev variables
 */
static DT_RELFN(equal_transdev)
{
  return strcmp(TRANSDEV_VARIABLE(va)->name, TRANSDEV_VARIABLE(vb)->name)==0;
}

/*-----------------------------------------------------------------------*
 * TransLocation
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new location-specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TransLocationDataType(Script *sc, char *name)
{
  return add_StringDataType(sc, name, 0, NULL);
}

/*-----------------------------------------------------------------------*
 * TransSerial
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new serial-number specification datatype and add it to the
 * specified script environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TransSerialDataType(Script *sc, char *name)
{
  return add_StringDataType(sc, name, 0, NULL);
}

/*-----------------------------------------------------------------------*
 * TransWho
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new specification datatype for specifying the person
 * logging a transaction and add it to the specified script
 * environment.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TransWhoDataType(Script *sc, char *name)
{
  return add_StringDataType(sc, name, 0, NULL);
}

/*-----------------------------------------------------------------------*
 * TransWho
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new specification datatype for an optional transaction
 * comment
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name to give the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TransCommentDataType(Script *sc, char *name)
{
  return add_StringDataType(sc, name, 0, NULL);
}

//-----------------------------------------------------------------------
// CalPos
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying a calibator position
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_CalPosDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    {"ambient", sza::util::CalPos::AMBIENT},
    {"load",    sza::util::CalPos::HOTLOAD},
    {"sky",     sza::util::CalPos::SKY},
  };
  return add_SetDataType(sc, name, 0, type, DIMENSION(type), NULL, NULL);
}

// An enumerator for tertiary positions

static Enumerator TertPosEn[] = {
  {"rx1cm",    TERTPOS_RX30GHZ},
  {"rx30ghz",  TERTPOS_RX30GHZ},
  
  {"rx3mm",    TERTPOS_RX90GHZ},
  {"rx90ghz",  TERTPOS_RX90GHZ},
  
  {"rx1mm",    TERTPOS_RX230GHZ},
  {"rx230ghz", TERTPOS_RX230GHZ},
};

int TertPosNen = DIMENSION(TertPosEn);

/**.......................................................................
 * Create a new datatype for specifying stepper tertiary positions
 *
 * These are stored as signed shorts. In addition, tertiary position
 * variables can consist of the values 0x1000 to mean RX30GHZ, 0x2000
 * to mean RX90GHZ, and 0x4000 to mean RX230GHZ.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 *
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_TertPosDataType(Script *sc, char *name)
{
  DataType *dt;    /* The object to be returned */
  
  if(!sc || !name) {
    lprintf(stderr, "add_TertPos: Invalid argument(s).\n");
    return NULL;
  };
  
  // Create the datatype and add it to the symbol table.
  
  dt = new_DataType(sc, name, DT_BUILTIN, NULL, sizeof(IntVariable),
		    0, 0, const_tertpos, print_tertpos, equal_tertpos, 
		    gt_tertpos, 0, 0, NULL);
  
  if(!dt || !add_ScriptSymbol(sc, name, SYM_DATATYPE, dt))
    return NULL;
  
  return dt;
}

/*.......................................................................
 * Parse a tertiary position constant.  The teritary encoder has full
 * range of an unsigned int.
 */
static DT_CONST(const_tertpos)
{
  char *usage = "Expected a valid encoder position or a receiver name.\n";
  Variable *var;       /* The variable that will contain the number */
  long l;              /* The integer read from the input stream */
  int i;
  
  // Read either an integral count, or an alphanumeric keyword, from
  // the input stream.
  
  if(isalpha(stream->nextc)) {
    if(input_keyword(stream, 0, 1))
      return input_error(stream, 1, usage);
    
    // Loop through the enumerated list, looking for a match.
    
    for(i=0;i < TertPosNen;i++) {
      if(strcmp(stream->work, TertPosEn[i].name)==0) {
	l = (int) TertPosEn[i].value; /* Pass the TertPosAction as an integer. */
	break;
      }
    }
    
    // If no match was found, return error.
    
    if(i==TertPosNen)
      return input_error(stream, 1, usage);
  } else {
    if(input_long(stream, 0, 0, &l))
      return input_error(stream, 1, usage);
    
    // Check the domain of the variable.
    
    if(l < 0 || l > 0xFFFF) {
      return input_error(stream, 1,
			 "Invalid count. Should be 0 - %d.\n", 0xFFFF);
    };
  };
  
  // Record the number in a new variable and push that variable onto
  // the expression stack.
  
  var = new_Variable(sc, dt->atom_reg);
  
  if(!var || !add_LoadOper(sc, e, var))
    return 1;
  
  // Initialize the variable.
  
  INT_VARIABLE(var)->i = l;
  
  return 0;
}

/**.......................................................................
 * Print an tertiary position count.
 */
static DT_PRINT(print_tertpos)
{
  long l = INT_VARIABLE(var)->i;
  int i;
  
  // If less than 2^16, this is an integral count.
  
  if(l < 0x10000)
    return output_long(output, OUT_DECIMAL, "", 0, 0, l);
  
  // Else loop through the Enumerated list, looking for a match.
  
  else {
    for(i=0; i < TertPosNen;i++)
      if(l == (long)TertPosEn[i].value)
	return write_OutputStream(output, TertPosEn[i].name);
  }
  
  // Else this was not a recognized tertiary position
  
  lprintf(stderr,"Unrecognized TertPos value.\n");
  return 1;
}

/*.......................................................................
 * Test two tertiary position counts for equality.
 */
static DT_RELFN(equal_tertpos)
{
  return INT_VARIABLE(va)->i == INT_VARIABLE(vb)->i;
}

/*.......................................................................
 * Test whether the value of an tertiary position count variable is greater than a
 * second. Note that "off" is reported as less than any
 * other tertiary position value, and "on" is reported as greater.
 */
static DT_RELFN(gt_tertpos)
{
  return INT_VARIABLE(va)->i > INT_VARIABLE(vb)->i;
}

//-----------------------------------------------------------------------
// CalTertOneWire
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying a caltert one-wire device
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_CalTertOWDeviceDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    {"module",  sza::util::CalTertTypes::MODULE},
    {"encoder", sza::util::CalTertTypes::ENCODER}
  };
  
  return add_SetDataType(sc, name, 0, type, DIMENSION(type), NULL, NULL);
}

//-----------------------------------------------------------------------
// OneWireCommand
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying a one-wire command
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_CalTertOWCommandDataType(Script *sc, char *name)
{
  static Enumerator type[] = {
    {"read",  sza::util::CalTertTypes::READ},
    {"write", sza::util::CalTertTypes::WRITE}
  };
  
  return add_SetDataType(sc, name, 0, type, DIMENSION(type), NULL, NULL);
}

//-----------------------------------------------------------------------
// IF Attenuation
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying IF attenuations
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_IFAttenuationDataType(Script *sc, char *name)
{
  return add_DoubleDataType(sc, name, check_IFAttenuation, sc_iterate_double, "Double",1);
}

/*.......................................................................
 * Check the validity of an IF attenuation variable.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_IFAttenuation)
{
  double atten = DOUBLE_VARIABLE(var)->d;
  
  if(!sza::util::IFAtten::isValidAtten(atten)) {
    std::ostringstream os;
    os << "Invalid IF attenuation. Should be 0 <= atten <= " 
       << sza::util::IFAtten::attenMax_ << std::endl;
    return input_error(stream, 1, os.str().c_str());
  }
  return 0;
}

//-----------------------------------------------------------------------
// IF Level
//-----------------------------------------------------------------------

/*.......................................................................
 * Create a new datatype for specifying IF levels
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_IFLevelDataType(Script *sc, char *name)
{
  return add_DoubleDataType(sc, name, check_IFLevel, sc_iterate_double, "Double",1);
}

/*.......................................................................
 * Check the validity of an IF level.
 *
 * Input:
 *  sc          Script *  The host scripting environment.
 *  var       Variable *  The variable who's value is to be checked.
 *  stream InputStream *  The stream from which the variable value was
 *                        read.
 * Output:
 *  return         int    0 - Value ok.
 *                        1 - Out of bounds.
 */
static DT_CHECK(check_IFLevel)
{
  double level = DOUBLE_VARIABLE(var)->d;
  
  if(!sza::util::IFLevel::isValidLevel(level)) {
    
    std::ostringstream os;
    
    os << "Invalid IF level. Should be 0 <= level <= " 
       << sza::util::IFLevel::maxLevel() << std::endl;
    
    return input_error(stream, 1, os.str().c_str());
  }
  return 0;
}

/*-----------------------------------------------------------------------*
 * GunnDevice
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of local oscillator
 * stages.
 *
 * Input:
 *  sc          Script *  The target scripting environment.
 *  name          char *  The name for the datatype.
 * Output:
 *  return    DataType *  The newly added datatype, or NULL on error.
 */
DataType *add_GunnDeviceDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"gunn",      sza::util::LoStage::LO_GUNN}, 
    {"sweep",     sza::util::LoStage::LO_SWEEP}, 
    {"ifmon",     sza::util::LoStage::LO_IFMONITOR},
    {"atten",     sza::util::LoStage::LO_ATTEN},
    {"backshort", sza::util::LoStage::LO_BACKSHORT},
    {"tuner",     sza::util::LoStage::LO_TUNER},
    {"all",       sza::util::LoStage::LO_ALL}
  };
  
  return add_SetDataType(sc, name, 0, en, DIMENSION(en), NULL, NULL);
}

/*-----------------------------------------------------------------------*
 * ArrayName
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of array names
 */
DataType *add_ArrayNameDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"sza",   sza::util::CarmaConfig::SZA}, 
    {"carma", sza::util::CarmaConfig::CARMA}, 
  };
  
  return add_ChoiceDataType(sc, name, en, DIMENSION(en), true);
}

/*-----------------------------------------------------------------------*
 * ArrayConfig
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of array names
 */
DataType *add_ArrayConfigDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"Unknown", sza::util::CarmaConfig::UNKNOWN}, 
    {"A",  sza::util::CarmaConfig::A}, 
    {"B",  sza::util::CarmaConfig::B}, 
    {"C",  sza::util::CarmaConfig::C}, 
    {"D",  sza::util::CarmaConfig::D}, 
    {"E",  sza::util::CarmaConfig::E}, 
    {"I",  sza::util::CarmaConfig::I}, 
    {"L",  sza::util::CarmaConfig::L},
    {"H",  sza::util::CarmaConfig::H}, 
    {"BP", sza::util::CarmaConfig::BP}, 
    {"AP", sza::util::CarmaConfig::AP}, 
  };
  
  return add_ChoiceDataType(sc, name, en, DIMENSION(en), true);
}


/*-----------------------------------------------------------------------*
 * AntennaType
 *-----------------------------------------------------------------------*/

/*.......................................................................
 * Create a new datatype for specifying a selection of array names
 */
DataType *add_AntennaTypeDataType(Script *sc, char *name)
{
  
  static Enumerator en[] = {
    {"sza",      sza::util::CarmaConfig::SZA}, 
    {"bima",     sza::util::CarmaConfig::BIMA}, 
    {"ovro",     sza::util::CarmaConfig::OVRO}, 
  };
  
  return add_ChoiceDataType(sc, name, en, DIMENSION(en), true);
}
