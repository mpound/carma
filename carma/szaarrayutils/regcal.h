#ifndef regcal_h
#define regcal_h

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/regset.h"
#include "carma/szaarrayutils/regdata.h"
#include "carma/szaarrayutils/input.h"

typedef struct RegCal RegCal;

/*
 * This module provides facilities for converting selected 32-bit int
 * archived data to physical double precision values. Calibration
 * offsets and scale factors to be applied during this process are
 * taken from a RegCal object. Such objects are initialized from text
 * input streams. In addition, integrated registers are divided by the
 * value of the frame.nsnap register to yield mean values per
 * snapshot. This requires that the frame.nsnap register slot contain
 * valid data (for this reason MonitorStream objects silently select
 * this register if it isn't selected for monitoring).
 *
 * Some archive values such as bit-masks are inherently 32 bit
 * integers and will presumably be promptly returned to that form.
 * Assuming that the calibration file doesn't specify a non-unity
 * scale factor or a non-zero offset, we can guarantee that the
 * conversion to and from a double is lossless because ANSI/ISO C
 * mandates that a double must be able to exactly represent any
 * number of at least 10 decimal digits.
 *
 * The calibration of scalar registers is implemented as:
 *
 *  reg[i] = offset + factor * reg[i].
 *
 * The calibration of complex registers is implemented as:
 *
 *  real = reg[i]
 *  imag = reg[i+1]
 *  reg[i]   = offset + factor * real;
 *  reg[i+1] = offset + factor * (imag/imag_gain + real * sin(phi)) / cos(phi);
 */

/*
 * Create a calibration object for a given register map.
 */
RegCal *new_RegCal(ArrayMap *arraymap);

/*
 * Delete a redundant calibration object.
 */
RegCal *del_RegCal(RegCal *regcal);

/*
 * Load calibration parameters from a file. Note that this is just a
 * convenient wrapper around load_cal_stream() that creates a
 * temporary input stream and attaches it to the specified file.
 */
int load_cal_file(ArrayMap *arraymap, RegCal *regcal, char *dir, char *name);

/*
 * Load calibration parameters from an input stream.
 */
int load_cal_stream(ArrayMap *arraymap, RegCal *regcal, InputStream *stream);

/*
 * The following structure contains a double precision array having
 * the same dimension as an archive frame. calibrate_regdata()
 * returns its results in a container of this type.
 */
typedef struct {
  unsigned nslot;    /* The number of elements in slot[] */
  double *slots;     /* The array of monitored registers */
  int empty;         /* True until the first successful call to */
                     /*  calibrate_regdata(). */
} RegCalData;

/*
 * The following functions create and destroy RegCalData objects.
 * Note that a given RegCalData object should only be used with
 * the register map that was passed to new_RegCalData().
 */
RegCalData *new_RegCalData(ArrayMap *arraymap);
RegCalData *del_RegCalData(RegCalData *cal);

/*
 * Calibrate selected registers while copying them from a raw archive
 * array of 32-bit unsigned integers to a parallel double precision array.
 */
int calibrate_regdata(ArrayMap *arraymap, RegCal *regcal, RegSet *regset,
		      RegRawData *raw, RegCalData *cal);

/*
 * Reset all calibration multipliers to 1.0 and zero all calibration
 * offsets.
 */
int reset_RegCal(RegCal *regcal);

/*
 * The following functions can be used to retrieve calibrated data
 * from a RegCalData array. The reg argument should be a register
 * specification that convers the register index range index..index+n-1.
 */
int get_cal_float(RegCalData *cal, ArrRegMapReg *reg, unsigned index, unsigned n,
		  float *data);
int get_cal_double(RegCalData *cal, ArrRegMapReg *reg, unsigned index,
		   unsigned n, double *data);
int get_cal_uint(RegCalData *cal, ArrRegMapReg *reg, unsigned index,
		 unsigned n, unsigned *data);
int get_cal_int(RegCalData *cal, ArrRegMapReg *reg, unsigned index,
		unsigned n, int *data);
int get_cal_ulong(RegCalData *cal, ArrRegMapReg *reg, unsigned index,
		  unsigned n, unsigned long *data);
int get_cal_long(RegCalData *cal, ArrRegMapReg *reg, unsigned index,
		 unsigned n, long *data);
int get_cal_string(RegCalData *cal, ArrRegMapReg *reg, unsigned nc, char *string);

#endif
