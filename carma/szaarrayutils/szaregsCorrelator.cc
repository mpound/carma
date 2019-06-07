#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/miscregs.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/LoMonitorFlags.h"
#include "carma/szautil/LobeRotatorFlags.h"
#include "carma/szautil/Oscillator.h"
#include "carma/szautil/TimeVal.h"

#include <map>

using namespace sza::util;
using namespace sza::array;

/**.......................................................................
 * Define the register map of a single correlator band.
 */
static RegBlockTemp szaBand[] = {
  
  // True when data for this band have been received from the correlator
  
  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data were not received for this band</li>"
	       "<li>Bit 1 high -- Data were received for this band</li></ul>",
	       "received", REG_UCHAR|REG_UNION),

  RegBlockTemp("A bitmask of states corresponding to:<br/><ul>"
	       "<li>Bit 0 high -- Data were not received for this baseline</li>"
	       "<li>Bit 1 high -- Data were received for this baseline</li></ul>",
	       "baselineReceived", REG_UCHAR|REG_UNION, 0, AntNum::NBASE),

  RegBlockTemp("The input source to the correlator",
	       "source", REG_UCHAR, 0, 12),

  RegBlockTemp("A complex floating-point register, consisting of (re, im) pairs "
	       "for the upper sideband (USB) cross-correlation.  The first dimension of this register "
	       "indexes the baseline number (i-j), in the order: 0-1, 0-2 ... 0-7, 1-2, 1-3, etc. "
	       "The second dimension indexes the number of frequency channels, from highest to lowest, "
	       "into which this 500 MHz band is divided, corresponding to the values "
	       "stored in the \"frequency\" register of this board",
	       "usb",      REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE, 
	       CorrelatorBand::NCHAN_TOTAL),
  
  RegBlockTemp("(re, im) pairs for the USB cross-correlation, averaged over frequency "
	       "channels. First dimension indexes the baseline number",
	       "usbAvg",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE),
  
  RegBlockTemp("The variance of the frequency-averaged USB cross-correlations. "
	       "The first dimension indexes the baseline number",
  	       "usbVar",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE),
  
  RegBlockTemp("The scalar-averaged amplitude for the USB cross-correlation."
	       " First dimension indexes the baseline number",
	       "usbAmplitude", REG_PREAVG|REG_FLOAT, 0, AntNum::NBASE,
	       CorrelatorBand::NCHAN_TOTAL),

  RegBlockTemp("The scalar-averaged amplitude for the USB cross-correlation."
  	       " First dimension indexes the baseline number",
  	       "usbAvgAmplitude", REG_PREAVG|REG_FLOAT, 0, AntNum::NBASE),

  RegBlockTemp("A complex floating-point register, consisting of (re, im) pairs "
	       "for the lower sideband (LSB) cross-correlation.  (see documentation for \"usb\")",
	       "lsb",      REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE, 
	       CorrelatorBand::NCHAN_TOTAL),
    
  RegBlockTemp("(re, im) pairs for the LSB cross-correlation, averaged over frequency "
	       "channels. First dimension indexes the baseline number",
	       "lsbAvg",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE),
  
  RegBlockTemp("The variance of the frequency-averaged LSB cross-correlations. "
	       "The first dimension indexes the baseline number",
	       "lsbVar",   REG_PREAVG|REG_COMPLEX|REG_FLOAT, 0, AntNum::NBASE),
  
  RegBlockTemp("The scalar-averaged amplitude for the LSB cross-correlation."
  	       " First dimension indexes the baseline number",
  	       "lsbAmplitude", REG_PREAVG|REG_FLOAT, 0, AntNum::NBASE,
  	       CorrelatorBand::NCHAN_TOTAL),

  RegBlockTemp("The scalar-averaged amplitude for the LSB cross-correlation."
  	       " First dimension indexes the baseline number",
  	       "lsbAvgAmplitude", REG_PREAVG|REG_FLOAT, 0, AntNum::NBASE),

  RegBlockTemp("Autocorrelation, averaged over frequency, one per antnena",
	       "autoAvg",  REG_PREAVG|REG_FLOAT,             0, AntNum::NANT),
  
  RegBlockTemp("Variance of the frequency-averaged autocorrelation, one per antenna",
	       "autoVar",  REG_PREAVG|REG_FLOAT,             0, AntNum::NANT),
  
  RegBlockTemp("Autocorrelation.  First dimension indexes antenna number, second indexes frequency channel",
	       "auto",     REG_PREAVG|REG_FLOAT,             0, AntNum::NANT, 
	       CorrelatorBand::NCHAN_TOTAL),
  
  RegBlockTemp("The center frequency of this 500 MHz band (GHz)",
	       "centerFrequency", REG_PREAVG|REG_FLOAT,      0, 1),
  
  RegBlockTemp("The frequency of each channel (GHz)",
	       "frequency",REG_PREAVG|REG_FLOAT,      0, 
	       CorrelatorBand::NCHAN_TOTAL),
};

//-----------------------------------------------------------------------
// Template for band-specific regs
//-----------------------------------------------------------------------

/**.......................................................................
 * Collect the band-specific boards into an array and give them names.
 */
static RegBoardTemp sza_correlator_boards[] = {
  { "band0",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""},
  { "band1",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band2",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band3",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band4",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band5",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band6",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band7",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band8",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  { "band9",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band10",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band11",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band12",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band13",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band14",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
  {"band15",   szaBand,    ARRAY_DIM(szaBand),   {0x0}, ""}, 
};

/**.......................................................................
 * Create a template for the array.
 */
static RegTemplate sza_correlator_template = {
  sza_correlator_boards,   ARRAY_DIM(sza_correlator_boards)
};

// And a public accessor for it

RegTemplate* getSzaCorrelatorTemplate()
{
  return &sza_correlator_template;
}

/**.......................................................................
 * Create the SZA correlator register map.
 *
 * Output:
 *  return    SzaRegMap *   The SZA register container object.
 */
SzaRegMap *new_SzaCorrelatorRegMap(void)
{
  return new RegMap(&sza_correlator_template);
}

/**.......................................................................
 * Delete a register map that was previously returned by new_SzaRegMap().
 *
 * Input:
 *  regs    SzaRegMap *  The register map to be deleted.
 * Output:
 *  return  SzaRegMap *  The deleted register map (always NULL).
 */
SzaRegMap *del_SzaCorrelatorRegMap(SzaRegMap *regs)
{
  delete regs;
  return NULL;
}

/**.......................................................................
 * Pack the current register map for transmission over a network.
 *
 * Input:
 *  net   NetBuf *  The network buffer in which to pack the register
 *                  map. It is left to the caller to call
 *                  net_start_put() and net_end_put().
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
int net_put_SzaCorrelatorRegMap(NetBuf *net)
{
  // Pack the register map via its template.
  
  return net_put_RegTemplate(&sza_correlator_template, net);
}

/**.......................................................................
 * Return the number of bytes needed by net_put_SzaBandRegMap() to pack the
 * current register map into a network buffer.
 *
 * Output:
 *  return  long   The number of bytes required.
 */
long net_SzaCorrelatorRegMap_size(void)
{
  return net_RegTemplate_size(&sza_correlator_template);
}

