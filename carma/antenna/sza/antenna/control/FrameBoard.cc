#include <cmath>

#include "carma/antenna/sza/antenna/control/FrameBoard.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor for a FrameBoard object
 */
FrameBoard::FrameBoard(SzaShare* share, string name) : 
  Board(share, name)
{
  // Initialize everything to NULL

  nsnap_      = 0;
  record_     = 0;
  utc_        = 0;
  lst_        = 0;
  features_   = 0;
  markSeq_    = 0;

  // Look up registers of the frame board

  nsnap_      = findReg("nsnap");
  record_     = findReg("record");
  utc_        = findReg("utc");
  lst_        = findReg("lst");
  features_   = findReg("features");
  markSeq_    = findReg("markSeq");
}

/**.......................................................................
 * Update the record number
 */
void FrameBoard::archiveRecordNumber(unsigned record)
{
  writeReg(record_, 0, 1, &record);
}

/**.......................................................................
 * Record the time in the register database
 */
void FrameBoard::setTime()
{
  share_->setClock();
}

/**.......................................................................
 * Record the time in the register database
 */
void FrameBoard::archiveTime()
{
  unsigned utc[2];   // The current UTC as a Modified Julian Day
                     // number and the time of day in milli-seconds.
  double days;       // The MJD day number
  unsigned lst;      // The local sidereal time in milliseconds

  // Get the current UTC as a Modified Julian Date.
 
  double mjd = share_->getUtc();

  // Split the MJD UTC into integral days and milliseconds parts.
 
  // Milli-seconds

  utc[1] = static_cast<unsigned int>(modf(mjd, &days) * daysec * 1000.0);  
  
  utc[0] = static_cast<unsigned int>(days); // Days

  sza::util::RegDate date(utc[0], utc[1]);

  // Get the Local Sidereal Time in milliseconds.
 
  lst = static_cast<unsigned int>(share_->getLst(mjd) * (daysec / twopi) 
				  * 1000.0);

  // Record the new values.
 
  share_->writeReg(utc_, date.data());
  writeReg(lst_, 0, 1, &lst);
}

/**.......................................................................
 * Update the features bitmask
 */
void FrameBoard::archiveFeatures(unsigned features, unsigned seq)
{
  writeReg(features_, 0, 1, &features);
  writeReg(markSeq_, 0, 1, &seq);
}

/**.......................................................................
 * Update the local register that contains a count of the number
 * of integrated snapshots per frame.
 */
void FrameBoard::archiveNsnap(unsigned nsnap)
{
  writeReg(nsnap_, 0, 1, &nsnap);
}
