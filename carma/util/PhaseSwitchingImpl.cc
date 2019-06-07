
/**
 *
 * Implementation of the CORBA phase switching class.
 *
 * @author: Steve Scott
 *
 * $Id: PhaseSwitchingImpl.cc,v 1.7 2011/05/11 17:41:07 iws Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <math.h>
#include <sstream>

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Logger.h"
#include "carma/util/PhaseSwitchingImpl.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace log4cpp;


PhaseSwitchingImpl::PhaseSwitchingImpl()
{
    ps90  = new PhaseSwitchTable();
    ps180 = new PhaseSwitchTable();
}

PhaseSwitchingImpl::~PhaseSwitchingImpl()
{
    delete ps90;
    delete ps180;
}
// helper
bool getStateFromTable(const carma::util::PhaseTable& t,
              const int nCols, const int col, const int row)
{
    const int bytesPerRow = static_cast<int>(ceil(nCols/8.0));
    unsigned char d = t[row*bytesPerRow + (col)/8];
    return 1 & (d >> col%8);
}
void PhaseSwitchingImpl::loadPhaseSwitchTable( CORBA::Long numColumns,
         CORBA::Long numRows90,  const carma::util::PhaseTable& phaseTable90,
         CORBA::Long numRows180, const carma::util::PhaseTable& phaseTable180
        )
try { // Catch everything so that we don't end up at unexpected...

    const bool debugTables = false;

    // Delete old tables and create new ones
    delete ps90;
    delete ps180;
    ps90  = new PhaseSwitchTable(numRows90,  numColumns);
    ps180 = new PhaseSwitchTable(numRows180, numColumns);

    if (debugTables) {
        Program::getLogger() << Priority::INFO << ps90->toString();
        Program::getLogger() << Priority::INFO << ps180->toString();
    }

    // Unpack into the phaseSwitchingTables
    const int bytesPerRow = static_cast<int>(ceil(numColumns/8.0));
    const unsigned int sizeNeeded90 = bytesPerRow*numRows90;
    if (phaseTable90.length() < sizeNeeded90) {
        ostringstream o;
        o << "Size of 90d table (" << phaseTable90.length()
          << ") won't hold cols=" << numColumns << " and rows="
          << numRows90;
        throw CARMA_ERROR(o);
    }
    const unsigned int sizeNeeded180 = bytesPerRow*numRows90;
    if (phaseTable90.length() < sizeNeeded180) {
        ostringstream o;
        o << "Size of 180d table (" << phaseTable180.length()
          << ") won't hold cols=" << numColumns << " and rows="
          << numRows180;
        throw CARMA_ERROR(o);
    }
    for (int c = 0; c < numColumns; c++) {
        for (int r = 0; r < numRows90; r++) {
            bool s = getStateFromTable(phaseTable90, numColumns, c, r);
            ps90->setState(c, r, s);
        }
        for (int r = 0; r < numRows180; r++) {
            bool s = getStateFromTable(phaseTable180, numColumns, c, r);
            ps180->setState(c, r, s);
        }
    }

    if (debugTables) {
        Program::getLogger() << Priority::INFO << ps90->toString();
        Program::getLogger() << Priority::INFO << ps180->toString();
    }
    // Let the registered Observer do something specific with the
    // new table (like send it to a CANbus node or the correlator hw)
    notifyObservers();
} // End all encompassing try block
catch (...) {
    rethrowCaughtAsUser();
}

PhaseSwitchTable* PhaseSwitchingImpl::getPhaseTable90()
{
    return ps90;
}

PhaseSwitchTable* PhaseSwitchingImpl::getPhaseTable180()
{
    return ps180;
}


