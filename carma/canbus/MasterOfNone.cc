/** @file
 * Definition of carma::canbus::MasterOfNone class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2004/12/16 22:47:07 $
 * $Id: MasterOfNone.cc,v 1.1 2004/12/16 22:47:07 abeard Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/canbus/MasterOfNone.h"

using namespace carma::canbus;

// -----------------------------------------------------------------------------
MasterOfNone::MasterOfNone(int boardId, int canbus)
    : Master(boardId, canbus)
{
    // Nothing.
}

// -----------------------------------------------------------------------------
MasterOfNone::MasterOfNone(int boardId)
    : Master(boardId)
{
    // Nothing.
}

// -----------------------------------------------------------------------------
MasterOfNone::~MasterOfNone()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void MasterOfNone::updateStatus()
{
    // Nothing to update
}
