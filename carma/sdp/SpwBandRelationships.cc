/**
 *
 * @file SpwBandRelationships.cc
 *
 * Mapping of correlator bands and sidebands to output spectral windows.
 *
 * $Id: SpwBandRelationships.cc,v 1.11 2009/12/31 04:11:03 friedel Exp $
 *
 * @author Athol Kemball
 */

// Carma includes
#include "carma/sdp/SpwBandRelationships.h"

// Carma tools includes

// C++ standard library or system includes

// Namespace using directives
using namespace carma::sdp;

namespace carma {
  namespace sdp {

  int SpwBandRelationships::numWin=0;

//============================================================================

SpwBandRelationships::SpwBandRelationships() 
{
  // Null constructor.
};

//============================================================================

SpwBandRelationships::~SpwBandRelationships()
{
  // Destructor
};

//============================================================================

int SpwBandRelationships::numSpw()
{
  // Return the number of output spectral windows.
  return SpwBandRelationships::numWin;
};

//============================================================================

bool SpwBandRelationships::isLSB(const int& spw)
{
  // Return true if specified spectral window is LSB.
  return (spw < numSpw()/2);
};

//============================================================================

bool SpwBandRelationships::isUSB(const int& spw)
{
  // Return true if specified spectral window is USB.
  return (spw >= numSpw()/2);
};

//============================================================================

int SpwBandRelationships::matchingSbSpw(const int& spw)
{
  // Return matching sideband spw for a specified input spw
  return (spw >= numSpw()/2) ? spw - numSpw()/2 : spw + numSpw()/2;
};

//============================================================================

std::vector<int> SpwBandRelationships::bandSbToSpw(const int& band,
						   const SIDEBAND& sideband)
{
  // Map a (band,sideband) tuple to the matching spectral windows.

  // Initialization
  std::vector<int> retval(sideband != DSB ? 1 : 2);

  // Case sideband of:
  switch (sideband) {
  case LSB:
    retval[0] = band;
    break;
  case USB:
    retval[0] = band + numSpw()/2;
    break;
  case DSB:
    retval[0] = band;
    retval[1] = band + numSpw()/2;
    break;
  };
  return retval;
};

//============================================================================

int SpwBandRelationships::spwToBand(const int& spw)
{
  // Map a spectral window to the matching band number.
  return (spw % (numSpw()/2));
};

//============================================================================

SpwBandRelationships::SIDEBAND 
SpwBandRelationships::spwToSideband(const int& spw)
{
  // Map a spectral window to the matching (single) sideband.
  return (spw < numSpw()/2 ? SpwBandRelationships::LSB : 
	  SpwBandRelationships::USB);
};

//============================================================================

int SpwBandRelationships::bandSbIndex(const int& band,
				      const SIDEBAND& sideband)
{
  // Map a (band,sideband) tuple to a unique sequential array index.

  return (numSpw()/2 * band + static_cast<int>(sideband));
};

//============================================================================
std::pair<int, SpwBandRelationships::SIDEBAND> 
SpwBandRelationships::bandSbIndexInv(const int& index)
{
  // Compute inverse mapping of bandSbIndex method.

  // Initialization
  std::pair<int, SIDEBAND> retval;

  // Set band and sideband
  retval.first = index / (numSpw()/2);
  retval.second = static_cast<SIDEBAND>(index%(numSpw()/2));

  return retval;
};

//============================================================================

int SpwBandRelationships::bandFreqIndex(const int& band,
					const SIDEBAND& sideband)
{
  // Map a (band,sideband) tuple to a bandfreq astro header index.

  return (2 * band + 1 - (static_cast<int>(sideband) % 2));
};

//============================================================================

int SpwBandRelationships::sysTempIndex(const int& ant, const int& nant,
				       const int& band, 
				       const SIDEBAND& sideband)
{
  // Map a (ant,band,sideband) tuple to a systemp astro header index

  return (3 * band * nant + 3 * ant + static_cast<int>(sideband));
};
//============================================================================

int SpwBandRelationships::psysIndex(const int& ant, const int& nant,
				    const int& band)
{
  // Map a (ant,band,sideband) tuple to a systemp astro header index

  return ( band * nant + ant);
};
//============================================================================

void SpwBandRelationships::setNumWin(const int &num)
{
  // Set the number of widows in the data set
  SpwBandRelationships::numWin = num;
};
} // namespace sdp
} // namespace carma
