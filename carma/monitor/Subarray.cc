/**
 * @file
 * Class to manage details of subarray structure and types of subarrays
 * within CARMA. Ties together monitor information and runtime needs.
 * Monitor information is primary source of structural information.
 *
 * @author: Amar Amarnath
 *
 * $Id: Subarray.cc,v 1.17 2014/06/04 17:09:24 mpound Exp $
 *
 * $CarmaCopyright$
 */

#include <string>

#include "carma/util/checking.h"
#include "carma/monitor/Subarray.h"
#include "carma/monitor/Correlator.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


// create private objects to get at correlator information

const long      Subarray::totalNumAntennas         = 
             ::carma::monitor::ControlSubsystemBase::antennaCount();


struct carma::monitor::SubarrayInfo {
    long                 subarrayNo;
    const char *         name;
    const char *         alphanumericName;
    const util::CorrelatorType correlator; // type of correlator
    bool                 sharedLOref;
};


namespace {


const SubarrayInfo carmaSubarrayInfoTab[ ] = {
 // subarray  subarray         correlator           shared
 //  number,    name,             type,             LOref,

  {  1, "Sci#1", "Sci1",  CORR_NONE, false  },
  {  2, "Sci#2", "Sci2",  CORR_NONE, false  },
  {  3, "Eng#1", "Eng1",  CORR_NONE, true   },
  {  4, "Eng#2", "Eng2",  CORR_NONE, true   },
  // last one must always be the maintenance subarray
  {  5, "Offline", "Offline", CORR_NONE, false  }
};


const int nSubarrays = 
    sizeof (carmaSubarrayInfoTab)/sizeof(carmaSubarrayInfoTab[0]);


const SubarrayInfo &
getSubarrayInfoEntry( const int subarrayNo )
{
    if ( (subarrayNo < 1) || (subarrayNo > nSubarrays) ) {
        string msg;
        {
            ostringstream oss;
            
            oss << "Bad subarray number " << subarrayNo;
            
            msg = oss.str();
        }
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_ERROR( msg );
    }

    for ( int i = 0; i < nSubarrays; ++i ) {
        if ( carmaSubarrayInfoTab[ i ].subarrayNo == subarrayNo )
            return carmaSubarrayInfoTab[ i ];
    }

    string msg;
    {
        ostringstream oss;
        
        oss << "Subarray number " << subarrayNo << " not found in table";
        
        msg = oss.str();
    }
    
    programLogErrorIfPossible( msg );
    
    throw CARMA_ERROR( msg );
}


}  // namespace < anonymous >


Subarray::Subarray( const int subarrayNo ) :
subarrayInfo_( getSubarrayInfoEntry ( subarrayNo ) )
{
    CARMA_CHECK( nSubarrays == ControlSubsystemBase::subarrayCount() );
}


Subarray::~Subarray()
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


int   
Subarray::numSubarrays () 
{
    return nSubarrays;
}

int   
Subarray::numBimaAntennas () 
{
    return ::carma::monitor::BimaSubsystem::COUNT;
}

int   
Subarray::numOvroAntennas () 
{
    return ::carma::monitor::OvroSubsystem::COUNT;
}

int   
Subarray::numSzaAntennas () 
{
    return ::carma::monitor::SzaSubsystem::COUNT;
}


string
Subarray::subarrayName( ) const
{
    return subarrayInfo_.name;
}


string
Subarray::subarrayName( const int subarrayNo )
{
    return getSubarrayInfoEntry( subarrayNo ).name;
}


string
Subarray::subarrayAlphanumericName( ) const
{
    return subarrayInfo_.alphanumericName;
}


string
Subarray::subarrayAlphanumericName( const int subarrayNo )
{
    return getSubarrayInfoEntry( subarrayNo ).alphanumericName;
}


long  
Subarray::subarrayNumber () const
{
    return subarrayInfo_.subarrayNo ;
}


bool  
Subarray::hasCorrelator () const 
{
    CARMA_CHECK (! (subarrayInfo_.correlator != CORR_NONE 
                                        && subarrayInfo_.sharedLOref));
    return (subarrayInfo_.correlator != CORR_NONE);
}


const Correlator  
Subarray::correlator () const 
{
    if (! hasCorrelator())  {
        ostringstream oss;
        oss << "Subarray::correlator() - Subarray # " << subarrayNumber()
            << " (" << subarrayName() << ") "
            << " does not have an associated correlator.";
        throw CARMA_ERROR (oss.str());
    }

    Correlator correlator(subarrayInfo_.correlator);
    return correlator;
}



bool  
Subarray::hasSharedLOref () const 
{
    CARMA_CHECK (! (subarrayInfo_.correlator != CORR_NONE 
                                            && subarrayInfo_.sharedLOref));
    return subarrayInfo_.sharedLOref;
}


