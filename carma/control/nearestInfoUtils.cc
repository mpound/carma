#include "carma/control/nearestInfoUtils.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/programLogging.h"
#include <set>
#include <iostream>
using namespace carma;
using namespace carma::control;
using namespace carma::util;
using namespace carma::services;
using namespace std;

/** 
* @brief Convert Neighbor objects to an IDL sequence of 
* NearestInfo objects
* Output of this method should be used to construct
* a NearestInfoSeq_var.
* @return an IDL sequence of NearestInfo objects given an
*         std::set< carma::services::Neighbor >
* @param neighborSet The std::set< carma::services::Neighbor > to 
* convert.
*/
NearestInfoSeq_var
carma::control::convertNeighborSetToNearestInfoSeq( const NeighborSet & neighborSet )
{
    ScopedLogNdc ndc("control::convertNeighborSetToNearestInfoSeq");
    /*
    {
        ostringstream os;
        os << " Entering with set of size " << neighborSet.size();
        programLogNoticeIfPossible( os.str() );
    }
    */
    const unsigned int nsize = neighborSet.size();
    NearestInfoSeq_var nis( new NearestInfoSeq( nsize ) );
    nis->length( nsize );
    NeighborSet::iterator si = neighborSet.begin();
    NeighborSet::iterator se = neighborSet.end();
    unsigned int i = 0;
    while ( si != se ) {
        //programLogNoticeIfPossible(" calling copyNeighbor ");
        NearestInfo n = copyNeighbor( *si );
        //programLogNoticeIfPossible(" returned from copyNeighbor ");
        // CORBA EXCEPION HERE
        nis[i] = n;
        i++;
        si++;
    }
    //programLogNoticeIfPossible(" returning ");

    return nis;
}

/**
* @brief Copy the data from a single services::Neighbor object 
* to a control::NearestInfo object.
* @param neighbor Input Neighbor object
*/
NearestInfo 
carma::control::copyNeighbor( const Neighbor & neighbor ) {
    ScopedLogNdc ndc("control::copyNeighbor");
    NearestInfo info;
    info.name       = neighbor.getName().c_str();
    info.reference  = neighbor.getReference().c_str();
    info.azimuth    = neighbor.getAzimuth();
    info.elevation  = neighbor.getElevation();
    info.distance   = neighbor.getDistance();
    info.brightness = neighbor.getBrightness();
    info.isOptical  = neighbor.isOptical();
    info.mjd        = neighbor.getMJD();
    
    /*
    {
        ostringstream os;
        os << info.name << " / returning";
        programLogNoticeIfPossible( os.str() );
    }
    */
    return info;
}
