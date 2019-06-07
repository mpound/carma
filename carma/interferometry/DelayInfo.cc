/**
 * @file carma/interferometry/DelayInfo.cc
 *
 * The implementation of the Delay Information class.
 * $Id: DelayInfo.cc,v 1.24 2010/09/24 20:30:32 mpound Exp $
 * @author Marc Pound
 */

#include <vector>
#include <iostream>
#include <iomanip>
#include "carma/interferometry/DelayInfo.h"
#include "carma/services/Atmosphere.h"
#include "carma/services/DecAngle.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::interferometry;
using namespace carma::services;
using namespace carma::util;

// constructor
DelayInfo::DelayInfo() {

    // get the number of available antennas
    unsigned short maxants = numAntennas();

    // slowly-varying antenna parameters
    X         = vector<double>(maxants);
    Y         = vector<double>(maxants);
    Z         = vector<double>(maxants);
    U         = vector<double>(maxants);
    V         = vector<double>(maxants);
    W         = vector<double>(maxants);
    latitude  = vector<double>(maxants);
    longitude = vector<double>(maxants);
    altitude  = vector<double>(maxants);
    axisMis   = vector<double>(maxants);


    // rapidly-varying antenna (source) parameters
    pntRa     = vector<double>(maxants);
    pntDec    = vector<double>(maxants);
    phsRa     = vector<double>(maxants);
    phsDec    = vector<double>(maxants);
    pntAz     = vector<double>(maxants);
    pntEl     = vector<double>(maxants);
    phsAz     = vector<double>(maxants);
    phsEl     = vector<double>(maxants);
    pntState  = vector<pntStateType>(maxants);

    // all delays
    axisDelay         = vector<double>(maxants);
    geometricDelay    = vector<double>(maxants);
    heightDelay       = vector<double>(maxants);
    ionosphericDelay  = vector<double>(maxants);
    thermalDelay      = vector<double>(maxants);
    totalDelay        = vector<double>(maxants);
    totalDelayPol1    = vector<double>(maxants);
    totalDelayPol2    = vector<double>(maxants);
    troposphericDelay = vector<double>(maxants);

    // misc
    pathlength   = vector<double>(maxants);
    refractivity = vector<double>(maxants);

    // time vectors
    timestamp       = vector<double>(maxants);
    calculatedAt    = vector<double>(maxants);
    validUntil      = vector<double>(maxants);
    source          = vector<string>(maxants);

    reset(); // Initialize everything
}



DelayInfo::~DelayInfo() {
    // nothing to do here
}

void DelayInfo::copy(unsigned short i, DelayInfo& from) 
{

    X.at(i) = from.X.at(i);
    Y.at(i) = from.Y.at(i);
    Z.at(i) = from.Z.at(i);
    U.at(i) = from.U.at(i);
    V.at(i) = from.V.at(i);
    W.at(i) = from.W.at(i);
    latitude.at(i) = from.latitude.at(i);
    longitude.at(i) = from.longitude.at(i);
    altitude.at(i) = from.altitude.at(i);
    axisMis.at(i) = from.axisMis.at(i);

    pntRa.at(i)    = from.pntRa.at(i);
    pntDec.at(i)   = from.pntDec.at(i);
    phsRa.at(i)    = from.phsRa.at(i) ;
    phsDec.at(i)   = from.phsDec.at(i);
    pntAz.at(i)    = from.pntAz.at(i) ;
    phsAz.at(i)    = from.phsAz.at(i) ;
    pntEl.at(i)    = from.pntEl.at(i) ;
    phsEl.at(i)    = from.phsEl.at(i) ;
    pntState.at(i) = from.pntState.at(i);

    axisDelay.at(i)         = from.axisDelay.at(i);
    geometricDelay.at(i)    = from.geometricDelay.at(i);
    heightDelay.at(i)       = from.heightDelay.at(i);
    ionosphericDelay.at(i)  = from.ionosphericDelay.at(i);
    thermalDelay.at(i)      = from.thermalDelay.at(i);
    totalDelay.at(i)        = from.totalDelay.at(i)  ;
    totalDelayPol1.at(i)    = from.totalDelayPol1.at(i)  ;
    totalDelayPol2.at(i)    = from.totalDelayPol2.at(i)  ;
    troposphericDelay.at(i) = from.troposphericDelay.at(i);
    pathlength.at(i)        = from.pathlength.at(i)   ;
    refractivity.at(i)      = from.refractivity.at(i) ;

    timestamp.at(i)     =  from.timestamp.at(i)    ;
    calculatedAt.at(i)  =  from.calculatedAt.at(i) ;
    validUntil.at(i)    =  from.validUntil.at(i)   ;
    airTemp     = from.airTemp;
    atmPressure = from.atmPressure;
    relHumid    = from.relHumid;
    source.at(i) = from.source.at(i);
    // don't copy the name

}

void DelayInfo::reset(unsigned short i) 
{
    X.at(i) = 0.0;
    Y.at(i) = 0.0;
    Z.at(i) = 0.0;
    U.at(i) = 0.0;
    V.at(i) = 0.0;
    W.at(i) = 0.0;
    latitude.at(i) = 0.0;
    longitude.at(i) = 0.0;
    altitude.at(i) = 0.0;
    axisMis.at(i) = 0.0;

    pntRa.at(i) = 0.0;
    pntDec.at(i) = 0.0;
    phsRa.at(i) = 0.0;
    phsDec.at(i) = 0.0;
    pntAz.at(i) = 0.0;
    pntEl.at(i) = 0.0;
    phsAz.at(i) = 0.0;
    phsEl.at(i) = 0.0;
    pntState.at(i) = RADEC;

    axisDelay.at(i)         = 0.0;
    geometricDelay.at(i)    = 0.0;
    heightDelay.at(i)       = 0.0;
    ionosphericDelay.at(i)  = 0.0;
    thermalDelay.at(i)      = 0.0;
    totalDelay.at(i)        = 0.0;
    totalDelayPol1.at(i)    = 0.0;
    totalDelayPol2.at(i)    = 0.0;
    troposphericDelay.at(i) = 0.0;

    pathlength.at(i)   = 0.0;
    refractivity.at(i) = 0.0;

    timestamp.at(i)    = 0.0;
    calculatedAt.at(i) = 0.0;
    validUntil.at(i) = 0.0;
    source.at(i) = "NONE";
    airTemp     = Atmosphere::DEFAULT_AIR_TEMP;
    atmPressure = Atmosphere::DEFAULT_ATM_PRESSURE; 
    relHumid    = Atmosphere::DEFAULT_RH;

}
void DelayInfo::reset() {
    initializeMembers();
    initializeVectors();
}

//========== PRIVATE METHODS ==============/
void DelayInfo::initializeMembers() {
    airTemp     = Atmosphere::DEFAULT_AIR_TEMP;
    atmPressure = Atmosphere::DEFAULT_ATM_PRESSURE; 
    relHumid    = Atmosphere::DEFAULT_RH;
    name = "unnamed";
}

void DelayInfo::initializeVectors() {

    // set all vector values to zero
    X.assign(X.size(),0.0);
    Y.assign(Y.size(),0.0);
    Z.assign(Z.size(),0.0);
    U.assign(U.size(),0.0);
    V.assign(V.size(),0.0);
    W.assign(W.size(),0.0);
    latitude.assign(latitude.size(),0.0);
    longitude.assign(longitude.size(),0.0);
    altitude.assign(altitude.size(),0.0);
    axisMis.assign(axisMis.size(),0.0);

    pntRa.assign(pntRa.size(),0.0);
    pntDec.assign(pntDec.size(),0.0);
    phsRa.assign(phsRa.size(),0.0);
    phsDec.assign(phsDec.size(),0.0);
    pntAz.assign(pntAz.size(),0.0);
    pntEl.assign(pntEl.size(),0.0);
    phsAz.assign(pntAz.size(),0.0);
    phsEl.assign(pntEl.size(),0.0);
    pntState.assign(pntState.size(),RADEC);


    axisDelay.assign(axisDelay.size(),0.0);
    geometricDelay.assign(geometricDelay.size(),0.0);
    heightDelay.assign(heightDelay.size(),0.0);
    ionosphericDelay.assign(ionosphericDelay.size(),0.0);
    thermalDelay.assign(thermalDelay.size(),0.0);
    totalDelay.assign(totalDelay.size(),0.0);
    totalDelayPol1.assign(totalDelayPol1.size(),0.0);
    totalDelayPol2.assign(totalDelayPol2.size(),0.0);
    troposphericDelay.assign(troposphericDelay.size(),0.0);

    pathlength.assign(troposphericDelay.size(),0.0);
    refractivity.assign(troposphericDelay.size(),0.0);

    timestamp.assign(timestamp.size(),0.0);
    calculatedAt.assign(calculatedAt.size(),0.0);
    validUntil.assign(validUntil.size(),0.0);
    source.assign(source.size(),"NONE");

}
