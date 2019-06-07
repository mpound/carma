/**
 * @file carma/interferometry/DelayStatus.cc
 *
 * The implementation of the Delay Information class.
 * $Id: DelayStatus.cc,v 1.3 2006/06/06 15:30:19 mpound Exp $
 * @author Marc Pound
 */

#include <iostream>
#include <vector>
#include "carma/interferometry/DelayStatus.h"

using namespace std;
using namespace carma::interferometry;

// constructor
DelayStatus::DelayStatus(unsigned short nAntennas) {

    // set the delay state control variables
    useAdjustable   = vector<bool>(nAntennas);
    useGeometric    = vector<bool>(nAntennas);
    useHeight    = vector<bool>(nAntennas);
    useIonospheric  = vector<bool>(nAntennas);
    useThermal      = vector<bool>(nAntennas);
    useTropospheric = vector<bool>(nAntennas);

    initializeVectors();
}



DelayStatus::~DelayStatus() {

}


void DelayStatus::initializeVectors() {

    useAdjustable.assign(useAdjustable.size(),true);
    useGeometric.assign(useGeometric.size(),true);
    useHeight.assign(useHeight.size(),true);
    useIonospheric.assign(useIonospheric.size(),true);
    useThermal.assign(useThermal.size(),true);
    useTropospheric.assign(useTropospheric.size(),true);
}
