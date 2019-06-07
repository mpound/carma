#ifndef COBRADATASIM_H
#define COBRADATASIM_H

#include <vector>
#include <complex>
#include "CobraDataFormat.h"
#include "CobraCorrelationInfo.h"
#include "CobraCorrelationData.h"
#include "CobraIntegrationData.h"

namespace carma {
  namespace correlator {
	namespace lib {
		
// Functions for generating COBRA Wideband simulated data
std::vector<float> CobraAutoSpectraVectorSim(
		int band, int an);

std::vector<std::complex<float> > CobraCrossSpectraSim(
		int band, int an1, int an2, int sideband);

CobraDataFormat CobraDataFormatSim(int bandNum);
std::vector<CobraCorrelationInfo> CobraCorrelationInfoSim(
		CobraDataFormat &format);
CobraIntegrationData CobraIntegrationDataSim(
		CobraDataFormat &format, std::vector<CobraCorrelationInfo> &info);
CobraAutoSpectra CobraAutoSpectraSim(
		CobraDataFormat &format, CobraCorrelationInfo &info);
CobraCrossSpectra CobraCrossSpectraSim(
		CobraDataFormat &format, CobraCorrelationInfo &info);



	}; // lib
  }; // correlator
}; // carma

#endif
