#include <cmath>
#include "CobraDataSim.h"
using namespace std;

namespace carma {
  namespace correlator {
    namespace lib {
		
// Create a uniform magnitude spectrum, with a notch
// at an antenna dependent location. Scale the
// spectrum by the band number.
//
// Assumptions:
// band     = 1, 2, 3, etc
// an1, an2 = 1, 2, 3, 4, 5, 6
// numChans = 33
// 
vector<float> CobraAutoSpectraVectorSim(
		int band, int an)
{
	float mag;
	int numChans = 33;
	vector<float> data(numChans);

	// Check for bad band index
	if (band == 0) {
		band = 1;
	}

	for (int i = 0; i < numChans; i++) {
		// Nominal magnitude
		mag = 1.0*band;
		
		// Antenna notch
		if (i == 5*an - 2) {
			mag = 0.9*band;
		}

		// First/Last channel amplitude
		if ((i == 0) || (i == numChans-1)) {
			mag = 0.1*band;
		}
		
		// Spectra
    	data.at(i) = mag;
	}
	return data;
}

vector<complex<float> > CobraCrossSpectraVectorSim(
		int band, int an1, int an2, int sideband)
{
	float mag, pha;
	float pi = M_PIl; /* in math.h included by cmath */
	int numChans = 33;
	vector<complex<float> > data(numChans);

	// Check for bad band index
	if (band == 0) {
		band = 1;
	}
	
	for (int i = 0; i < numChans; i++) {
		// Nominal magnitude and phase
		mag = 1.0*band;
		pha = pi/(numChans-1)*i - pi/2;
		
		// Antenna notches
		if (i == 5*an1 - 2) {
			mag = 0.9*band;
		}
		if (i == 5*an2 - 2) {
			mag = 0.9*band;
		}

		// First/Last channel amplitude
		if ((i == 0) || (i == numChans-1)) {
			mag = 0.1*band;
		}
		
		// Spectra
		if (sideband == 0) {
    		data[i] = complex<float>(mag*cos(pha), mag*sin(pha));
		} else {
			// Invert the phase to make them different
    		data[i] = complex<float>(mag*cos(-pha), mag*sin(-pha));
		}
	}
	return data;
}

CobraDataFormat CobraDataFormatSim(int bandNum)
{
	CobraDataFormat format;

	/* COBRA Wideband 500ms integrations */
	int   id             = 0x11111111;
	int   band           = 1;
	int   nauto          = 6;
	int   nauto_samples  = 33;
	int   ncross         = 15;
	int   ncross_samples = 33;
	int   sample_type    = 2;  /* float */
	int   sample_domain  = 1;  /* spectra */
	int   sample_format  = 2;  /* sideband separated */
	int   scale_factor   = 1;
	int   nint           = 80;
	float bandwidth      = 500.0;
	float tpsw           = 6.25e-3;
	float tint           = 6.18e-3;
	int   nintegrations  = 1;

	/* Construct an object, serialize, and deserialize it */
	format.setId(id); 
	format.setBand(band); 
	format.setNauto(nauto); 
	format.setNautoSamples(nauto_samples); 
	format.setNcross(ncross); 
	format.setNcrossSamples(ncross_samples);
	format.setSampleType(sample_type); 
	format.setSampleDomain(sample_domain);
	format.setSampleFormat(sample_format); 
	format.setScaleFactor(scale_factor);
	format.setNint(nint);
	format.setBandwidth(bandwidth);
	format.setTint(tint);
	format.setTpsw(tpsw);
	format.setNintegrations(nintegrations);

	return format;
}

vector<CobraCorrelationInfo> CobraCorrelationInfoSim(
		CobraDataFormat &format)
{
	vector<CobraCorrelationInfo> info;
	
	int nauto = format.getNauto();
	int ncross = format.getNcross();

	// The following assumes ncross = nauto*(nauto - 1)
	
	info.resize(nauto+ncross);
	for (int i = 0; i < nauto; i++) {
		info[i].setInput(0, i+1);
		info[i].setInput(1, i+1);
		info[i].setOffset(0);
	}
	int n = nauto;
	for (int i = 0; i < nauto; i++) {
		for (int j = i+1; j < nauto; j++) {
			info[n].setInput(0, i+1);
			info[n].setInput(1, j+1);
			info[n].setOffset(0);
			n++;
		}
	}
	return info;
}

CobraIntegrationData CobraIntegrationDataSim(
		CobraDataFormat &format, vector<CobraCorrelationInfo> &info)
{
	CobraIntegrationData integ;
	int nauto = format.getNauto();
	int ncross = format.getNcross();
	
	for (int i = 0; i < nauto; i++) {
		CobraAutoSpectra spec = CobraAutoSpectraSim(format, info[i]);
		integ.setAutoSpectra(spec, i);
	}
	int n = nauto;
	for (int i = 0; i < ncross; i++) {
		CobraCrossSpectra spec = CobraCrossSpectraSim(format, info[n]);
		integ.setCrossSpectra(spec, i);
		n++;
	}
	// Need to set this to something valid when data is sent.
	integ.setTimestamp(1234.5678);
	
	return integ;
}

CobraAutoSpectra CobraAutoSpectraSim(
		CobraDataFormat &format, CobraCorrelationInfo &info)
{
	CobraAutoSpectra spec;

	int band = format.getBand();
	int an   = info.getInput();

	spec.setStatus(0);
	spec.setMode(0);
	spec.setNint(format.getNint());
	vector<float> data = CobraAutoSpectraVectorSim(band, an);
	spec.setData(data);

	return spec;
}


CobraCrossSpectra CobraCrossSpectraSim(
		CobraDataFormat &format, CobraCorrelationInfo &info)
{
	CobraCrossSpectra spec;

	int band = format.getBand();
	int an[2];
	an[0] = info.getInput(0);
	an[1] = info.getInput(1);

	spec.setStatus(0);
	spec.setMode(0);
	spec.setNint(format.getNint());
	for (int j = 0; j < 2; j++) {
	vector<complex<float> > data = 
		CobraCrossSpectraVectorSim(band, an[0], an[1], j);
		spec.setData(data, j);
	}
	return spec;
}

	}; // lib
  }; // correlator
}; // carma
