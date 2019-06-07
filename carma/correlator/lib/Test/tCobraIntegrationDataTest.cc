#include <iostream>
#include <vector>
#include <cmath>
#include <netinet/in.h>

#include "carma/correlator/lib/CobraCorrelationData.h"
#include "carma/correlator/lib/CobraIntegrationData.h"
#include "carma/correlator/lib/CobraDataSim.h"

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

void test1();
void test2();
void printIntegrationData(CobraIntegrationData &integ);

#define NAUTO    6
#define NCROSS   15
#define NSAMPLES 33
#define RAWSIZE (8 + NAUTO*(3+NSAMPLES) + NCROSS*(3+2*2*NSAMPLES))

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaIntegrationDataTest
//
int Program::main() 
{
	test1();
	test2();
	
	return 0;
}

/* Object serialization/deserialization */
void test1()
{
	CobraIntegrationData integ;
	CobraIntegrationData recovered;

	CobraDataFormat format = CobraDataFormatSim(1);
	vector<CobraCorrelationInfo> info  = CobraCorrelationInfoSim(format);
	integ  = CobraIntegrationDataSim(format, info);

	cout << "Serialized size should be " << integ.size() << endl;
	
	/* serialize */
	vector< char > byteArray;
	integ.serialIntoByteVec( byteArray );
	
	cout << "Serialized size is " << byteArray.size() << endl;
	
	/* deserialize */
	recovered.setNauto(integ.getNauto());
	recovered.setNautoSamples(integ.getNautoSamples());
	recovered.setNcross(integ.getNcross());
	recovered.setNcrossSamples(integ.getNcrossSamples());
	recovered.deserial(byteArray);
	
	/* Check */
	if (recovered != integ) {
		cout << 
			"TEST 1 Error: integration serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"TEST 1: integration serialization/deserialization success!" 
			<< endl;
	}
//	printIntegrationData(integ);
//	printIntegrationData(recovered);
}


/* C-generated 'raw object' followed by deserialization */
void test2()
{
	CobraIntegrationData integ;
	CobraIntegrationData recovered;

//	createIntegrationData(integ);
	CobraDataFormat format = CobraDataFormatSim(1);
	vector<CobraCorrelationInfo> info  = CobraCorrelationInfoSim(format);
	integ  = CobraIntegrationDataSim(format, info);

	/* COBRA Wideband Integration
	 * timestamp
	 * 6  autos: status, mode, nint, data[33]
	 * 15 cross: status, mode, nint, data[2][33] (complex)
	 *  8 + 6 x (3 + 33) + 15 x (3 + 2*2*33) = 2249
	 */
	int raw[RAWSIZE];
	int net[RAWSIZE];

	int ts[2];
	*(double *)ts = integ.getTimestamp(); 
	raw[0] = ts[1]; /* Flip the 8-bytes in the double */
	raw[1] = ts[0]; 
	int i = 2;
	for (int j = 0; j < NAUTO; j++) {
		CobraAutoSpectra spec = integ.getAutoSpectra(j);
		raw[i++] = spec.getStatus();
		raw[i++] = spec.getMode();
		raw[i++] = spec.getNint();
		vector <float> data = spec.getData();
		float *f = (float *)&raw[i];
		for (int k = 0; k < NSAMPLES; k++) {
			*f++ = data[k];
			i++;
		}
	}
	for (int j = 0; j < NCROSS; j++) {
		CobraCrossSpectra spec = integ.getCrossSpectra(j);
		raw[i++] = spec.getStatus();
		raw[i++] = spec.getMode();
		raw[i++] = spec.getNint();
		for (int k = 0; k < 2; k++) {
			vector<complex <float> > data = spec.getData(k);
			float *f = (float *)&raw[i];
			for (int m = 0; m < NSAMPLES; m++) {
				*f++ = data[m].real();
				*f++ = data[m].imag();
				i+=2;
			}
		}
	}
	
	/* convert to network order */
	for (int i = 0; i < RAWSIZE; i++) {
		net[i] = htonl(raw[i]);
	}
	
	/* construct a byte array to hold the data */
	vector<char> bytes;
	bytes.resize(RAWSIZE*4+1);
	bytes[0] = 0; /* big-endian */
	memcpy(&bytes[1], net, RAWSIZE*4);

	/* initialize the byteArray pointer */
	const vector<char>& byteArray = bytes;

	/* deserialize */
	recovered.setNauto(integ.getNauto());
	recovered.setNautoSamples(integ.getNautoSamples());
	recovered.setNcross(integ.getNcross());
	recovered.setNcrossSamples(integ.getNcrossSamples());
	recovered.deserial(byteArray);
		
	/* Check */
	if (recovered != integ) {
		cout << 
			"TEST 2 Error: integration serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"TEST 2: integration serialization/deserialization success!" 
			<< endl;
	}
//	printIntegrationData(integ);
//	printIntegrationData(recovered);

}

void printIntegrationData(CobraIntegrationData &integ)
{
	cout << "\nTimestamp     : " << integ.getTimestamp()
		 << "\nNauto         : " << integ.getNauto()
		 << "\nNautoSamples  : " << integ.getNautoSamples()
		 << "\nNcross        : " << integ.getNcross()
		 << "\nNcrossSamples : " << integ.getNcrossSamples()
		 << endl; 
	
	int nauto = integ.getNauto();
	for (int i = 0; i < nauto; i++) {
		CobraAutoSpectra spec = integ.getAutoSpectra(i);
		cout << "Auto " << i << ":"
			 << "\nStatus " << spec.getStatus()
			 << "\nMode   " << spec.getMode()
			 << "\nNint   " << spec.getNint()
			 << "\ndata   ";
		vector<float> data = spec.getData();
		int size = data.size();
		for (int j = 0; j < size; j++) {
			cout << data[j] << " ";
		}
		cout << endl;
	}
	// int ncross = integ.getNcross();
	for (int i = 0; i < nauto; i++) {
		CobraCrossSpectra spec = integ.getCrossSpectra(i);
		cout << "Cross " << i << ":" 
		 << "\nStatus " << spec.getStatus()
		 << "\nMode   " << spec.getMode()
		 << "\nNint   " << spec.getNint()
		 << "\nlsb data   ";
		vector<complex<float> > data = spec.getData(0);
		int size = data.size();
		for (int j = 0; j < size; j++) {
			cout << abs(data[j]) << " ";
		}
		cout << endl;
		cout << "usb data   ";
		data = spec.getData(1);
		size = data.size();
		for (int j = 0; j < size; j++) {
			cout << abs(data[j]) << " ";
		}
		cout << endl;
	}
	return;
}

