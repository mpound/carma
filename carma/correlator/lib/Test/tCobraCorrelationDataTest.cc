#include <iostream>
#include <vector>
#include <netinet/in.h>

#include "carma/correlator/lib/CobraCorrelationData.h"

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

void autotest1();
void autotest2();
// Dave, this is considered confusing according to Eckel. He recommends
// that objects which will be modified in methods, go in as pointers,
// otherwise as const &.  -- rick
void createAuto(CobraAutoSpectra &spec);
void printAuto(CobraAutoSpectra &spec);

void crosstest1();
void crosstest2();
void createCross(CobraCrossSpectra &cross);
void printCross(CobraCrossSpectra &cross);

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaCorrelationDataTest
//
int Program::main() 
{
//	CobraCorrelationData base;
//	CobraAutoSpectra  aspec;
//	CobraCrossSpectra cspec;

	autotest1();
	autotest2();
	crosstest1();
	crosstest2();
	
	return 0;
}

void createAuto(CobraAutoSpectra &spec)
{
	spec.setStatus(1);
	spec.setMode(2);
	spec.setNint(80);
	vector<float> data(33);
	for (int i = 0; i < 33; i++) {
		data.at(i) = i+1;
	}
	spec.setData(data);
}


void printAuto(CobraAutoSpectra &spec)
{
	cout << "Auto: \nStatus " << spec.getStatus()
		 << "\nMode   " << spec.getMode()
		 << "\nNint   " << spec.getNint()
		 << "\ndata   ";
	vector<float> data = spec.getData();
	int size = data.size();
	for (int i = 0; i < size; i++) {
		cout << data[i] << ", ";
	}
	cout << endl;
}

void createCross(CobraCrossSpectra &spec)
{
	spec.setStatus(1);
	spec.setMode(2);
	spec.setNint(80);
	vector<complex<float> > data(33);
	for (int j = 0; j < 2; j++) {
		for (int i = 0; i < 33; i++) {
			data.at(i) = complex<float>(i+1+j*100,0);
		}
		spec.setData(data, j);
	}
}

void printCross(CobraCrossSpectra &spec)
{
	cout << "Cross: \nStatus " << spec.getStatus()
		 << "\nMode   " << spec.getMode()
		 << "\nNint   " << spec.getNint()
		 << "\nlsb data   ";
	vector<complex<float> > data = spec.getData(0);
	int size = data.size();
	for (int i = 0; i < size; i++) {
		cout << data[i] << " ";
	}
	cout << endl;
	cout << "\nusb data   ";
	data = spec.getData(1);
	size = data.size();
	for (int i = 0; i < size; i++) {
		cout << data[i] << " ";
	}
	cout << endl;
}


/* Object serialization/deserialization */
void autotest1()
{
	CobraAutoSpectra spec;
	CobraAutoSpectra recovered;

	createAuto(spec);

	cout << "Serialized size should be " << spec.size() << endl;
	
	/* serialize */
	vector< char > byteArray;
	spec.serialIntoByteVec( byteArray );
	
	cout << "Serialized size is " << byteArray.size() << endl;

	/* deserialize */
	recovered.setNumChannels(spec.getNumChannels());
	//recovered.deserialize(byteArray, &offset);
	recovered.deserial(byteArray);
	
	/* Check */
	if (recovered != spec) {
		cout << 
			"AUTOTEST 1 Error: auto serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"AUTOTEST 1: auto serialization/deserialization success!" 
			<< endl;
	}
	//printAuto(spec);
	//printAuto(recovered);
}


/* C-generated 'raw object' followed by deserialization */
void autotest2()
{
	CobraAutoSpectra spec;
	CobraAutoSpectra recovered;

	/* Serialized object */
	vector <char> byteArray;
	
	createAuto(spec);

	/* COBRA Wideband Autocorrelation
	 * status, mode, nint, data[33]
	 */
	int raw[36];
	int net[36];
	
	raw[0] = spec.getStatus();
	raw[1] = spec.getMode();
	raw[2] = spec.getNint();
	vector <float> data = spec.getData();
	float *f = (float *)&raw[3];
	for (int i = 0; i < 33; i++) {
		*f++ = data[i];
	}

	/* convert to network order */
	for (int i = 0; i < 36; i++) {
		net[i] = htonl(raw[i]);
	}
	
	/* construct a byte array to hold the data */
	vector<char> bytes;
	bytes.resize(36*4+1);
	bytes[0] = 0; /* big-endian */
	
	/* copy the raw data */
	memcpy(&bytes[1], net, 36*4);

	/* initialize the byteArray pointer */
	byteArray = bytes;

	/* deserialize */
	recovered.setNumChannels(spec.getNumChannels());
	recovered.deserial(byteArray);
	
	/* Check */
	if (recovered != spec) {
		cout << 
			"AUTOTEST 2 Error: auto serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"AUTOTEST 2: auto serialization/deserialization success!" 
			<< endl;
	}
//	printAuto(spec);
//	printAuto(recovered);
	
}


void crosstest1()
{
	CobraCrossSpectra spec;
	CobraCrossSpectra recovered;

	createCross(spec);
	
	cout << "Serialized size should be " << spec.size() << endl;
	
	/* serialize */
	vector < char > byteArray;
	spec.serialIntoByteVec( byteArray );
	
	cout << "Serialized size is " << byteArray.size() << endl;

	/* deserialize */
	recovered.setNumChannels(spec.getNumChannels());
	recovered.deserial(byteArray);
	
	/* Check */
	if (recovered != spec) {
		cout << 
			"CROSSTEST 1 Error: cross serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"CROSSTEST 1: cross serialization/deserialization success!" 
			<< endl;
	}
//	printCross(spec);
//	printCross(recovered);
}


/* C-generated 'raw object' followed by deserialization */
void crosstest2()
{
	CobraCrossSpectra spec;
	CobraCrossSpectra recovered;

	/* Serialized object */
	vector <char> byteArray;
	
	createCross(spec);

	/* COBRA Wideband Crosscorrelation
	 * status, mode, nint, data[2][33] (complex)
	 *  3 + 2*2*33 = 198 ints
	 */
	int raw[198];
	int net[198];
	
	raw[0] = spec.getStatus();
	raw[1] = spec.getMode();
	raw[2] = spec.getNint();
	float *f = (float *)&raw[3];
	vector <complex<float> > data;
	for (int j = 0; j < 2; j++) {
		data = spec.getData(j);
		for (int i = 0; i < 33; i++) {
			*f++ = data[i].real();
			*f++ = data[i].imag();
		}
	}
	
	/* convert to network order */
	for (int i = 0; i < 198; i++) {
		net[i] = htonl(raw[i]);
	}
	
	/* construct a byte array to hold the data */
	vector<char> bytes;
	bytes.resize(198*4+1);
	bytes[0] = 0; /* big-endian */
	memcpy(&bytes[1], net, 198*4);

	/* initialize the byteArray pointer */
	byteArray = bytes;

	/* deserialize */
	recovered.setNumChannels(spec.getNumChannels());
	recovered.deserial(byteArray);
	
	/* Check */
	if (recovered != spec) {
		cout << 
			"CROSSTEST 2 Error: cross serialization/deserialization failed!" 
			<< endl;
	} else {
		cout << 
			"CROSSTEST 2: cross serialization/deserialization success!" 
			<< endl;
	}
//	printCross(spec);
//	printCross(recovered);
	
}



