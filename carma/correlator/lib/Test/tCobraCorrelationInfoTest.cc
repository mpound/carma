#include <iostream>
#include <vector>
#include <netinet/in.h>

#include "carma/correlator/lib/CobraCorrelationInfo.h"
#include "carma/correlator/lib/CobraDataSim.h"

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

void test1();
void test2();
void createInfo(vector<CobraCorrelationInfo> &info);
void printInfo(vector<CobraCorrelationInfo> &info);

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaCorrelationInfoTest
//
int Program::main() 
{
//	vector<CobraCorrelationInfo> info;
//	createInfo(info);
//	printInfo(info);

	test1();
	test2();

	return 0;
}

void createInfo(vector<CobraCorrelationInfo> &info)
{
	/* Cobra wideband data 6 + 15 = 21 correlations */
	info.resize(21);
	for (int i = 0; i < 6; i++) {
		info[i].setInput(0, i+1);
		info[i].setInput(1, i+1);
		info[i].setOffset(0);
	}
	int n = 6;
	for (int i = 0; i < 6; i++) {
		for (int j = i+1; j < 6; j++) {
			info[n].setInput(0, i+1);
			info[n].setInput(1, j+1);
			info[n].setOffset(0);
			n++;
		}
	}
}

void printInfo(vector<CobraCorrelationInfo> &info)
{
	for (unsigned int i = 0; i < info.size(); i++) {
		cout << "Baseline " << info[i].getInput(0) 
			 << "-" << info[i].getInput(1)
			 << "   offset " << info[i].getOffset() << endl;
	}
}


/* Object serialization/deserialization */
void test1()
{
	vector<CobraCorrelationInfo> info;
	vector<CobraCorrelationInfo> recovered;

	/* Serialized object (one per object) */
	vector<vector <char> > byteArray;
	
//	createInfo(info);
	CobraDataFormat format = CobraDataFormatSim(1);
	info = CobraCorrelationInfoSim(format);


	
	/* Collect all the serialized byteArray pointers */
	int size = info.size();
	byteArray.resize(size);
	recovered.resize(size);

	cout << "Serialized size should be " << info[0].size() << endl;
	
	/* serialize */
	for (int i = 0; i < size; i++) {
		info[i].serialIntoByteVec( byteArray[i] );
	}
	
	cout << "Serialized size is " << byteArray[0].size() << endl;

	/* deserialize */
	int offset = 0;
	for (int i = 0; i < size; i++) {
		offset = 0;
		recovered[i].deserial(byteArray[i]);
	}
	
	/* Check */
	for (int i = 0; i < size; i++) {
		if (recovered[i] != info[i]) {
			cout << 
				"TEST 1 Error: serialization/deserialization failed!" 
				<< endl;
		} else {
			cout << 
				"TEST 1: Serialization/deserialization success!" 
				<< endl;
		}
	}
}

/* C-generated 'raw object' followed by deserialization */
void test2()
{
	vector<CobraCorrelationInfo> info;
	vector<CobraCorrelationInfo> recovered;

	/* Array of serialized objects */
	vector < vector<char> > byteArray;
	
	createInfo(info);
	int info_size = info.size();
	recovered.resize(info_size);
	byteArray.resize(info_size);
	
	/* COBRA Wideband 500ms integrations */
	int raw[21*4];
	int net[21*4];
	for (int i = 0; i < info_size; i++) {
		if (info[i].isAuto()) {
			raw[i*4] = 0;
		} else {
			raw[i*4] = 1;
		}
		raw[i*4+1]   = info[i].getInput(0);
		raw[i*4+2] = info[i].getInput(1);
		raw[i*4+3] = info[i].getOffset();
	}

	/* convert to network order */
	for (int i = 0; i < 21*4; i++) {
		net[i] = htonl(raw[i]);
	}
	
	/* construct byte arrays to hold the data */
	vector< vector<char> > bytes;
	bytes.resize(info_size);
	char *p = (char *)net;
	for (int i = 0; i < info_size; i++) {
		bytes[i].resize(4*4 + 1);

		/* Copy the raw data into the byte array */
		bytes[i].at(0) = 0; /* big-endian */
		memcpy(&(bytes[i].at(1)), p, 4*4);
		p += 4*4;

		/* initialize the byteArray pointer */
		byteArray[i] = bytes[i];
	}

	for (int i = 0; i < info_size; i++) {
		recovered[i].deserial(byteArray[i]);
	}
	
	/* Check */
	for (int i = 0; i < info_size; i++) { 
		if (recovered[i] != info[i]) {
			cout << 
				"TEST 2 Error: serialization/deserialization failed!" 
				<< endl;
		} else {
			cout << 
				"TEST 2: Serialization/deserialization success!" 
				<< endl;
		}
	}
//	printFormat(info);
//	printFormat(recovered);
	
}


