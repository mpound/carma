#include <iostream>
#include <vector>
#include <netinet/in.h>

#include "carma/correlator/lib/CobraDataFormat.h"
#include "carma/correlator/lib/CobraDataSim.h"

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

void test1();
void test2();
void createFormat(CobraDataFormat &format);
void printFormat(CobraDataFormat &format);

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaDataFormatTest
//
int Program::main() 
{
	cout << "-----------------------------------------------" << endl;
	test1();
	cout << "-----------------------------------------------" << endl;
	test2();
	cout << "-----------------------------------------------" << endl;

	return 0;
}
	
void createFormat(CobraDataFormat &format)
{
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
}

/* Object serialization/deserialization */
void test1()
{
	CobraDataFormat format;
	CobraDataFormat recovered;
	int format_size;
	int bytearray_size;
	
//	createFormat(format);
	format = CobraDataFormatSim(1);

	format_size = format.size();
	cout << "Serialized size should be " << format.size() << endl;
	
	/* serialize */
	vector< char > byteArray;
	format.serialIntoByteVec( byteArray );
	
	bytearray_size = byteArray.size();
	cout << "Serialized size is " << byteArray.size() << endl;

	cout << "byteArray[0] is " << (int)(byteArray[0]) << endl;
	
	if (bytearray_size != format_size + 1) {
		cout << "WARNING: incorrect size detected!" << endl;
	}
	
	/* deserialize */
	recovered.deserial(byteArray);

	/* Check */
	if (recovered != format) {
		cout << "TEST 1 Error: serialization/deserialization failed!" << endl;
	} else {
		cout << "TEST 1: Serialization/deserialization success!" << endl;
	}
/*	
	cout << "\nTest 1: input object " << endl;
	printFormat(format);
	cout << "\nTest 1: recovered object " << endl;
	printFormat(recovered);
*/
}

/* C-generated 'raw object' followed by deserialization */
void test2()
{
	CobraDataFormat format;
	CobraDataFormat recovered;

	createFormat(format);
	
	/* COBRA Wideband 500ms integrations */
	float bandwidth      = format.getBandwidth();
	float tpsw           = format.getTpsw();
	float tint           = format.getTint();
	int *p;
	int raw[15];
	int net[15];
	raw[0] = format.getId();
	raw[1] = format.getBand();
	raw[2] = format.getNauto();
	raw[3] = format.getNautoSamples();
	raw[4] = format.getNcross();
	raw[5] = format.getNcrossSamples();
	raw[6] = format.getSampleType();
	raw[7] = format.getSampleDomain();
	raw[8] = format.getSampleFormat();
	raw[9] = format.getScaleFactor();
	raw[10] = format.getNint();
	p = (int *)&bandwidth;
	raw[11] = *p;
	p = (int *)&tint;
	raw[12] = *p;
	p = (int *)&tpsw;
	raw[13] = *p;
	raw[14] = format.getNintegrations();

	/* convert to network order */
	for (int i = 0; i < 15; i++) {
		net[i] = htonl(raw[i]);
	}
	
	/* construct a byte array to hold the data */
	vector<char> bytes;
	bytes.resize(15*4+1);

	/* UPDATE: Rick has modified his serialized format
	 * to include a byte-order flag.
	 */
	/* Put a byte flag at the beginning of the data */
	bytes[0] = 0; /* big-endian */

	/* Copy the raw data into the byte array */
	memcpy(&bytes[1], net, 15*4);

	/* initialize the byteArray pointer */
	const vector<char>& byteArray = bytes;

	/* deserialize */
	recovered.deserial(byteArray);

	/* Check */
	if (recovered != format) {
		cout << "TEST 2 Error: serialization/deserialization failed!" << endl;
	} else {
		cout << "TEST 2: Serialization/deserialization success!" << endl;
	}
/*
	cout << "\nTest 2: input object " << endl;
	printFormat(format);
	cout << "\nTest 2: recovered object " << endl;
	printFormat(recovered);
*/	
}

void printFormat(CobraDataFormat &format)
{
	cout << "\nid             " << hex << format.getId() << dec 
		 << "\nband           " << format.getBand()
		 << "\nnauto          " << format.getNauto()
		 << "\nnauto_samples  " << format.getNautoSamples()
		 << "\nncross         " << format.getNcross() 
		 << "\nncross_samples " << format.getNcrossSamples()
		 << "\nsample_type    " << format.getSampleType() 
		 << "\nsample_domain  " << format.getSampleDomain()
		 << "\nsample_format  " << format.getSampleFormat()
		 << "\nscale_factor   " << format.getScaleFactor()
		 << "\nnint           " << format.getNint()
		 << "\nbandwidth      " << format.getBandwidth()
		 << "\ntint           " << format.getTint()
		 << "\ntpsw           " << format.getTpsw()
		 << "\nnintegrations  " 
		 << format.getNintegrations()
		 << endl;

}


