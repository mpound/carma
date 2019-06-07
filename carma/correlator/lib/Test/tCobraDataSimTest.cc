#include <iostream>
#include <vector>
#include <netinet/in.h>

#include "carma/correlator/lib/CobraDataSim.h"

#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

// Test to see whether erase() end is < or <=
/* rh function not used
static void vector_test()
{
	vector <char> b(10);
	cout << "Vector size is " << b.size() << endl; 
	cout << "Vector contents : ";
	for (unsigned int i = 0; i < b.size(); i++) {
		b[i] = (char)(i+1);
		cout << (int)b[i] << " ";
	}
	cout << endl;
	vector<char>::iterator bstart = b.begin() + 1;

	// The end element does NOT get erased
	vector<char>::iterator bend   = b.begin() + 3;
	b.erase(bstart,bend);
	cout << "Vector size is " << b.size() << endl; 
	cout << "Vector contents : ";
	for (unsigned int i = 0; i < b.size(); i++) {
		cout << (int)b[i] << " ";
	}
	cout << endl;
}
*/

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaDataSimTest
//
int Program::main() 
{
	CobraDataFormat format;
	CobraDataFormat format_rec;
	vector<CobraCorrelationInfo> info;
	vector<CobraCorrelationInfo> info_rec;
	CobraIntegrationData integ;
	CobraIntegrationData integ_rec;

	/* Create band 1 */
	cout << "Create the objects " << endl;
	cout << "Format for band 1" << endl;
	format = CobraDataFormatSim(1);
	cout << "Correlation info" << endl;
	info   = CobraCorrelationInfoSim(format);
	cout << "Integration data" << endl;
	integ  = CobraIntegrationDataSim(format, info);

	int format_size    = format.size();
	int infoarray_size = (6+15)*info[0].size();
	int integ_size     = integ.size();
	int total_size     = format_size + infoarray_size + integ_size;
	
	cout << "Format size = " << format_size
		 << "\nInfo size   = " << infoarray_size
		 << "\nInteg size  = " << integ_size
		 << "\nTotal size  = " << total_size 
		 << endl;
	
	cout << "Serializing ..." << endl;
	/* Serialized object */
	vector <char> bytes;
	
	/* serialize format */
	cout << "format ";
	vector< char > byteArray;
	format.serialIntoByteVec( byteArray );
	int size = byteArray.size();
	bytes.resize(size);
	int bytes_size = size;
	cout << "(size " << size << ") ";
	memcpy(&bytes[0], &byteArray[0], size);
	
	// serialize the info array
	// blarg! damn serializable interface
	cout << "info ";
	int start;
	int info_size = info.size();
	for (int i = 0; i < info_size; i++) {
		vector< char > localByteArray;
		info[i].serialIntoByteVec( localByteArray );
		start       = bytes_size;
		size        = localByteArray.size();
		cout << "(info[" << i << "], size " << size << ") ";
		bytes_size += size;
		bytes.resize(bytes_size);
		memcpy(&bytes[start], &localByteArray[0], size);
	}
	
	// Now the integration data
	cout << "integ ...";
	vector< char > byteArray2;
	integ.serialIntoByteVec( byteArray2 );
	start       = bytes_size;
	size        = byteArray2.size();
	cout << "(size " << size << ") ";
	bytes_size += size;
	bytes.resize(bytes_size);
	memcpy(&bytes[start], &byteArray2[0], size);
	
	cout << "done" << endl;

	cout << "Serialized object size is " << bytes.size() << endl;
	
	cout << "Deserializing ..." << endl;
	/* deserialize */
	int offset = 0;
	const vector<char>& byteArray3 = bytes;

	// Initialize the objects that need it before
	// deserialization
	integ_size     = integ_rec.size();
	cout << "Integ size prior to init " << integ_size << endl;
	integ_rec.setNauto(integ.getNauto());
	integ_rec.setNautoSamples(integ.getNautoSamples());
	integ_rec.setNcross(integ.getNcross());
	integ_rec.setNcrossSamples(integ.getNcrossSamples());
	integ_size     = integ_rec.size();
	cout << "Integ size prior to deserialize " << integ_size << endl;

	/* format */
	start = offset;
	format_rec.deserial(byteArray3);

	/* Need to consume the 'size' of format plus endian flag */
	size = format_rec.size() + 1;
	vector<char>::iterator vstart = bytes.begin();
	vector<char>::iterator vend   = bytes.begin() + size;
//	cout << "erasing " << size << " bytes" << endl;
	bytes.erase(vstart, vend);
	const vector<char>& byteArray4 = bytes;
	
	/* info */
	start = offset;
	info_rec.resize(info_size);
	for (int i = 0; i < info_size; i++) {
		info_rec[i].deserial(byteArray4);

		/* Need to consume the 'size' of info, plus
		 * the endian flag (for this test, all the objects
		 * have inserted the required flag)
		 */
		size   = info_rec[i].size() + 1;
		vstart = bytes.begin();
		vend   = bytes.begin() + size;
	//	cout << "erasing " << size << " bytes" << endl;
		bytes.erase(vstart, vend);
		// const vector<char>& byteArray4 = bytes;
	}

	/* integration data */
	integ_rec.deserial(byteArray4);

	/* Check */
	if (format_rec != format) {
		cout << "FORMAT Error: serialization/deserialization failed!" << endl;
	} else {
		cout << "FORMAT: Serialization/deserialization success!" << endl;
	}
	for (int i = 0; i < info_size; i++) {
		if (info_rec[i] != info[i]) {
			cout << "INFO Error: serialization/deserialization failed!" << endl;
		} else {
			cout << "INFO: Serialization/deserialization success!" << endl;
		}
	}
	if (integ_rec != integ) {
		cout << "INTEG Error: serialization/deserialization failed!" << endl;
	} else {
		cout << "INTEG: Serialization/deserialization success!" << endl;
	}
	
	return 0;
}
	
