
// $Id: correlatorIPQdump.cc,v 1.3 2012/12/06 00:24:48 abeard Exp $

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/util/IPQbasicTypeBuffer.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <fstream>
#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;
using namespace carma::pipeline;
using namespace carma::util;
using namespace carma::correlator::lib;

namespace {

void
printBasicInfo( const CorrelatorData & cd ) 
{
    const BandVector & bands = cd.getBands();
    cout << endl << "frame " << Time::computeFrame( cd.getHeader().getMJD() ) 
        << " (Band, BlCount): ";
    BOOST_FOREACH( const CorrelatorBand & band, bands ) {
        const BaselineVector & baselines = band.getBaselines();
        cout << "(" << band.getBandNumber() << "," << baselines.size() << ") ";
    }
    cout << endl;

} // printBasicInfo

} // namespace < unnamed >

/**
 *  @description
 *  Reads from a correlator IPQ and dumps data to a file and standard out.
 *
 *  @usage Usage: correlatorIPQdump <keywords>
 *
 *  @key mode @mandatory s 
 *       Mode of operation: Sl, Wb, C3g23 or C3g8 (not case sensitive). 
 *  @key s @mandatory string   
 *       Output file to save data to.
 *  @key ipqname @noDefault s 
 *       Output ipq filename. If not specified use a default name\n\t
 *       of "catch" + Mode + "DataIPQ".  This paremeter must match\n\t
 *       catchData (the defaults do).
 *  @key ipqmaxsize @noDefault i 
 *       Max bytes of IPQ element. If not specified use an internal default\n\t
 *       based on the mode.  This parameter must match catchData (the\n\t
 *       defaults do).
 *
 *  @logger DEFAULT_FACILITY carma.pipeline.correlatorIPQdump
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.3 $, $Date: 2012/12/06 00:24:48 $
 */
int Program::main() {

  const PipelineType pt( stringToPipelineType( getStringParameter( "mode" ) ) );

  string ipqFilename;
  if ( parameterWasSpecified( "ipqname" ) ) 
      ipqFilename = getStringParameter( "ipqname" );
  else
      ipqFilename = getDefaultCatchDataIpqName( pt );

  int ipqMaxElementSize;
  if ( parameterWasSpecified( "ipqmaxsize" ) ) 
      ipqMaxElementSize = getIntParameter( "ipqmaxsize" );
  else
      ipqMaxElementSize = getDefaultCatchDataIpqElementBytes( pt );

  string outFileName = Program::getProgram().getStringParameter( "s" );

  size_t bytesWritten = 0;
  const size_t bytesPerFile = 500000000; // 500 MB
  size_t filesWritten = 0;

  char* byteArray = new char[ipqMaxElementSize];
  vector<char> data;
  data.resize(ipqMaxElementSize);
  try {
    carma::util::IPQbasicTypeBuffer* ipq_ = 
      new carma::util::IPQbasicTypeBuffer(byteArray,
					   ipqMaxElementSize,
					   ipqFilename);
    CorrelatorData* cd = new CorrelatorData();

    int sizeInBytes = 0;
    cout << "IPQ Element size is " << ipqMaxElementSize << " bytes." << endl; 

    while (1) {

      ipq_->read();
      memcpy(&data[0], byteArray, ipqMaxElementSize);
      cd->deserial(data);
      
      bool writeRecordToFile = false;

      if ( cd->getTotalSerialBytes() != sizeInBytes ) {
          sizeInBytes = cd->getTotalSerialBytes();
          cout << endl << "CD " << cd->getTotalSerialBytes() << " bytes ";
          writeRecordToFile = true;
          printBasicInfo( *cd );
      } else {
         cerr << "." << flush;
          printBasicInfo( *cd );
      }

	  if ( !cd->baselineCountsPhysicallyValid( ) ) {
          cout << endl << "!";
          printBasicInfo( *cd );
          writeRecordToFile = true;
	  }

      if ( writeRecordToFile ) {
          carma::util::ByteBuffer dataByteBuffer;
          cd->serialIntoByteBuffer( dataByteBuffer );
          const size_t size = dataByteBuffer.size();
          ofstream fp; 
          fp.open( outFileName.c_str(), ios::out | ios::app | ios::binary );
          fp << size;
          fp.write( dataByteBuffer.get(), size );
          fp.close();
          bytesWritten += size;
       }

       if ( bytesWritten > bytesPerFile ) {
          bytesWritten = 0;
          ++filesWritten;
          ostringstream oss;
          oss << Program::getProgram().getStringParameter( "s" ) 
              << "." << filesWritten;
          outFileName = oss.str();
      }

    }
  } catch (ErrorException& err) {
    cerr << err << endl;
  }
  return 0;
}
