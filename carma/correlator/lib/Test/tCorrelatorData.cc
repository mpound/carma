
// $Id: tCorrelatorData.cc,v 1.42 2011/05/04 18:15:36 abeard Exp $

// tCorrelatorData.cc

// test serialization and other operations on CorrelatorData instances.


#include "carma/correlator/lib/CorrelatorDataTestSZA.h"
#include "carma/correlator/lib/CorrelatorDataTestWb.h"
#include "carma/correlator/lib/CorrelatorDataTestSl.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/simpleStats.h"

#include <boost/foreach.hpp>
#include <netinet/in.h>
#include <sys/time.h>
#include <unistd.h>           // for usleep
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <complex>
#include <fstream>

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;


namespace {


long long
diffMicros( const struct ::timeval & lhs,
            const struct ::timeval & rhs )
{
    const long long lhsMicros =
        static_cast< long long >( lhs.tv_sec ) * 1000LL * 1000LL +
        static_cast< long long >( lhs.tv_usec );

    const long long rhsMicros =
        static_cast< long long >( rhs.tv_sec ) * 1000LL * 1000LL +
        static_cast< long long >( rhs.tv_usec );

    return (lhsMicros - rhsMicros);
}


struct IterTimingInfo {
    struct ::timeval beginTime;
    struct ::timeval endTime;
};


string
getReportString( const vector< IterTimingInfo > & iterTimingInfos )
{
    if ( iterTimingInfos.empty() )
        return "no iterations";
        
    double firstMillis = -1.0;
    vector< double > otherMillis;
    {
        otherMillis.reserve( iterTimingInfos.size() - 1 );
    
        const vector< IterTimingInfo >::const_iterator iBegin =
            iterTimingInfos.begin();
            
        const vector< IterTimingInfo >::const_iterator iEnd =
            iterTimingInfos.end();
            
        vector< IterTimingInfo >::const_iterator i = iBegin;
        
        for ( ; i != iEnd; ++i ) {
            const long long micros = diffMicros( i->endTime, i->beginTime );
            const double millis = (static_cast< double >( micros ) / 1000.0);

            if ( i == iBegin )
                firstMillis = millis;
            else
                otherMillis.push_back( millis );
        }
    }
    
    ostringstream oss;
    
    if ( otherMillis.empty() ) {
        oss << firstMillis << " ms";
    } else {
        oss << "first iteration " << firstMillis << " ms";
    
        double minValue, maxValue, medianValue, meanValue, stdDev;

        calcSimpleStatsInplace( otherMillis,
                                &minValue,
                                &maxValue,
                                &medianValue,
                                &meanValue,
                                &stdDev );

        oss << " and the other " << otherMillis.size() << " iterations were "
            << minValue << " ms min, "
            << maxValue << " ms max, "
            << medianValue << " ms median, "
            << meanValue << " ms mean, "
            << stdDev << " ms std dev.";
    }

    return oss.str();
}


string
getSummaryText( const CorrelatorData & cd )
{
    size_t baselineCount = 0;
    bool haveBaselineCount = false;
    bool matchedBaselineCounts = true;

    size_t numMatchedSbDataCounts = 0;

    size_t sbDataCount = 0;
    bool haveSbDataCount = false;
    bool matchedSbDataCounts = true;

    const vector< CorrelatorBand > & cdBands = cd.getBands();
    
    vector< CorrelatorBand >::const_iterator j = cdBands.begin();
    const vector< CorrelatorBand >::const_iterator jEnd = cdBands.end();
    
    for ( ; j != jEnd; ++j ) {
        const vector< CorrelatorBaseline > & jBls = j->getBaselines();
        
        if ( haveBaselineCount == false ) {
            baselineCount = jBls.size();
            haveBaselineCount = true;
            matchedBaselineCounts = true;
        } else if ( jBls.size() != baselineCount ) {
            matchedBaselineCounts = false;
            break;
        }
            
        vector< CorrelatorBaseline >::const_iterator k = jBls.begin();
        const vector< CorrelatorBaseline >::const_iterator kEnd = jBls.end();
            
        for ( ; k != kEnd; ++k ) {
            const SidebandVector & kSbs = k->getSidebands();

            BOOST_FOREACH( const CorrelatorSideband & sideband, kSbs ) {
                    
                if ( haveSbDataCount == false ) {
                    sbDataCount = sideband.getData().size();
                    haveSbDataCount = true;
                    matchedSbDataCounts = true;
                    numMatchedSbDataCounts = 1;
                } else if ( sideband.getData().size() != sbDataCount ) {
                    matchedSbDataCounts = false;
                    break;
                } else
                    ++numMatchedSbDataCounts;
            }
        }
    }

    ostringstream oss;
    
    oss << cdBands.size() << " bands ";
    
    if ( matchedBaselineCounts ) {
        oss << "each with " << baselineCount << " baselines ";
        
        if ( matchedSbDataCounts ) {
            oss << "and all " << numMatchedSbDataCounts
                << " total sidebands each has a data count of "
                << sbDataCount;
        } else
            oss << "but with varying sideband data counts";
    } else
        oss << "with varying baseline counts";
        
    return oss.str();
}


void
runIt( const int  itersPerTest,
       const bool verbose )
{
  vector< IterTimingInfo > iterTimingInfos;

  {
      CorrelatorDataTestWb cdwb;

      cout << "Wb correlator data has " << getSummaryText( cdwb ) << endl;

      {
          vector< char > mvwb;
          cout << "Starting Wb serialIntoByteVec..." << endl;
          iterTimingInfos.resize( itersPerTest );
          for ( int j = 0; j < itersPerTest; ++j ) {
              ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
    
              cdwb.serialIntoByteVec( mvwb );
    
              ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
          }
          cout << "  Finished Wb serialIntoByteVec: "
               << getReportString( iterTimingInfos )
               << " Wb size= " << (mvwb.size() * sizeof(char)) << endl;
      }

      {
          ByteBuffer bb;
          cout << "Starting Wb serialIntoByteBuffer..." << endl;
          iterTimingInfos.resize( itersPerTest );
          for ( int j = 0; j < itersPerTest; ++j ) {
              ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
    
              cdwb.serialIntoByteBuffer( bb );
    
              ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
          }
          cout << "  Finished Wb serialIntoByteBuffer: "
               << getReportString( iterTimingInfos )
               << " Wb size= " << (bb.size() * sizeof(char)) << endl;
      }
  }
    
  CorrelatorDataTestSl cd;

  cout << "Sl correlator data has " << getSummaryText( cd ) << endl;

  vector< char > mv;
  {
      cout << "Starting Sl serialIntoByteVec..." << endl;
      iterTimingInfos.resize( itersPerTest );
      for ( int j = 0; j < itersPerTest; ++j ) {
          ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );

          cd.serialIntoByteVec( mv );

          ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
      }
      cout << "  Finished Sl serialIntoByteVec: "
           << getReportString( iterTimingInfos )
           << " Sl size= " << (mv.size() * sizeof(char)) << endl;
  }
  
  {
      ByteBuffer bb;
      
      cout << "Starting Sl serialIntoByteBuffer..." << endl;
      iterTimingInfos.resize( itersPerTest );
      for ( int j = 0; j < itersPerTest; ++j ) {
          ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );

          cd.serialIntoByteBuffer( bb );

          ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
      }
      cout << "  Finished Sl serialIntoByteBuffer: "
           << getReportString( iterTimingInfos )
           << " Sl size= " << (bb.size() * sizeof(char)) << endl;
  }
  
  for (int id = 0; id < 5; ++id)
    printf("serialized data: mv[%d]= 0x%2.2x  mv[end-%d]= 0x%2.2x\n",
           id, mv[id], id, mv[mv.size()-1-id]);
  // reset values

  
  // create new objects
  CorrelatorData cd2;
  
  // loop used to check memory leaks by setting 1 to a very large number
  // and watching 'top'
  cout << "Starting Sl deserialization..." << endl;
  iterTimingInfos.resize( itersPerTest );
  for ( int j = 0; j < itersPerTest; ++j ) {
    ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );

    cd2.deserial(mv);

    ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
  }
  cout << "  Finished Sl deserialization: "
       << getReportString( iterTimingInfos ) << endl;

  {
   cout << "Starting Sl getTotalSerialBytes..." << endl;
   iterTimingInfos.resize( itersPerTest );
   for ( int j = 0; j < itersPerTest; ++j ) {
     ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
 
     cd2.getTotalSerialBytes();
 
     ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
   }
   cout << "  Finished Sl getTotalSerialBytes: "
        << getReportString( iterTimingInfos ) << endl;
  }

  // time copy
  {
    cout << "Starting Sl copy by constructor..." << endl;
    iterTimingInfos.resize( itersPerTest );
    for ( int j = 0; j < itersPerTest; ++j ) {
      ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
  
      CorrelatorData cd3(cd2);
  
      ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
    }
    cout << "  Finished Sl copy by constructor: "
         << getReportString( iterTimingInfos ) << endl;
  }
  
  {
    cout << "Starting Sl copy by assignment..." << endl;
    iterTimingInfos.resize( itersPerTest );
    for ( int j = 0; j < itersPerTest; ++j ) {
      ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
  
      CorrelatorData cd3;
      cd3 = cd2;
  
      ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
    }
    cout << "  Finished Sl copy by assignment: "
         << getReportString( iterTimingInfos ) << endl;
  }
  
  {
    CorrelatorData cd3( cd2 );
    {
      cout << "Starting Sl addIn..." << endl;
      iterTimingInfos.resize( itersPerTest );
      for ( int j = 0; j < itersPerTest; ++j ) {
        ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
    
        cd3.addIn( cd2 );
    
        ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
      }
      cout << "  Finished Sl addIn: "
           << getReportString( iterTimingInfos ) << endl;
    }
    
    {
      cout << "Starting Sl normalize..." << endl;
      iterTimingInfos.resize( itersPerTest );
      for ( int j = 0; j < itersPerTest; ++j ) {
        CorrelatorData cd4( cd3 );
        
        ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
    
        cd4.normalize();
    
        ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
      }
      cout << "  Finished Sl normalize: "
           << getReportString( iterTimingInfos ) << endl;
    }
  }
  
  {
      cout << "Starting Sl removeBaseline..." << endl;
      iterTimingInfos.resize( itersPerTest );
      for ( int j = 0; j < itersPerTest; ++j ) {
        CorrelatorData cd3( cd2 );
        
        vector< CorrelatorBand > & cd3Bands = cd3.getBands();

        if ( cd3Bands.empty() )
            throw CARMA_ERROR( "cd3Bands is empty" );

        CorrelatorBand & cd3FirstBand = *(cd3Bands.begin());

        const vector< CorrelatorBaseline > & cd3FirstBandBls =
            cd3FirstBand.getBaselines();

        if ( cd3FirstBandBls.empty() )
            throw CARMA_ERROR( "cd3FirstBandBls is empty" );

        const CorrelatorBaseline & cd3FirstBandFirstBl =
            *(cd3FirstBandBls.begin());

        const int a1 = cd3FirstBandFirstBl.getInput1Number();
        const int a2 = cd3FirstBandFirstBl.getInput2Number();

        ::gettimeofday( &(iterTimingInfos[j].beginTime), 0 );
    
        cd3FirstBand.removeBaseline( a1, a2 );
    
        ::gettimeofday( &(iterTimingInfos[j].endTime), 0 );
      }
      cout << "  Finished Sl removeBaseline: "
           << getReportString( iterTimingInfos ) << endl;
  }
  
  if ( verbose )
    cout << "cd2: numberOfBands= " << cd2.getNumberOfBands() << endl;
  const vector<CorrelatorBand>& bands = cd2.getBands();

  for (unsigned int idx = 0; idx < bands.size(); ++idx) {
    if ( verbose )
      cout << "Band Number: " << bands[idx].getBandNumber() << endl;
    int numBa =  bands[idx].getNumberOfBaselines();
    if ( verbose ) {
      cout << "   Number of Baselines: " << numBa << endl;
      cout << "   Number of Inputs: " << bands[idx].getNumberOfInputs()
           << endl;
    }
    const vector<CorrelatorBaseline>& ba = bands[idx].getBaselines();
    for (int bidx = 0; bidx < numBa; ++bidx) {
      int numSb = ba[bidx].getNumberOfSidebands();
      if ( verbose )
        cout << "   Number of sidebands= " << numSb << endl;
        
      const SidebandVector & sb = ba[bidx].getSidebands();
      for (int sidx = 0; sidx < numSb; ++sidx) {
        if ( verbose ) {
          cout << "    number of chans= " << sb[sidx].getNumberOfChans()
               << endl;
        }
      }
    }
  }

  const CorrelatorHeader& head = cd2.getHeader();

  if ( verbose ) {
    cout << "head mjd= " << head.getMJD() << endl;
    cout << "head asmmjd= " << head.getAssembledMJD() << endl;
    cout << "head txmjd= " << head.getTransmissionMJD() << endl;
    cout << "head rxmjd= " << head.getReceivedMJD() << endl;
    cout << "head seq= " << head.getSequenceNumber() << endl;
  }
  
  // Test addIn method

  CorrelatorData cd1a;
  CorrelatorBand cb;
  cb.setBandNumber(0);
  cb.setNumberOfInputs(2);
  CorrelatorBaseline cbl;
  cbl.setInput1Number(0);
  cbl.setInput2Number(1);
  cb.addBaseline(cbl);

  cbl.setInput1Number(0);
  cbl.setInput2Number(0);
  cb.addBaseline(cbl);

  cbl.setInput1Number(1);
  cbl.setInput2Number(1);
  cb.addBaseline(cbl);
  // add band
  cd1a.addBand(cb);


  // 2nd band
  cb.setBandNumber(2);
  cb.setNumberOfInputs(2);

  cbl.setInput1Number(0);
  cbl.setInput2Number(1);
  // need to remove old baseline since we are re-using cb
  cb.removeBaseline(0, 1);
  cb.addBaseline(cbl);
  cbl.setInput1Number(0);
  cbl.setInput2Number(0);
  cb.removeBaseline(0, 0);
  cb.addBaseline(cbl);
  cbl.setInput1Number(1);
  cbl.setInput2Number(1);
  cb.removeBaseline(1, 1);
  cb.addBaseline(cbl);
  // add band
  cd1a.addBand(cb);

  // 2nd cd
  CorrelatorData cd1b;

  cb.setBandNumber(0);
  cb.setNumberOfInputs(2);
  cbl.setInput1Number(0);
  cbl.setInput2Number(1);
  cb.removeBaseline(0, 1);
  cb.addBaseline(cbl);
  cbl.setInput1Number(0);
  cbl.setInput2Number(0);
  cb.removeBaseline(0, 0);
  cb.addBaseline(cbl);
  cbl.setInput1Number(1);
  cbl.setInput2Number(1);
  cb.removeBaseline(1, 1);
  cb.addBaseline(cbl);
  cd1b.addBand(cb);
  // 2nd band
  cb.setBandNumber(1);
  cb.setNumberOfInputs(2);
  //CorrelatorBaseline cbl;
  cbl.setInput1Number(0);
  cbl.setInput2Number(1);
  cb.removeBaseline(0, 1);
  cb.addBaseline(cbl);
  cbl.setInput1Number(0);
  cbl.setInput2Number(0);
  cb.removeBaseline(0, 0);
  cb.addBaseline(cbl);
  cbl.setInput1Number(1);
  cbl.setInput2Number(1);
  cb.removeBaseline(1, 1);
  cb.addBaseline(cbl);
  cd1b.addBand(cb);


  // sum
  cd1a.addIn( cd1b );
  if (cd1a.getNumberOfBands() != 3)
    cerr << "FAILED:: cd1a number of bands should equal 3, got "
         << cd1a.getNumberOfBands() << endl;
  const vector<CorrelatorBand>& cba1 = cd1a.getBands();
  for (int idx = 0; idx < cd1a.getNumberOfBands(); ++idx)
    cout << "cd1a.Band[" << cba1[idx].getBandNumber() << "], no. Baselines= "
         << cba1[idx].getNumberOfBaselines() << endl;

  // test memory leak
  // OK on 1/28/05
  /*
  CorrelatorDataTestSl cdSZA1;
  CorrelatorDataTestSl cdSZA2;
  while(1) {
    cdSZA1 = cdSZA2;
    for (int idx = 0; idx < 100; ++idx)
      cdSZA1.addIn( cdSZA2 );
    cdSZA1.normalize();
  }
  */

  /*
    for (int idx = 0; idx < 10; ++idx) {
    CorrelatorDataTestSl* tsza = new CorrelatorDataTestSl();
    CorrelatorData cd;
    int start = 0;
    //cd.deserialize(tsza->serialize(), &start);
    cd.deserial(tsza->serial());
    delete tsza;
    }
  */
}

}

//
// @version $Revision: 1.42 $
//
// @usage Usage: tCorrelatorData
//
// @description
// Time serialization and other operations.
//
//  @key f correlator/correlator.conf string
//       Correlator config filename
//       (install or build conf dir path will be prepended)
//
//  @key iters 100 int
//       Number of iterations per test
//
//  @key verbose false bool
//       Whether or not to produce verbose output
//
//  @key startCccThread false bool
//       Whether or not to start the correlator config checker thread
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorrelatorData
//

int
Program::main( )
{
    cout << "Running..." << endl;

    const bool verbose = getBoolParameter( "verbose" );
    const bool startCccThread = getBoolParameter( "startCccThread" );

    int itersPerTest = getIntParameter( "iters" );
    if ( itersPerTest < 1 )
        itersPerTest = 1;

    // filename path must start in the conf directory. getConfFile will
    // prepend the correct path for the build directories and install
    // directories
    CorrelatorConfigChecker * const ccc =
        CorrelatorConfigChecker::getInstance(
            getConfFile( getStringParameter("f") ) );
    
    if ( startCccThread )
        ccc->start();
    
    runIt( itersPerTest, verbose );

    return 0;
}
