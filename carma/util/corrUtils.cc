//$Id: corrUtils.cc,v 1.3 2014/09/09 22:30:51 iws Exp $
#include "carma/util/corrUtils.h"

#include "carma/util/AstroBand.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/StringUtils.h"

using namespace carma;
using namespace carma::util;
using namespace std;

namespace carma {
    namespace util {


        // ADB: Pairs are hashable and hence make great std::map keys.
        typedef std::pair< CorrelatorBandWidthType, 
                           CorrelatorBitType > BWBITKey;

        // Wideband cobra correlator has fixed number of channels
        const unsigned NUM_COBRA_WIDEBAND_CHANS = 15;

        // Van Vleck correction correlator efficiencies.
        // Taken from Kevin Rauch's calculation.
        // See http://www.mmarray.org/wiki/Correlator_efficiency#Important_numbers
        const float CORR_EFF_2BIT = 0.872;
        const float CORR_EFF_3BIT = 0.963;
        const float CORR_EFF_4BIT = 0.984;

        PthreadMutex gMasterGuard(PTHREAD_MUTEX_RECURSIVE);
        typedef ScopedLock< PthreadMutex > MasterGuardLockType;
        static std::map<std::string,float> * bandwidthMap_ = 0;
        static std::map<std::string,unsigned> * abConfMap_ = 0;
        static std::map<BWBITKey,unsigned> * bwBitMap_ = 0;

        // map of bandwidth mode versus true bandwidth in MHz
        static void constructBandWidthMap() {
            MasterGuardLockType lock( gMasterGuard );
            if ( bandwidthMap_ == 0 ) {
                bandwidthMap_ = new map<std::string, float>;
                bandwidthMap_->insert( make_pair("500MHz",500.0) );
                bandwidthMap_->insert( make_pair("250MHz",250.0) );
                bandwidthMap_->insert( make_pair("125MHz",125.0) );
                bandwidthMap_->insert( make_pair("62MHz",62.5) );
                bandwidthMap_->insert( make_pair("31MHz",31.25) );
                bandwidthMap_->insert( make_pair("8MHz",7.8125) );
                bandwidthMap_->insert( make_pair("2MHz",1.953125) );
            }
        }

        // map of astroband configuration vs. number of correlator
        // bands for that config.
        static void constructABConfMap() {
            MasterGuardLockType lock( gMasterGuard );
            if ( abConfMap_ == 0 ) {
                abConfMap_ = new map<std::string, unsigned>;
                abConfMap_->insert( make_pair("LL",1) );
                abConfMap_->insert( make_pair("RR",1) );
                abConfMap_->insert( make_pair("DUALPOL",2) );
                abConfMap_->insert( make_pair("FULLSTOKES",2) );
                abConfMap_->insert( make_pair("CARMA23",2) );
                abConfMap_->insert( make_pair("MAXSENS_DUALPOL",1) );
                abConfMap_->insert( make_pair("MAXSENS_CARMA23",2) );
            }
        }

        static void constructBwBitMap()
        {
            /* Correlator channel data taken from 
             * http://cedarflat.mmarray.org/observing/doc/instrument_desc.html
             */
            MasterGuardLockType lock( gMasterGuard );
            if ( bwBitMap_ == 0 ) {
                bwBitMap_ = new map<BWBITKey, unsigned>;
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_500MHZ,CORR_2BIT), 95) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_500MHZ,CORR_3BIT), 39) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_500MHZ,CORR_4BIT), 15) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_250MHZ,CORR_2BIT), 191) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_250MHZ,CORR_3BIT), 79) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_250MHZ,CORR_4BIT), 31) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_125MHZ,CORR_2BIT), 319) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_125MHZ,CORR_3BIT), 159) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_125MHZ,CORR_4BIT), 63) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_62MHZ,CORR_2BIT), 383) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_62MHZ,CORR_3BIT), 255) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_62MHZ,CORR_4BIT), 127) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_31MHZ,CORR_2BIT), 383) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_31MHZ,CORR_3BIT), 319) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_31MHZ,CORR_4BIT), 159) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_8MHZ,CORR_2BIT), 383) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_8MHZ,CORR_3BIT), 319) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_8MHZ,CORR_4BIT), 159) );

                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_2MHZ,CORR_2BIT), 383) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_2MHZ,CORR_3BIT), 319) );
                bwBitMap_->insert(
                        make_pair(BWBITKey(CORR_BW_2MHZ,CORR_4BIT), 159) );
            }
        }

    } // namespace correlator
} // namespace carma

carma::util::hardwareType
carma::util::hwType( unsigned int bandNo ) 
{
  // AstroBands 1-8  are spectral line correlator (as of 1/2011)
  // AstroBands 9-24 are wideband correlator
  // rest UNKNOWN
  // *** This prescription may change in the future. ***

  if(bandNo >= 1 && bandNo <= 8) 
    return HARDWARE_TYPE_CARMA;
  else if(bandNo >= 9 && bandNo <= 24)
    return HARDWARE_TYPE_COBRA;
// MWP : For now don't change AstroBand:nBandMax_ until
// the ripple effects are understood. So hardcode 40 here.
  else if(bandNo >= 25 && bandNo <= 40 )
    return HARDWARE_TYPE_C3G;
  else
    return HARDWARE_TYPE_UNKNOWN;
}

/**.......................................................................
 * Return the hardware type for an astroband
 */
carma::util::hardwareType
carma::util::hwType(carma::util::AstroBand& astroBand)
{
  return hwType(astroBand.bandNo_);
}

float 
carma::util::actualBandwidth( const string & bwstring )
{
    if ( bandwidthMap_ == 0 ) constructBandWidthMap();
    const string bwuc = StringUtils::lowASCIIAlphaNumericToUpper( bwstring );
    if ( bandwidthMap_->find( bwstring ) != bandwidthMap_->end() ) {
        return bandwidthMap_->find(bwstring)->second;
    }
    ostringstream os;
    os << "In corrUtils::actualBandwidth(): Unrecognized bandwidth string - " 
       << bwstring;
    throw CARMA_EXCEPTION(carma::util::NotFoundException, os);
}

const string 
carma::util::getStringForHardwareType( const hardwareType hwType )
{
    switch( hwType ) {
        case HARDWARE_TYPE_CARMA:
            return "HARDWARE_TYPE_CARMA";
        case HARDWARE_TYPE_COBRA:
            return "HARDWARE_TYPE_COBRA";
        case HARDWARE_TYPE_C3G:
            return "HARDWARE_TYPE_C3G";
        default:
        case HARDWARE_TYPE_UNKNOWN:
            return "HARDWARE_TYPE_UNKNOWN";
    }
}

unsigned
carma::util::numExpectedCorrBands( 
        const string & abConf,
        const carma::util::CorrelatorType corrType )
{
    if ( abConfMap_ == 0 ) constructABConfMap();

    // If configuration not found, return zero.
    if ( abConfMap_->find(abConf) == abConfMap_->end() )
        return 0;

    // Wideband correlator only ever has one corr band.
    if ( corrType == CORR_WIDEBAND )
        return 1;

    // Found it, return the value.
    return abConfMap_->find( abConf )->second;
}

unsigned 
carma::util::numExpectedAstroChans(
              const CorrelatorType corrType, 
              const CorrelatorBandWidthType bwType,
              const CorrelatorBitType bitType,
              const CorrelatorFpgaModeType corrFpgaMode )
{
    // Wideband correlator only ever has one corr band 
    // and one mode 500MHZ, 2Bit.
    if ( corrType == CORR_WIDEBAND )
        return NUM_COBRA_WIDEBAND_CHANS;

    if ( corrType != CORR_SPECTRAL )
        return 0;

    if ( bwBitMap_ == 0 ) constructBwBitMap();
    BWBITKey key(bwType,bitType);
    if ( bwBitMap_->find(key) == bwBitMap_->end() )
        return 0;

    switch ( corrFpgaMode ) {
        case CORR_SINGLEPOL:
        case CORR_DUALPOL:
            return bwBitMap_->find(key)->second;
        case CORR_FULLPOL:
        case CORR_CARMA23:
            return ( ( bwBitMap_->find(key)->second + 2 ) / 2 ) - 1;
        default:
            throw CARMA_EXCEPTION( carma::util::NotFoundException, 
                                   "Unknown correlator fpga mode." );
    }
}

float
carma::util::correlatorEfficiency(const CorrelatorBitType bitType)
{
    switch( bitType ) 
    {
        default:
        case CORR_2BIT:
            return CORR_EFF_2BIT;
        case CORR_3BIT:
            return CORR_EFF_3BIT;
        case CORR_4BIT:
            return CORR_EFF_4BIT;
    }
}

carma::util::CorrelatorBandWidthType 
carma::util::getBandWidthTypeForBandWidth( const float bw ) 
{
    if ( bw < 3.0 )
        return carma::util::CORR_BW_2MHZ;
    if ( bw < 9.0 )
        return carma::util::CORR_BW_8MHZ;
    if ( bw < 32.0 )
        return carma::util::CORR_BW_31MHZ;
    if ( bw < 63.0 )
        return carma::util::CORR_BW_62MHZ;
    if ( bw < 126.0 )
        return carma::util::CORR_BW_125MHZ;
    if ( bw < 251.0 )
        return carma::util::CORR_BW_250MHZ;

    return carma::util::CORR_BW_500MHZ;
}

carma::util::CorrelatorType
carma::util::getCorrTypeForAstroBandNo( const int abNo )
{
    if ( abNo < 1 )
        return carma::util::CORR_NONE;
    if ( abNo < 9 )
        return carma::util::CORR_SPECTRAL;
    if ( abNo > 8 && abNo <= 24 )
        return carma::util::CORR_WIDEBAND;

    if( abNo > 24 && abNo <= 32) {
      return carma::util::CORR_C3GMAX23;
    }

    if( abNo > 32 && abNo <= 40)
      return carma::util::CORR_C3GMAX8;

    return carma::util::CORR_NONE;
}

std::string
carma::util::getStringForCorrType( const carma::util::CorrelatorType corr)
{
  carma::util::CorrelatorSet corrSet(corr);
  return corrSet.corrTypeString();
}

/*
unsigned
carma::util::firstAstroBandNo(const carma::util::CorrelatorType type) {
    switch ( type ) {
        case CORR_SPECTRAL:
            return 1;
        case CORR_WIDEBAND:
            return 9;
        case CORR_C3GMAX23:
            return 9;
            return 9;
}

unsigned
carma::util::lastAstroBandNo(const carma::util::CorrelatorType type)
*/
