#include "carma/correlator/obsRecord2/obsRecordUtils.h"
#include <sstream>
using namespace std;
using namespace carma;

string 
correlator::obsRecord2::getStringForBandWidthType( const correlator::obsRecord2::BandWidthType bw )
{
    // These strings match the section names in the correlator configuration
    // file, e.g. conf/correlator/carma15-2pol.ini
    switch ( bw ) {
        case correlator::obsRecord2::BAND_500MHZ: 
            return "BAND_500MHZ" ;
            break;
        case correlator::obsRecord2::BAND_500MHZ_3BIT: 
            return "BAND_500MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_500MHZ_4BIT: 
            return "BAND_500MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_250MHZ:
            return "BAND_250MHZ";
            break;
        case correlator::obsRecord2::BAND_250MHZ_3BIT:
            return "BAND_250MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_250MHZ_4BIT:
            return "BAND_250MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_125MHZ:
            return "BAND_125MHZ";
            break;
        case correlator::obsRecord2::BAND_125MHZ_3BIT:
            return "BAND_125MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_125MHZ_4BIT:
            return "BAND_125MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_62MHZ:
            return "BAND_62MHZ";
            break;
        case correlator::obsRecord2::BAND_62MHZ_3BIT:
            return "BAND_62MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_62MHZ_4BIT:
            return "BAND_62MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_31MHZ:
            return "BAND_31MHZ";
            break;
        case correlator::obsRecord2::BAND_31MHZ_3BIT:
            return "BAND_31MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_31MHZ_4BIT:
            return "BAND_31MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_8MHZ:
            return "BAND_8MHZ";
            break;
        case correlator::obsRecord2::BAND_8MHZ_3BIT:
            return "BAND_8MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_8MHZ_4BIT:
            return "BAND_8MHZ_4BIT";
            break;
        case correlator::obsRecord2::BAND_2MHZ:
            return "BAND_2MHZ";
            break;
        case correlator::obsRecord2::BAND_2MHZ_3BIT:
            return "BAND_2MHZ_3BIT";
            break;
        case correlator::obsRecord2::BAND_2MHZ_4BIT:
            return "BAND_2MHZ_4BIT";
            break;
        default:
            return "<unknown bandwidth type>";
            break;
    }
}

string 
correlator::obsRecord2::getStringForFpgaModeType( const correlator::obsRecord2::FpgaModeType astrobandMode )
{
    switch ( astrobandMode ) {
        default:
        case SINGLEPOL:
            return "SINGLEPOL";
            break;
        case DUALPOL:
            return "DUALPOL";
            break;
        case FULLPOL:
            return "FULLPOL";
            break;
        case CARMA23:
            return "CARMA23";
            break;
    }
}

string 
correlator::obsRecord2::getIniString( const correlator::obsRecord2::BandWidthType bw , const correlator::obsRecord2::FpgaModeType astrobandMode)
{
    std::ostringstream os;
    switch ( bw ) {
        case correlator::obsRecord2::BAND_500MHZ: 
            os << "500MHz" ;
            break;
        case correlator::obsRecord2::BAND_500MHZ_3BIT: 
            os << "500MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_500MHZ_4BIT: 
            os << "500MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_250MHZ:
            os << "250MHz";
            break;
        case correlator::obsRecord2::BAND_250MHZ_3BIT:
            os << "250MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_250MHZ_4BIT:
            os << "250MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_125MHZ:
            os << "125MHz";
            break;
        case correlator::obsRecord2::BAND_125MHZ_3BIT:
            os << "125MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_125MHZ_4BIT:
            os << "125MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_62MHZ:
            os << "62MHz";
            break;
        case correlator::obsRecord2::BAND_62MHZ_3BIT:
            os << "62MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_62MHZ_4BIT:
            os << "62MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_31MHZ:
            os << "31MHz";
            break;
        case correlator::obsRecord2::BAND_31MHZ_3BIT:
            os << "31MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_31MHZ_4BIT:
            os << "31MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_8MHZ:
            os << "8MHz";
            break;
        case correlator::obsRecord2::BAND_8MHZ_3BIT:
            os << "8MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_8MHZ_4BIT:
            os << "8MHz_4BIT";
            break;
        case correlator::obsRecord2::BAND_2MHZ:
            os << "2MHz";
            break;
        case correlator::obsRecord2::BAND_2MHZ_3BIT:
            os << "2MHz_3BIT";
            break;
        case correlator::obsRecord2::BAND_2MHZ_4BIT:
            os << "2MHz_4BIT";
            break;
        default:
            return "<unknown bandwidth type>";
            break;
    }

    if ( astrobandMode != SINGLEPOL ) // single pol has no extension
       os << "_" << getStringForFpgaModeType( astrobandMode );

    return os.str();
}

// yeah, this is kinda ugly
correlator::obsRecord2::BandWidthType
correlator::obsRecord2::getBandWidthType( 
        const carma::util::CorrelatorBandWidthType bandwidth,
        const carma::util::CorrelatorBitType       bits ) 
{
  switch ( bandwidth ) {
  default:
  case carma::util::CORR_BW_500MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_500MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_500MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_500MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_250MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_250MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_250MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_250MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_125MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_125MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_125MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_125MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_62MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_62MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_62MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_62MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_31MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_31MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_31MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_31MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_8MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_8MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_8MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_8MHZ_4BIT;
    }
    break;
  case carma::util::CORR_BW_2MHZ:
    switch  ( bits ) {
    default:
    case carma::util::CORR_2BIT:
      return carma::correlator::obsRecord2::BAND_2MHZ;
    case carma::util::CORR_3BIT:
      return carma::correlator::obsRecord2::BAND_2MHZ_3BIT;
    case carma::util::CORR_4BIT:
      return carma::correlator::obsRecord2::BAND_2MHZ_4BIT;
    }
    break;
  }
}

correlator::obsRecord2::FpgaModeType 
correlator::obsRecord2::utilFpgaModeToObsrecordFpgaMode( const carma::util::CorrelatorFpgaModeType mode )
{
    switch ( mode ) {
    default:
    case carma::util::CORR_SINGLEPOL:
        return carma::correlator::obsRecord2::SINGLEPOL;
        break;
    case carma::util::CORR_DUALPOL:
        return carma::correlator::obsRecord2::DUALPOL;
        break;
    case carma::util::CORR_FULLPOL:
        return carma::correlator::obsRecord2::FULLPOL;
        break;
    case carma::util::CORR_CARMA23:
        return carma::correlator::obsRecord2::CARMA23;
        break;
    }
}

