#include "carma/correlator/lib/CorrelatorSideband.h"

#include <complex>
#include <cmath>
#include <iomanip>

#include "carma/util/complexManip.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::util;

namespace {

typedef MonitorPoint MP;
typedef CorrelatorSideband CS;
    
const unsigned int NOFLAG_MASK = CS::A1_MINOR_TRACKING | 
                                 CS::A2_MINOR_TRACKING |
                                 CS::MONITOR_DATA_BAD;

string
getValidReasonString( const MP::BLANKING_FLAGGING bfStatus,
                      const unsigned int validReason )
{

    string answer;
    
    switch ( bfStatus ) {
        case MP::OK:
            answer += "OK: ";
            break;
        case MP::UNDETERMINED:
            answer += "UNDETERMINED: ";
            break;
        case MP::BLANKED:
            answer += "BLANKED: ";
            break;
        case MP::FLAGGED:
            answer += "FLAGGED: ";
            break;
        case MP::BLANKED_FLAGGED:
            answer += "BLANKED_FLAGGED: ";
            break;
        default:
            answer += "< error >: ";
            break;
    }

    if ( !validReason )
        answer += "< no valid reason >";
    if ( validReason & CS::A1_PHASELOCK )  
        answer += "A1_PHASELOCK ";
    if ( validReason & CS::A2_PHASELOCK )
        answer += "A2_PHASELOCK ";
    if ( validReason & CS::A1_MAJOR_TRACKING )
        answer += "A1_MAJOR_TRACKING ";
    if ( validReason & CS::A2_MAJOR_TRACKING )
        answer += "A2_MAJOR_TRACKING ";
    if ( validReason & CS::A1_TSYS_BAD )
        answer += "A1_TSYS_BAD ";
    if ( validReason & CS::A2_TSYS_BAD )
        answer += "A2_TSYS_BAD ";
    if ( validReason & CS::A1_SHADOWED )
        answer += "A1_SHADOWED ";
    if ( validReason & CS::A2_SHADOWED )
        answer += "A2_SHADOWED ";
    if ( validReason & CS::A1_OFFLINE )
        answer += "A1_OFFLINE ";
    if ( validReason & CS::A2_OFFLINE )
        answer += "A2_OFFLINE ";
    if ( validReason & CS::A1_MINOR_TRACKING )
        answer += "A1_MINOR_TRACKING ";
    if ( validReason & CS::A2_MINOR_TRACKING )
        answer += "A2_MINOR_TRACKING ";
    if ( validReason & CS::A1_CALSTATE )
        answer += "A1_CALSTATE ";
    if ( validReason & CS::A2_CALSTATE )
        answer += "A2_CALSTATE ";
    const unsigned int unknowns =  validReason & ( 
        CS::UNKNOWN12 | CS::UNKNOWN13 | 
        CS::UNKNOWN14 | CS::UNKNOWN15 | CS::UNKNOWN16 | CS::UNKNOWN17 | 
        CS::UNKNOWN18 | CS::UNKNOWN19 | CS::UNKNOWN20 );
    if ( unknowns )
        answer += "UNKNOWN10-UNKNOWN21 ";
    if ( validReason & CS::MANUAL_FLAG )
        answer += "MANUAL_FLAG ";
    if ( validReason & CS::BAND_OFFLINE )
        answer += "BAND_OFFLINE ";
    if ( validReason & CS::UNMAPPED_SIGNAL )
        answer += "UNMAPPED_SIGNAL ";
    if ( validReason & CS::MONITOR_DATA_BAD )
        answer += "MONITOR_DATA_BAD ";
    if ( validReason & CS::BAD_CHANNEL_COUNT )
        answer += "BAD_CHANNEL_COUNT ";
    if ( validReason & CS::NO_RX_IN_SIDEBAND )
        answer += "NO_RX_IN_SIDEBAND ";
    if ( validReason & CS::CORR_DATA_MISSING )
        answer += "CORR_DATA_MISSING ";
    if ( validReason & CS::CORR_DATA_INVALID )
        answer += "CORR_DATA_INVALID ";
    
    return answer;
}

} // namespace < unnamed >


void
CorrelatorSideband::addIn( const CorrelatorSideband & rhs )
{
    if ( !rhs.data_.empty() ) {
    
        if (data_.empty() ) {
            // Existing data is blanked, set to incoming data but preserve
            // both the validity flags and BFcounts.  
            const unsigned int savedValidReason = validReason_;
            const pair< unsigned int, unsigned int > savedBfCount =  
                minorTrackBfCountNoSerialize_;
            *this = rhs;  
            validReason_ = savedValidReason;
            minorTrackBfCountNoSerialize_ = savedBfCount;
        } else if ( data_.size() == rhs.data_.size() ) {

            const vector< complex< float > > & rhsData = rhs.getData();
            const vector< int > & rhsDataValid = rhs.getDataValid();

            for ( size_t idx = 0; idx < data_.size(); ++idx ) {
                // if new rhs data valid is good, then continue
                if ( rhsDataValid.at(idx) > 0 ) {
                    if ( dataValid_.at(idx) > 0 ) {
                        // if current dataValid_ is good, then sum
                        data_.at(idx) += rhsData.at(idx);
                    } else {
                        // else, replace current value with good data
                        data_.at(idx) = rhsData.at(idx);
                    }

                    // accumulate totals
                    dataValid_.at(idx) += rhsDataValid.at(idx);
                }
            }

            // Now accumulate integration time if sideband is valid
            if ( rhs.isValid() )
                stats_.addInIntegrationTime( rhs.getStats() );

        } else { 

            // Incompatible visibility sizes.  This shouldn't happen as we 
            // check that the # of channels matches what is expected upstream.
            // However, if it does, assume that the first (valid) integration 
            // is the authority - subsequent frames must match it. 
            blankData( CorrelatorSideband::BAD_CHANNEL_COUNT );
        }

    }
        
    setBlankFlagStatus( rhs.validReason_ );

}


int
CorrelatorSideband::getSizeInBytes( ) const
{
  int size = 0;
  size += sizeof(autoSideband_);
  size += sizeof(usb_);
  size += sizeof(numberOfChans_);
  size += sizeof(numberOfLags_);
  size += stats_.getSizeInBytes();
  size += sizeof(frequency_);
  size += sizeof(deltaFrequency_);
  size += sizeof(offsetFrequency_);

  if (data_.size() > 0)
    size += sizeof( DataVector::value_type ) * data_.size();

  if (dataValid_.size() > 0)
    size += sizeof( DataValidVector::value_type ) * dataValid_.size();

  // cerr << "CorrelatorSideband size[bytes]= " << size << endl;
  size += sizeof(validReason_);
  size += sizeof(bfStatus_);
  return size;
}


void
CorrelatorSideband::mySerialize( char * const byteArray,
                                 int * const  offset ) const
{
  int dsize = data_.size();
  if (dsize != numberOfChans_) {
    ostringstream os;
    os << "Number of elements of data not equal to number of channels."
       << " data elements= " << data_.size() << " numberOfChans= "
       << numberOfChans_;
    throw CARMA_ERROR(os.str());
  }
  int dvsize = dataValid_.size();
  if (dvsize != numberOfChans_) {
    ostringstream os;
    os << "Number of elements of dataValid not equal to number of channels."
       << " data elements= " << dataValid_.size() << " numberOfChans= "
       << numberOfChans_;
    throw CARMA_ERROR(os.str());
  }
  pack(autoSideband_, byteArray, offset);
  pack(usb_, byteArray, offset);
  pack(numberOfChans_, byteArray, offset);
  pack(numberOfLags_, byteArray, offset);
  stats_.serialize(byteArray, offset);
  pack(frequency_, byteArray, offset);
  pack(deltaFrequency_, byteArray, offset);
  pack(offsetFrequency_, byteArray, offset);
  pack(data_, byteArray, offset);
  pack(dataValid_, byteArray, offset);
  pack(validReason_, byteArray, offset);
  const int tmpBfStatus = bfStatus_;
  pack(tmpBfStatus, byteArray, offset);
}


void
CorrelatorSideband::deserializeVer0( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Sideband V0");

  unpack(autoSideband_, byteArray, offset, byteArraySize);
  unpack(usb_, byteArray, offset, byteArraySize);
  unpack(numberOfChans_, byteArray, offset, byteArraySize);
  unpack(numberOfLags_, byteArray, offset, byteArraySize);
  CARMA_CPTRACE(carma::util::Trace::TRACE6,"CS1 autoSideband= " << autoSideband_ << " usb= " << usb_ << " numberOfChans= " << numberOfChans_ << " numberOfLags= " << numberOfLags_);
  /*
    cerr << "\n   autoSideband_= " << autoSideband_ << endl
    << "   usb_= " << usb_                                    << endl
    << "   numberOfChans_= " << numberOfChans_                << endl
    << "   numberOfLags_= " << numberOfLags_ << endl;
  */
  stats_.deserializeVer0(byteArray, offset, byteArraySize);
  unpack(frequency_, byteArray, offset, byteArraySize);
  unpack(deltaFrequency_, byteArray, offset, byteArraySize);
  unpack(offsetFrequency_, byteArray, offset, byteArraySize);
  CARMA_CPTRACE(carma::util::Trace::TRACE6,"CS2 frequency= " << frequency_ << " deltaFrequency= " << deltaFrequency_ << " offsetFrequency= " << offsetFrequency_);
  // IMPORTANT: must resize vector before unpacking
  data_.resize(numberOfChans_);
  unpack(data_, byteArray, offset, byteArraySize);
  ostringstream ostr;
  ostr << "data ";
  for(int i = 0; i < numberOfChans_; i++){
    ostr << data_[i].real() << "_" << data_[i].imag() << " ";
  }
  CARMA_CPTRACE(carma::util::Trace::TRACE6,ostr.str());
  // IMPORTANT: must resize vector before unpacking
  dataValid_.resize(numberOfChans_);
  unpack(dataValid_, byteArray, offset, byteArraySize);
  ostr.str("dataValid ");
  for(int i = 0; i < numberOfChans_; i++){
    ostr << dataValid_[i] << " ";
  }
  CARMA_CPTRACE(carma::util::Trace::TRACE6,ostr.str());
}


void
CorrelatorSideband::deserializeVer1( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Sideband V1");
  unpack(autoSideband_, byteArray, offset, byteArraySize);
  unpack(usb_, byteArray, offset, byteArraySize);
  unpack(numberOfChans_, byteArray, offset, byteArraySize);
  unpack(numberOfLags_, byteArray, offset, byteArraySize);
  /*
    cerr << "\n   autoSideband_= " << autoSideband_ << endl
    << "   usb_= " << usb_                                    << endl
    << "   numberOfChans_= " << numberOfChans_                << endl
    << "   numberOfLags_= " << numberOfLags_ << endl;
  */
  stats_.deserializeVer1(byteArray, offset, byteArraySize);
  unpack(frequency_, byteArray, offset, byteArraySize);
  unpack(deltaFrequency_, byteArray, offset, byteArraySize);
  unpack(offsetFrequency_, byteArray, offset, byteArraySize);
  // IMPORTANT: must resize vector before unpacking
  data_.resize(numberOfChans_);
  unpack(data_, byteArray, offset, byteArraySize);
  // IMPORTANT: must resize vector before unpacking
  dataValid_.resize(numberOfChans_);
  unpack(dataValid_, byteArray, offset, byteArraySize);
  unpack(validReason_, byteArray, offset, byteArraySize);
  int tmpBfStatus;
  unpack(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatus_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );
}


void
CorrelatorSideband::deserializeSwapVer0( const char * const byteArray,
                                         int * const        offset,
                                         const int          byteArraySize )
{
  unpackSwap(autoSideband_, byteArray, offset, byteArraySize);
  unpackSwap(usb_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfChans_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfLags_, byteArray, offset, byteArraySize);
  stats_.deserializeSwapVer0(byteArray, offset, byteArraySize);
  unpackSwap(frequency_, byteArray, offset, byteArraySize);
  unpackSwap(deltaFrequency_, byteArray, offset, byteArraySize);
  unpackSwap(offsetFrequency_, byteArray, offset, byteArraySize);

  // IMPORTANT: must resize vector before unpacking
  data_.resize(numberOfChans_);
  unpackSwap(data_, byteArray, offset, byteArraySize);

  // IMPORTANT: must resize vector before unpacking
  dataValid_.resize(numberOfChans_);
  unpackSwap(dataValid_, byteArray, offset, byteArraySize);
}


void
CorrelatorSideband::deserializeSwapVer1( const char * const byteArray,
                                         int * const        offset,
                                         const int          byteArraySize )
{
  unpackSwap(autoSideband_, byteArray, offset, byteArraySize);
  unpackSwap(usb_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfChans_, byteArray, offset, byteArraySize);
  unpackSwap(numberOfLags_, byteArray, offset, byteArraySize);
  stats_.deserializeSwapVer1(byteArray, offset, byteArraySize);
  unpackSwap(frequency_, byteArray, offset, byteArraySize);
  unpackSwap(deltaFrequency_, byteArray, offset, byteArraySize);
  unpackSwap(offsetFrequency_, byteArray, offset, byteArraySize);
  // IMPORTANT: must resize vector before unpacking
  data_.resize(numberOfChans_);
  unpackSwap(data_, byteArray, offset, byteArraySize);
  // IMPORTANT: must resize vector before unpacking
  dataValid_.resize(numberOfChans_);
  unpackSwap(dataValid_, byteArray, offset, byteArraySize);
  unpackSwap(validReason_, byteArray, offset, byteArraySize);
  int tmpBfStatus = MonitorPoint::UNDETERMINED;
  unpackSwap(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatus_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );
}

void
CorrelatorSideband::setData( const vector< complex< float > > & data )
{
  data_ = data;
  numberOfChans_ = data_.size();
  dataHasBeenSet_ = true;
  if ( dataValidAllHasBeenSet_ )
    setDataValidAllPrivate(dataValidAllValue_);
  else
    setDataValidAllPrivate(true); // default
}


void
CorrelatorSideband::swapData( vector< complex< float > > & data )
{
  data_.swap( data );
  numberOfChans_ = data_.size();
  dataHasBeenSet_ = true;
  if ( dataValidAllHasBeenSet_ )
    setDataValidAllPrivate(dataValidAllValue_);
  else
    setDataValidAllPrivate(true); // default
}

bool 
CorrelatorSideband::isValid( const int channelIndex ) const 
{
  return dataValid_.at( channelIndex ) >= 1 ? true : false;
}

bool 
CorrelatorSideband::isValid() const 
{
  BOOST_FOREACH( const int & validity, dataValid_ ) {
    if ( validity >= 1 ) 
        return true;
  }
  return false;
}

void 
CorrelatorSideband::setValid( const int channelIndex, const bool val) 
{
  dataValid_.at( channelIndex ) = val ? 1 : 0;
}

void 
CorrelatorSideband::setValidAll( const bool val ) 
{
  dataValidAllValue_ = val;
  if (dataHasBeenSet_)
    setDataValidAllPrivate(val);
  else
    dataValidAllHasBeenSet_ = true;
}

unsigned int CorrelatorSideband::getValidReason( ) const 
{
  return validReason_;
}

std::pair< unsigned int, unsigned int > 
CorrelatorSideband::getMinorTrackBfCount( ) const
{
    return minorTrackBfCountNoSerialize_;
}

void 
CorrelatorSideband::setDataValidAllPrivate( const bool val ) 
{
  int dvsize = dataValid_.size();
  int dsize = data_.size();
  if (dvsize != dsize) {
    dataValid_.resize(dsize);
  }
  for (int idx = 0; idx < dsize; ++idx) {
    dataValid_.at( idx ) = val ? 1 : 0;
  }
}

void
CorrelatorSideband::computeStats(bool keepEndChannels) {
  ScopedLogNdc ndc( "CorrelatorSideband::computeStats()" );

  // start and end array index used in statistic computation
  size_t startChannel;
  size_t endChannel;

  // better have enough data to start with. If not, bail out.
  // could be a bit more picky regarding the size and whether
  // end channels are kept or not, but realistically, our data
  // should always be greater than 3.
  if (data_.size() > 3) {
    if (keepEndChannels) {
      startChannel = 0;
      endChannel   = data_.size() - 1;
    } else {
      startChannel = 1;
      endChannel   = data_.size() - 2;
    }

    // initialize sums
    float ar = data_.at( startChannel ).real();
    float ai = data_.at( startChannel ).imag();
    double vr = ar * ar;
    double vi = ai * ai;

    size_t npts = 1;
    for ( size_t idx = startChannel + 1; idx <= endChannel; ++idx ) {
      const float xr = data_.at( idx ).real();
      const float xi = data_.at( idx ).imag();
            
      ++npts;
      ar += xr;
      ai += xi;
      vr += xr * xr;
      vi += xi * xi;
    }

    if (npts > 0) {
      ar /= npts;
      ai /= npts;
      vr = vr / npts - ar * ar;
      vi = vi / npts - ai * ai;
      if (npts > 1) {
	vr = vr * npts / (npts - 1);
	vi = vi * npts / (npts - 1);
      } else {
	vr = 0.0;
	vi = 0.0;

	// set npts to 1 so standard deviation done below doesn't blow up
	npts = 1;
      }
    } else {
      vr = 0.0;
      vi = 0.0;

      // set npts to 1 so standard deviation done below doesn't blow up
      npts = 1;
    }

    stats_.setAvg( complex< float >( ar, ai ) );
    stats_.setVariance(
		       complex< float >( static_cast<float>( vr ), 
					 static_cast<float>( vi ) ) );
                              
    // compute standard deviation
    vr = sqrt(vr / npts);
    vi = sqrt(vi / npts);

    stats_.setStandardDeviation(
				complex< float >( static_cast<float>( vr ), 
						  static_cast<float>( vi ) ) );
  } else { // not enough data, so just set values to 0
    stats_.setAvg(complex< float >( 0.0, 0.0));
    stats_.setVariance(complex< float >( 0.0, 0.0));
    stats_.setStandardDeviation(complex< float >( 0.0, 0.0));
  }
}


void
CorrelatorSideband::normalize( )
{
  ScopedLogNdc ndc( "CorrelatorSideband::normalize()" );
  const size_t dsize = data_.size();

  if ( data_.size() != dataValid_.size() ) 
    programLogErrorIfPossible("Data size and validity sizes do NOT match.");

  for ( size_t i = 0; i < dsize; ++i ) {
    const int dvi = dataValid_.at(i);
        
    if ( (dvi < 0) || (dvi > 1) ) {
      data_.at(i) /= dvi;
      dataValid_.at(i) = 1;
    }
  }
    
  // must recompute stats
  computeStats();
}

string
CorrelatorSideband::getSummary() const
{
    ostringstream sum;
    if ( autoSideband_ ) {
        if ( usb_ ) 
            sum << "INVALID SB: ";
        else
            sum << "AUTO ";
    } else {
        if ( usb_ ) 
            sum << "USB: ";
        else
            sum << "LSB: ";
    }

    sum << numberOfChans_ << " chans (" << numberOfLags_ << " lags) "
        << "rxfreq " << std::fixed << setprecision( 1 ) << frequency_ 
        << "+"  << std::fixed << setprecision( 1 ) <<  offsetFrequency_ 
        << "MHz, chan width " << deltaFrequency_ << ", "
        << getValidReasonString( bfStatus_, validReason_ ) << ".";

    return sum.str();
}

string
CorrelatorSideband::getSpectra() const
{
    ostringstream oss;

    oss << setiosflags(ios::fixed);

    if ( !data_.empty() )
        oss << "  Chan       Real           Imag    (amp, phase deg)" << endl;

    BOOST_FOREACH( complex<float> & chan, data_ ) {
        oss << std::scientific << std::showpoint
            << setw(12) << setprecision(3) << chan.real() << " "
            << setw(12) << setprecision(3) << chan.imag()
            << "  (" << setprecision(2) << amp( chan ) << ", "
            << std::fixed << setprecision(1) << phase( chan ) << ")"
            << endl;
    }

    return oss.str();
}

void
carma::correlator::lib::CorrelatorSideband::flagData( 
    const unsigned int reason )
{
    setBlankFlagStatus( reason );
}

void
carma::correlator::lib::CorrelatorSideband::unflagData(
            const unsigned int reason )
{
    unsetBlankFlagStatus(reason);
}

void
carma::correlator::lib::CorrelatorSideband::blankData(
    const unsigned int reason )
{
    const vector< complex< float > > empty;
    setValidAll( false );
    setData( empty );
    stats_ = CorrelatorStats();
    
    setBlankFlagStatus( reason );
}

void
carma::correlator::lib::CorrelatorSideband::setBlankFlagStatus( 
    const unsigned int reason ) 
{
    validReason_ |= reason;

    // Increment bf count if A1 or A2 minor tracking is being set.
    if ( reason & A1_MINOR_TRACKING ) 
        minorTrackBfCountNoSerialize_.first++;

    if ( reason & A2_MINOR_TRACKING )
        minorTrackBfCountNoSerialize_.second++;

    if ( data_.empty() ) {
        bfStatus_ = MP::BLANKED;
    } else if ( validReason_ & (~NOFLAG_MASK) ) {
        bfStatus_ = MP::FLAGGED;
    } else {
        bfStatus_ = MP::OK;
    }

}

void
carma::correlator::lib::CorrelatorSideband::unsetBlankFlagStatus( 
    const unsigned int reason ) 
{
    validReason_ &= ~reason;
}

