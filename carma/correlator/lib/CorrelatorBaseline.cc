#include "carma/correlator/lib/CorrelatorBaseline.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>
#include <ostream>

using namespace ::std;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::util;


CorrelatorBaseline::CorrelatorBaseline( const CorrelatorBaseline & rhs )
{
  // CPTRACE( Trace::TRACE5, "Copy Constructor");
  assign( rhs );
}


CorrelatorBaseline::~CorrelatorBaseline( )
{

} 

void
CorrelatorBaseline::mySerialize( char * const byteArray,
                                 int * const  offset ) const
{
  pack(ant1Number_, byteArray, offset);
  pack(ant2Number_, byteArray, offset);
  pack(input1Number_, byteArray, offset);
  pack(input2Number_, byteArray, offset);
  pack(polarization1_, byteArray, offset);
  pack(polarization2_, byteArray, offset);
  pack(boardId_, byteArray, offset);
  pack(boardSN_, byteArray, offset);

  const int numberOfSidebands = sidebands_.size();

  pack(numberOfSidebands, byteArray, offset);
  const int tmpBfStatus = bfStatusObsolete_;
  pack(tmpBfStatus, byteArray, offset);
  packSidebands(byteArray, offset);
}


int
CorrelatorBaseline::getSizeInBytes( ) const
{
  int size = 0;
  size += sizeof(ant1Number_);
  size += sizeof(ant2Number_);
  size += sizeof(input1Number_);
  size += sizeof(input2Number_);
  size += sizeof(polarization1_);
  size += sizeof(polarization2_);
  size += sizeof(boardId_);
  size += sizeof(boardSN_);

  const int numberOfSidebands = sidebands_.size();

  size += sizeof(numberOfSidebands);
  size += sizeof(int); // for bfStatusObsolete_
  
  for ( int i = 0; i < numberOfSidebands; ++i )
    size += sidebands_.at(i).getSizeInBytes();
  //cerr << "CorrelatorBaseline size[bytes]= " << size << endl;
  return size;
}


void
CorrelatorBaseline::deserializeVer0( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Baseline V0");
  unpack(input1Number_, byteArray, offset, byteArraySize);
  unpack(input2Number_, byteArray, offset, byteArraySize);
  unpack(boardId_, byteArray, offset, byteArraySize);
  unpack(boardSN_, byteArray, offset, byteArraySize);
  int numberOfSidebands = 0;
  unpack(numberOfSidebands, byteArray, offset, byteArraySize);
  int tmpBfStatus;
  unpack(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatusObsolete_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );

  // resize sidebands.
  // sidebands_.resize( numberOfSidebands, 0 );
  sidebands_.clear( );

  /*
    CPTRACE( Trace::TRACE5, "\n   input1Number_= " << input1Number_ << endl
           << "   input2Number_= " << input2Number_ << endl
           << "   boardId_= " << boardId_ << endl
           << "   boardSN_= " << boardSN_ << endl
           << "   numberOfSidebands= " << numberOfSidebands);
  */
  
  if ( numberOfSidebands == 0 ) 
    return;

  if (input1Number_ == input2Number_) {
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    asb.deserializeVer0(byteArray, offset, byteArraySize);
    sidebands_.push_back( asb );
    // sidebands_.at( 0 ) = asb;
  } else {
    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    usb.deserializeVer0(byteArray, offset, byteArraySize);
    lsb.deserializeVer0(byteArray, offset, byteArraySize);
    sidebands_.push_back(usb);
    sidebands_.push_back(lsb);
    // sidebands_.at( 0 ) = usb;
    // sidebands_.at( 1 ) = lsb;
  }
}


void
CorrelatorBaseline::deserializeVer1( const char * const byteArray,
                                     int * const        offset,
                                     const int          byteArraySize )
{
  //CPTRACE(carma::util::Trace::TRACE4, "Baseline V1");
  unpack(ant1Number_, byteArray, offset, byteArraySize);
  unpack(ant2Number_, byteArray, offset, byteArraySize);
  unpack(input1Number_, byteArray, offset, byteArraySize);
  unpack(input2Number_, byteArray, offset, byteArraySize);
  unpack(polarization1_, byteArray, offset, byteArraySize);
  unpack(polarization2_, byteArray, offset, byteArraySize);
  unpack(boardId_, byteArray, offset, byteArraySize);
  unpack(boardSN_, byteArray, offset, byteArraySize);
  int numberOfSidebands = 0;
  unpack(numberOfSidebands, byteArray, offset, byteArraySize);
  int tmpBfStatus;
  unpack(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatusObsolete_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );

  // resize sidebands.
  // sidebands_.resize( numberOfSidebands, 0 );
  sidebands_.clear( );

  /*
    CPTRACE( Trace::TRACE5, "\n   input1Number_= " << input1Number_ << endl
           << "   ant2Number_= " << ant2Number_ << endl
           << "   boardId_= " << boardId_ << endl
           << "   boardSN_= " << boardSN_ << endl
           << "   numberOfSidebands= " << numberOfSidebands);
  */
  if ( numberOfSidebands == 0 ) 
    return;

  if (input1Number_ == input2Number_) {
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    asb.deserializeVer1(byteArray, offset, byteArraySize);
    sidebands_.push_back( asb );
    // sidebands_.at( 0 ) = asb;
  } else {
    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    usb.deserializeVer1(byteArray, offset, byteArraySize);
    lsb.deserializeVer1(byteArray, offset, byteArraySize);
    sidebands_.push_back(usb);
    sidebands_.push_back(lsb);
    // sidebands_.at( 0 ) = usb;
    // sidebands_.at( 1 ) = lsb;
  }
}


void
CorrelatorBaseline::deserializeSwapVer0( const char * const byteArray,
                                         int * const        offset,
                                         const int          byteArraySize )
{
  unpackSwap(input1Number_, byteArray, offset, byteArraySize);
  unpackSwap(input2Number_, byteArray, offset, byteArraySize);
  unpackSwap(boardId_, byteArray, offset, byteArraySize);
  unpackSwap(boardSN_, byteArray, offset, byteArraySize);
  
  int numberOfSidebands = 0;
  unpackSwap(numberOfSidebands, byteArray, offset, byteArraySize);
  
  //unpackSwap(bfStatusObsolete_, byteArray, offset);
  int tmpBfStatus = MonitorPoint::UNDETERMINED;
  unpackSwap(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatusObsolete_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );

  // resize sidebands.
  // sidebands_.resize( numberOfSidebands, 0 );
  sidebands_.clear( );

  /*
    CPTRACE( Trace::TRACE5, "   input1Number_= " << input1Number_ << endl
           << "   ant2Number_= " << ant2Number_ << endl
           << "   boardId_= " << boardId_ << endl
           << "   boardSN_= " << boardSN_ << endl
           << "   numberOfSidebands= " << numberOfSidebands);
  */
  
  if ( numberOfSidebands == 0 ) 
    return;

  if (input1Number_ == input2Number_) {
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    asb.deserializeSwapVer0(byteArray, offset, byteArraySize);
    sidebands_.push_back( asb );
    // sidebands_.at( 0 ) = asb;
  } else {
    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    usb.deserializeSwapVer0(byteArray, offset, byteArraySize);
    lsb.deserializeSwapVer0(byteArray, offset, byteArraySize);
    sidebands_.push_back( usb );
    sidebands_.push_back( lsb );
    // sidebands_.at( 0 ) = usb;
    // sidebands_.at( 1 ) = lsb;
  }
}


void
CorrelatorBaseline::deserializeSwapVer1( const char * const byteArray,
                                         int * const        offset,
                                         const int          byteArraySize )
{
  unpackSwap(ant1Number_, byteArray, offset, byteArraySize);
  unpackSwap(ant2Number_, byteArray, offset, byteArraySize);
  unpackSwap(input1Number_, byteArray, offset, byteArraySize);
  unpackSwap(input2Number_, byteArray, offset, byteArraySize);
  unpackSwap(polarization1_, byteArray, offset, byteArraySize);
  unpackSwap(polarization2_, byteArray, offset, byteArraySize);
  unpackSwap(boardId_, byteArray, offset, byteArraySize);
  unpackSwap(boardSN_, byteArray, offset, byteArraySize);
  
  int numberOfSidebands = 0;
  unpackSwap(numberOfSidebands, byteArray, offset, byteArraySize);
  
  //unpackSwap(bfStatusObsolete_, byteArray, offset);
  int tmpBfStatus = MonitorPoint::UNDETERMINED;
  unpackSwap(tmpBfStatus, byteArray, offset, byteArraySize);
  bfStatusObsolete_ = static_cast< MonitorPoint::BLANKING_FLAGGING >( tmpBfStatus );

  // resize sidebands.
  // sidebands_.resize( numberOfSidebands, 0 );
  sidebands_.clear( );

  /*
    CPTRACE( Trace::TRACE5, "   input1Number_= " << input1Number_ << endl
           << "   ant2Number_= " << ant2Number_ << endl
           << "   boardId_= " << boardId_ << endl
           << "   boardSN_= " << boardSN_ << endl
           << "   numberOfSidebands= " << numberOfSidebands);
  */
  if ( numberOfSidebands == 0 ) 
    return;

  if (input1Number_ == input2Number_) {
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    asb.deserializeSwapVer1(byteArray, offset, byteArraySize);
    sidebands_.push_back(asb);
    // sidebands_.at( 0 ) = asb;
  } else {
    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    usb.deserializeSwapVer1(byteArray, offset, byteArraySize);
    lsb.deserializeSwapVer1(byteArray, offset, byteArraySize);
    sidebands_.push_back(usb);
    sidebands_.push_back(lsb);
    // sidebands_.at( 0 ) = usb;
    // sidebands_.at( 1 ) = lsb;
  }
}


bool
CorrelatorBaseline::isValid( ) const
{
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {

        if ( sideband.isValid( ) ) {
            return true;
        }
    }
    return false;
}

void
CorrelatorBaseline::addSideband( const CorrelatorSideband & sb )
{
    string ndcString;
    {
        ostringstream ndcStringStream;
        ndcStringStream << "CorrelatorBaseline::addSideband() "
            << "bl I" << getInput1Number() 
            << "-I" << getInput2Number() << ", C" 
            << getAnt1Number() << "-C" << getAnt2Number();
        ndcString = ndcStringStream.str();
    }

    ScopedLogNdc ndc( ndcString );

    // Prohibit adding a sideband if it already exists.
    if ( containsSideband( sb.getFlavor() ) ) 
        throw CARMA_EXCEPTION( ErrorException, "Sideband already exists." );

    // Point out logic errors by preventing addition of incompatible sidebands.
    if ( input1Number_ == input2Number_ && !sb.isAuto() )  
        throw CARMA_EXCEPTION( ErrorException, 
                               "Can't add cross sideband to auto baseline." );
    else if ( input1Number_ != input2Number_ && sb.isAuto() ) 
        throw CARMA_EXCEPTION( ErrorException, 
                               "Can't add auto sideband to cross baseline." );

    try { 
        sidebands_.push_back(sb);
    } catch (...) {
        logCaughtAsError( );
        throw; // Rethrow 
    }
}

bool
CorrelatorBaseline::containsSideband( 
    const CorrelatorSideband::Flavor flavor ) const
{
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
        if ( sideband.getFlavor() == flavor )
            return true;
    }
    
    return false;
}

CorrelatorSideband &
CorrelatorBaseline::getSideband( const CorrelatorSideband::Flavor flavor ) 
{
    BOOST_FOREACH( CorrelatorSideband & sideband, sidebands_ ) {
        if ( sideband.getFlavor( ) == flavor )
            return sideband;
    }
        
    throw CARMA_EXCEPTION( NotFoundException, "Auto Sideband does not exist" );
}

const CorrelatorSideband &
CorrelatorBaseline::getAutoSideband( ) const
{
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
        if ( sideband.isAuto() )
            return sideband;
    }
        
    throw CARMA_EXCEPTION( NotFoundException, "Auto Sideband does not exist" );
}

const CorrelatorSideband &
CorrelatorBaseline::getUpperSideband( ) const
{
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
        if ( sideband.isUSB() ) 
            return sideband;
    }

    throw CARMA_EXCEPTION( NotFoundException, "Upper Sideband does not exist");
}

const CorrelatorSideband &
CorrelatorBaseline::getLowerSideband( ) const
{
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
        if ( sideband.isLSB() ) 
            return sideband;
    }

    throw CARMA_EXCEPTION( NotFoundException, "Lower Sideband does not exist");
}

void
CorrelatorBaseline::packSidebands( char * const byteArray,
                                   int * const  offset ) const
{
  // do Auto sideband first. Of course, this is the only sideband
  // when input1Number == input2Number
  BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
    if ( sideband.isAuto() ) 
      sideband.serialize( byteArray, offset );
  }

  // do USB before LSB when input1Number != input2Number
  BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
    if ( sideband.isUSB() ) 
      sideband.serialize( byteArray, offset );
  }
  
  // finally do LSB
  BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands_ ) {
    if ( sideband.isLSB() )
      sideband.serialize(byteArray, offset);
  }
}


void
CorrelatorBaseline::normalize( )
{
    BOOST_FOREACH( CorrelatorSideband & sideband, sidebands_ ) {
        sideband.normalize();
    }
}


CorrelatorBaseline &
CorrelatorBaseline::operator=( const CorrelatorBaseline & rhs )
{
  if (this != &rhs) {
    // CPTRACE( Trace::TRACE5, "operator=");
    assign(rhs);
  }
  return *this;
}


void
CorrelatorBaseline::addIn( const CorrelatorBaseline & rhs )
{
    ScopedLogNdc ndc( "CorrelatorBaseline::addIn" );

    const vector< CorrelatorSideband > & rhsSbVec = rhs.getSidebands();
    BOOST_FOREACH( const CorrelatorSideband & rhsSb, rhsSbVec ) {

        const CorrelatorSideband::Flavor rhsFlavor = rhsSb.getFlavor();

        if ( containsSideband( rhsFlavor ) ) {
            getSideband( rhsFlavor ).addIn( rhsSb );
        } else {
            addSideband( rhsSb );
        }
            
    }
}


void
CorrelatorBaseline::assign( const CorrelatorBaseline & cb )
{
    // assign fundamental types
    ant1Number_ = cb.ant1Number_;
    ant2Number_ = cb.ant2Number_;
    input1Number_ = cb.input1Number_;
    input2Number_ = cb.input2Number_;
    polarization1_ = cb.polarization1_;
    polarization2_ = cb.polarization2_;
    boardId_ = cb.boardId_;
    boardSN_ = cb.boardSN_;
    bfStatusObsolete_ = cb.bfStatusObsolete_;

    const int nsb = cb.sidebands_.size();

    // sidebands_.resize( nsb, 0 );
    sidebands_.clear( );
    
    for ( int i = 0; i < nsb; ++i ) {
            sidebands_.push_back( cb.sidebands_.at( i ) );
    }
}

string
CorrelatorBaseline::getSummary( ) const
{
    ostringstream oss;

    oss << "Baseline " << input1Number_ << "-" << input2Number_ 
        << " (C" << ant1Number_ << getPolarization1String()
        << "-C" << ant2Number_ << getPolarization2String() << "): ";

    try {
        const int numChans = getUpperSideband().getNumberOfChans();
        oss << numChans << " upper, "; 
    } catch ( carma::util::NotFoundException & ) {
        oss << "0 upper, ";
    }

    try {
        const int numChans = getLowerSideband().getNumberOfChans();
        oss << numChans << " lower ";
    } catch ( carma::util::NotFoundException & ) {
        oss << "0 lower ";
    }

    try {
        const int numChans = getAutoSideband().getNumberOfChans();
        oss << "and " << numChans << " auto channels.";
    } catch ( carma::util::NotFoundException & ) {
        oss << "and 0 auto channels.";
    }

    return oss.str();
}
