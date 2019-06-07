#ifndef CORRELATORBASELINE_H
#define CORRELATORBASELINE_H

#include <vector>
#include "carma/util/Serializable.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/correlator/lib/CorrelatorSideband.h"
#include "carma/correlator/lib/CorrelatorPolarization.h"

/**
 * @file CorrelatorBaseline.h
 *
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      class CorrelatorBaseline;

      typedef ::std::vector< CorrelatorBaseline > BaselineVector;

      /**
       * Class to hold Correlator Baseline data.
       */
      class CorrelatorBaseline : public carma::util::Serializable {
      public:

        /**
         *  Constructor
         */
        explicit CorrelatorBaseline();

        /**
         * Copy Constructor
         */
        CorrelatorBaseline( const CorrelatorBaseline & rhs );

        /**
         *  Destructor
         */
        virtual ~CorrelatorBaseline();

        /**
         * Swap this instance with another CorrelatorBaseline instance
         */
        void swap( CorrelatorBaseline & rhs );

        /**
         *  Called by serialize method. This is the place to call pack
         *  methods for internal variable.
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         * call to reconstruct the object.
         */
        void deserializeVer0( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        void deserializeVer1( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        /**
         * call to reconstruct the object. Swap bytes.
         */
        void deserializeSwapVer0( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        void deserializeSwapVer1( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        /**
         *  Normalize baseline data. Normally called after summing
         *  together baselines.
         */
        void normalize();

        /**
         * Return size in bytes
         */
        int getSizeInBytes() const;

        /**
         *  Set the first input number for this baseline.
         */
        void setInput1Number(int i1num);

	void setAnt1Number(int a1num);
        /**
         *  Get the first input number for this baseline.
         */
        int getInput1Number() const;

	int getAnt1Number() const;

        /**
         *  Set the second input number for this baseline.
         */
        void setInput2Number(int i2num);

	void setAnt2Number(int a2num);

        /**
         *  Get the second input number for this baseline.
         */
        int getInput2Number() const;

	int getAnt2Number() const;

        /**
         *  Set the board ID from which this data came from.
         */
        void setBoardId(int boardId);

        /**
         *  Get the board ID from which this data came from.
         */
        int getBoardId() const;

        /**
         *  Set the Board serial number from which this data came from.
         */
        void setBoardSN(int boardSN);

        /**
         *  Get the Board serial number from which this data came from.
         */
        int getBoardSN() const;

        /**
         *  Get the number of Sidebands for this baseline.
         */
        int getNumberOfSidebands() const;

        /**
         *  Add a sideband object to this baseline. 
         */
        void addSideband(const carma::correlator::lib::CorrelatorSideband & sb);

        /**
         *  Get an UpperSideband object.
         *  @throw NotFoundException("Upper Sideband does not exists");
         */
        const carma::correlator::lib::CorrelatorSideband &
          getUpperSideband() const;

        /**
         *  Get a LowerSideband object.
         *  @throw NotFoundException("Lower Sideband does not exists");
         */
        const carma::correlator::lib::CorrelatorSideband &
          getLowerSideband() const;

        /**
         *  Get an AutoSideband object.
         *  @throw NotFoundException("Auto Sideband does not exists");
         */
        const carma::correlator::lib::CorrelatorSideband &
          getAutoSideband() const;

        /**
         *  Get all sidebands stored for this baseline. Check size of
         *  vector to determine how many sidebands are present.
         */
        const std::vector<carma::correlator::lib::CorrelatorSideband> &
          getSidebands() const;

        std::vector<carma::correlator::lib::CorrelatorSideband> &
          getSidebands();

        /** 
         * Flag data with specified reason.
         * @param reason Bitwise or of CorrelatorSideband::ValidReason.
         */
        void flagData( unsigned int reason );

        /**
         * Blank data with specified reason (removes all data).
         * @param reason Bitwise or of CorrelatorSideband::ValidReason.
         */
        void blankData( unsigned int reason );

        // Deprecated - underlying data is no longer set.
        carma::monitor::MonitorPoint::BLANKING_FLAGGING
          getBlankFlagStatusDEPRECATED() const;

        /**
         * Returns true if any sideband is valid, false otherwise.
         */
        bool isValid( ) const;


        /**
         *  == operator. Baselines are equal if input numbers are equal.
         */
        friend bool operator==( const CorrelatorBaseline & lhs,
                                const CorrelatorBaseline & rhs ) {
          return (lhs.getInput1Number() == rhs.getInput1Number() &&
                  lhs.getInput2Number() == rhs.getInput2Number());
        }

        /**
         * = operator. Assignment
         */
        CorrelatorBaseline & operator=( const CorrelatorBaseline & rhs );

        /**
         *  Add a CorrelatorBaseline into this one
         */
        void addIn( const CorrelatorBaseline & rhs );

	/**
	 *  Set polarization & antenna number
	 */
	void setAntPol1( int antNum, carma::correlator::lib::Polarization polarization );

	void setAntPol2( int antNum, carma::correlator::lib::Polarization polarization );

	/**
	 *  Get polarization
	 */
	Polarization getPolarization1() const;

	Polarization getPolarization2() const;

	std::string getPolarization1String() const;

	std::string getPolarization2String() const;

	std::string getPolarization() const;

    AntNoPolPair getAntPol1() const;
    AntNoPolPair getAntPol2() const;

    std::string getSummary() const;
    

    bool containsSideband( CorrelatorSideband::Flavor flavor ) const;
      
private:
        carma::correlator::lib::CorrelatorSideband & 
        getSideband( CorrelatorSideband::Flavor flavor );

        void packSidebands( char * byteArray, int * offset ) const;
        void assign(const CorrelatorBaseline& cb);

	void setPolarization1( const carma::correlator::lib::Polarization polarization );

	void setPolarization2( const carma::correlator::lib::Polarization polarization );

        int                                      ant1Number_;
        int                                      ant2Number_;
	int                                      input1Number_;
	int                                      input2Number_;
        int                                      boardId_;
        int                                      boardSN_;
	carma::correlator::lib::Polarization     polarization1_;
	carma::correlator::lib::Polarization     polarization2_;

        monitor::MonitorPoint::BLANKING_FLAGGING bfStatusObsolete_;
        ::std::vector< CorrelatorSideband >    sidebands_;
      }; // End class CorrelatorBaseline

    } // End namespace lib
  } // End namespace correlator
} // End namespace carma


inline
carma::correlator::lib::CorrelatorBaseline::CorrelatorBaseline( ) :
ant1Number_( -1 ),
ant2Number_( -1 ),
input1Number_( 0 ),
input2Number_( 0 ),
boardId_( 0 ),
boardSN_( 0 ),
polarization1_(RIGHT_POL),
polarization2_(RIGHT_POL),
bfStatusObsolete_( monitor::MonitorPoint::OK ),
sidebands_()
{
}


inline void
carma::correlator::lib::CorrelatorBaseline::swap( CorrelatorBaseline & rhs )
{
    ::std::swap( ant1Number_, rhs.ant1Number_ );
    ::std::swap( ant2Number_, rhs.ant2Number_ );
    ::std::swap( input1Number_, rhs.input1Number_ );
    ::std::swap( input2Number_, rhs.input2Number_ );
    ::std::swap( boardId_, rhs.boardId_ );
    ::std::swap( boardSN_, rhs.boardSN_ );
    ::std::swap( bfStatusObsolete_, rhs.bfStatusObsolete_ );
    sidebands_.swap( rhs.sidebands_ );
    ::std::swap( polarization1_, rhs.polarization1_ );
    ::std::swap( polarization2_, rhs.polarization2_ );
}


inline int
carma::correlator::lib::CorrelatorBaseline::getInput1Number( ) const
{
    return input1Number_;
}

inline int
carma::correlator::lib::CorrelatorBaseline::getAnt1Number( ) const
{
    return ant1Number_;
}

inline int
carma::correlator::lib::CorrelatorBaseline::getInput2Number( ) const
{
    return input2Number_;
}

inline int
carma::correlator::lib::CorrelatorBaseline::getAnt2Number( ) const
{
    return ant2Number_;
}

inline int
carma::correlator::lib::CorrelatorBaseline::getBoardId( ) const
{
    return boardId_;
}


inline int
carma::correlator::lib::CorrelatorBaseline::getBoardSN( ) const
{
    return boardSN_;
}


inline int
carma::correlator::lib::CorrelatorBaseline::getNumberOfSidebands( ) const
{
    return sidebands_.size();
}

inline carma::monitor::MonitorPoint::BLANKING_FLAGGING
carma::correlator::lib::CorrelatorBaseline::getBlankFlagStatusDEPRECATED( ) const
{
    return bfStatusObsolete_;
}

inline const ::std::vector< carma::correlator::lib::CorrelatorSideband > &
carma::correlator::lib::CorrelatorBaseline::getSidebands( ) const
{
    return sidebands_;
}


inline ::std::vector< carma::correlator::lib::CorrelatorSideband > &
carma::correlator::lib::CorrelatorBaseline::getSidebands( )
{
    return sidebands_;
}


inline void
carma::correlator::lib::CorrelatorBaseline::setInput1Number( const int i1num )
{
    input1Number_ = i1num;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setAnt1Number( const int a1num )
{
    ant1Number_ = a1num;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setInput2Number( const int i2num )
{
    input2Number_ = i2num;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setAnt2Number( const int a2num )
{
    ant2Number_ = a2num;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setBoardId( const int boardId )
{
    boardId_ = boardId;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setBoardSN( const int boardSN )
{
    boardSN_ = boardSN;
}

inline void 
carma::correlator::lib::CorrelatorBaseline::flagData( unsigned int reason )
{
    for( unsigned int i = 0; i < sidebands_.size(); i++ ) {
        sidebands_.at(i).flagData( reason );
    }
}

inline void 
carma::correlator::lib::CorrelatorBaseline::blankData( unsigned int reason )
{
    for( unsigned int i = 0; i < sidebands_.size(); i++ ) {
        sidebands_.at(i).blankData( reason );
    }
}

inline void
carma::correlator::lib::CorrelatorBaseline::setPolarization1(
    const carma::correlator::lib::Polarization polarization)
{
  polarization1_ = polarization;
}

inline void
carma::correlator::lib::CorrelatorBaseline::setPolarization2(
    const carma::correlator::lib::Polarization polarization)
{
  polarization2_ = polarization;
}

inline void 
carma::correlator::lib::CorrelatorBaseline::setAntPol1(
	   const int antNum, const carma::correlator::lib::Polarization polarization ){
  setAnt1Number(antNum);
  setPolarization1(polarization);
}

inline void 
carma::correlator::lib::CorrelatorBaseline::setAntPol2(
	   const int antNum, const carma::correlator::lib::Polarization polarization ){
  setAnt2Number(antNum);
  setPolarization2(polarization);
}

inline carma::correlator::lib::Polarization
carma::correlator::lib::CorrelatorBaseline::getPolarization1( ) const
{
  return polarization1_;
}


inline carma::correlator::lib::Polarization
carma::correlator::lib::CorrelatorBaseline::getPolarization2( ) const
{
  return polarization2_;
}

inline carma::correlator::lib::AntNoPolPair
carma::correlator::lib::CorrelatorBaseline::getAntPol1( ) const
{
    return AntNoPolPair( ant1Number_, polarization1_ );
}

inline carma::correlator::lib::AntNoPolPair
carma::correlator::lib::CorrelatorBaseline::getAntPol2( ) const
{
    return AntNoPolPair( ant2Number_, polarization2_ );
}

inline std::string
carma::correlator::lib::CorrelatorBaseline::getPolarization1String( ) const
{
  if(polarization1_ == LEFT_POL){
    return "L";
  }
  else if(polarization1_ == RIGHT_POL){
    return "R";
  }
  else if(polarization1_ == HORIZONTAL_POL){
    return "X";
  }
  else if(polarization1_ == VERTICAL_POL){
    return "Y";
  }
  return "NONE";
}

inline std::string
carma::correlator::lib::CorrelatorBaseline::getPolarization2String( ) const
{
  if(polarization2_ == LEFT_POL){
    return "L";
  }
  else if(polarization2_ == RIGHT_POL){
    return "R";
  }
  else if(polarization2_ == HORIZONTAL_POL){
    return "X";
  }
  else if(polarization2_ == VERTICAL_POL){
    return "Y";
  }
  return "NONE";
}

inline std::string
carma::correlator::lib::CorrelatorBaseline::getPolarization( ) const
{
  std::string output = "";
  if(polarization1_ == LEFT_POL){
    output += "L";
  }
  else if(polarization1_ == RIGHT_POL){
    output += "R";
  }
  else if(polarization1_ == HORIZONTAL_POL){
    output += "X";
  }
  else if(polarization1_ == VERTICAL_POL){
    output += "Y";
  }
  else{
    return "NONE";
  }
  if(polarization2_ == LEFT_POL){
    output += "L";
  }
  else if(polarization2_ == RIGHT_POL){
    output += "R";
  }
  else if(polarization2_ == HORIZONTAL_POL){
    output += "X";
  }
  else if(polarization2_ == VERTICAL_POL){
    output += "Y";
  }
  else{
    return "NONE";
  }
  return output;
}

#endif
