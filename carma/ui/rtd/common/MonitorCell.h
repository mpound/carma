#ifndef CARMA_UI_RTD_MONITORCELL_H
#define CARMA_UI_RTD_MONITORCELL_H

//! @file
//!
//! Cell that takes a MonitorPoint and displays its current value
//!
//! @author Steve Scott
//!
//! $CarmaCopyright$
//!


#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/ui/rtd/common/AntennaMapper.h"
#include "carma/ui/rtd/common/RtDisplay.h"

#include "carma/szautil/DataType.h"
#include "carma/szautil/MonitorCondition.h"

#include "carma/util/ErrorException.h"

#include <boost/shared_ptr.hpp>

#include <string>
#include <sstream>
#include <iostream>
#include <map>

namespace carma {

namespace monitor {

class MonitorPoint;
class MonitorPointBool;

}  // namespace carma::monitor


namespace ui {
namespace rtd {

class MonitorCell;
typedef boost::shared_ptr<MonitorCell> MonitorCellPtr;

/// A Cell that displays the current value of a MonitorPoint, with the
/// background color gotten from a second MP.
//!
//! By default, the string is truncated if its length exceeds the amount of
//! space allocated in the cell. This behavior may be changed to give the
//! standard overflow message of a string of stars with the setNotruncate 
//! option.
//! By default, the MP for the background MP is the same as the one for the 
//! value.
class MonitorCell : public Cell {
    public:

	//------------------------------------------------------------
	// Class for manging a monitor point condition
	//------------------------------------------------------------

	class Condition {
	public:
	  monitor::MonitorPoint* mp_;
	  sza::util::DataType currVal_;
	  sza::util::MonitorCondition cond_;
	  AntennaMapper::Antenna* antenna_;
	  int sampleNo_;

	  Condition(monitor::MonitorPoint& mp, sza::util::MonitorCondition cond, int sampleNo) {
	    mp_ = &mp;
	    antenna_ = 0;
	    cond_ = cond;
	    sampleNo_ = sampleNo;
	  };

	  Condition(AntennaMapper::Antenna* antenna, sza::util::MonitorCondition cond) {
	    mp_ = 0;
	    antenna_ = antenna;
	    cond_ = cond;
	    sampleNo_ = 0;
	  };

	  void cacheCurrentValue() {

	    std::ostringstream os;
	    os.str("");
	    os << "Inside CCV 0" << std::endl;
	    RtDisplay::appendToFile(os.str());

	    if(mp_ != 0) {

	      os.str("");
	      os << "Inside CCV 0a" << std::endl;
	      RtDisplay::appendToFile(os.str());

	      switch (mp_->getValuetype()) {
	      case carma::monitor::MONITOR_VALUE_TYPE_SHORT:
		currVal_ = mp_->getMonitorPointSample(sampleNo_).getMonitorValue().sh;
		break;
	      case carma::monitor::MONITOR_VALUE_TYPE_INTEGER:		
		currVal_ = (int)mp_->getMonitorPointSample(sampleNo_).getMonitorValue().lo;
		break;
	      case carma::monitor::MONITOR_VALUE_TYPE_FLOAT:		
		currVal_ = (float)mp_->getMonitorPointSample(sampleNo_).getMonitorValue().fl;
		break;					
	      case carma::monitor::MONITOR_VALUE_TYPE_DOUBLE:		
		currVal_ = (double)mp_->getMonitorPointSample(sampleNo_).getMonitorValue().db;
		break;					
	      default:						
		os.str("");
		os << "Inside CCV 1" << std::endl;
		RtDisplay::appendToFile(os.str());

		ThrowCarmaError("Unhandled CARMA data type");  
		break;	     
	      }
	    } else {
	      os.str("");
	      os << "Inside CCV 2" << std::endl;
	      RtDisplay::appendToFile(os.str());

	      if(antenna_ == 0)
		ThrowCarmaError("A condition was set on the current frequency, but this cell has an invalid antenna descriptor");

	      os.str("");
	      os << "Inside CCV 3" << std::endl;
	      RtDisplay::appendToFile(os.str());

	      currVal_ = antenna_->frequency_.GHz();
	    }
	      os.str("");
	      os << "Inside CCV 4" << std::endl;
	      RtDisplay::appendToFile(os.str());

	  };

	  bool isSatisfied() {

	    std::ostringstream os;
	    os.str("");
	    os << "Inside isSatisfied 0" << std::endl;
	    RtDisplay::appendToFile(os.str());

	    cacheCurrentValue();

	    os.str("");
	    os << "Inside isSatisfied 1" << std::endl;
	    RtDisplay::appendToFile(os.str());

	    bool res = cond_.isSatisfiedBy(currVal_);

	    os.str("");
	    os << "Inside isSatisfied 2" << std::endl;
	    RtDisplay::appendToFile(os.str());

	    return res;
	  };
	};

	//------------------------------------------------------------
	// Methods of MonitorCell
	//------------------------------------------------------------

        static MonitorCellPtr makeCell(monitor::MonitorPoint& monitorPoint );

        static MonitorCellPtr makeCell(monitor::MonitorPoint& monitorPoint,
                                     int                    sampleNo );

        static MonitorCellPtr makeCell(int                    cellWidth,
                                     monitor::MonitorPoint& monitorPoint );

        static MonitorCellPtr makeCell(int                    cellWidth,
                                     monitor::MonitorPoint& monitorPoint,
                                     int                    sampleNo );
                                       
        static MonitorCellPtr makeCell(monitor::MonitorPoint& monitorPoint,
                                     monitor::MonitorPoint& mpColor);

        static MonitorCellPtr makeCell(monitor::MonitorPoint& monitorPoint,
                                     monitor::MonitorPoint& mpColor,
                                     int                    sampleNo );

        static MonitorCellPtr makeCell(int                    cellWidth,
                                     monitor::MonitorPoint& monitorPoint,
                                     monitor::MonitorPoint& mpColor);

        static MonitorCellPtr makeCell(int                    cellWidth,
                                     monitor::MonitorPoint& monitorPoint,
                                     monitor::MonitorPoint& mpColor,
                                     int                    sampleNo);
        
        //! @brief destructor
        virtual ~MonitorCell( );

        // Overrides Cell
        virtual void update( );
        virtual void updateColor( );

        /// Don't truncate value string; display stars if it overflows cell
        void setNotruncate( );

        void setErrorColor( CellColor errorColor );
        void setWarnColor( CellColor warnColor );
        void setGoodColor( CellColor goodColor );
        
        void setGoodColorTextOverride( const ::std::string & text,
                                       CellColor             color );
                                   
        void removeGoodColorTextOverride( const ::std::string & text );

        std::string getName();

	void setAntenna(AntennaMapper::Antenna* ant);
	bool hasAntenna();

	//------------------------------------------------------------
	// Add conditions that must be met for this monitor point to
	// be colored, else color the default color
	//------------------------------------------------------------

	void colorIf(monitor::MonitorPoint& mp, 
		     sza::util::MonitorCondition condition,
		     int sampleNo=0);
	
	// Special Condition for current LO frequency

	void colorIfFrequencyInGHz(sza::util::MonitorCondition condition);

	//------------------------------------------------------------
	// Return true if all specified conditions are currently met
	//------------------------------------------------------------

	bool conditionsAreMet();

    protected:
        /**
         * Constructor
         * @param cellWidth cell width in characters
         * @param setMpWidth use the cellWidth; otherwise use width from MP
         * @param monitorPoint reference to the MonitorPoint to display
         * @param mpColor      reference to the MonitorPoint to use for the
         *                     background color
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  
         *        Default is zero, which means use the average of all samples.
         */
        MonitorCell(int                    cellWidth,
                    bool                   setMpWidth,
                    monitor::MonitorPoint& monitorPoint,
                    monitor::MonitorPoint& mpColor, 
                    int                    sampleNo);

        /**
         * Constructor
         * @param cellWidth cell width in characters
         * @param setMpWidth use the cellWidth; otherwise use width from MP
         * @param monitorPoint reference to the MonitorPoint to display
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  
         *        Default is zero, which means use the average of all samples.
         */
        MonitorCell( int                     cellWidth,
                     bool                    setMpWidth,
                     monitor::MonitorPoint & monitorPoint,
                     int                     sampleNo );

        int getSampleNo( ) const;

        /**
         * Sets the monitor point width to the internal Cell Format
         * width.
         * @return the width that was set
         */
        int setMpWidthToFormatLength();

        virtual ::std::string computeText( );

        virtual CellColor computeColor( );

        virtual CellColor computeErrorColor( );
        virtual CellColor computeWarnColor( );
        virtual CellColor computeGoodColor( );


  protected:
        // No copying
        MonitorCell( const MonitorCell & rhs );
        MonitorCell & operator=( const MonitorCell & rhs );
        
        static MonitorCellPtr makeCell(int                    cellWidth,
                                     bool                   setMpWidth,
                                     monitor::MonitorPoint& monitorPoint,
                                     int                    sampleNo );

        static MonitorCellPtr makeCell(int                    cellWidth,
                                     bool                   setMpWidth,
                                     monitor::MonitorPoint& monitorPoint,
                                     monitor::MonitorPoint& mpColor,
                                     int                    sampleNo );

        typedef ::std::map< ::std::string, CellColor > StringToColorMap;
         
        monitor::MonitorPoint& mp_;
        monitor::MonitorPoint& mpc_;  // MP for background color

        // The sample number to display in this cell.
        const int sampleNo_;

        CellColor errorColor_;
        CellColor warnColor_;
        CellColor defaultGoodColor_;

        ::std::auto_ptr< StringToColorMap > goodColorTextOverrides_;
        
        // Flag  that determines whether data is truncated
        bool truncateFlag_;

	// Antenna this cell is associated with (if any)

	AntennaMapper::Antenna* antenna_;

	// A vector of conditions for this monitor point

	std::vector<Condition> conditions_;

        private:
            void initialize(int w, bool useW);
};


class MonitorCellComplex : public MonitorCell {
    public:
        MonitorCellComplex(
            int                                        cellWidth,
            bool                                       setMpWidth,
            carma::monitor::MonitorPointComplex &      mpComplex,
            int                                        sampleNo,
            carma::monitor::MonitorPointComplex::stringValueReturnType 
                                                       complexRep );

        MonitorCellComplex(
            int                                        cellWidth,
            bool                                       setMpWidth,
            carma::monitor::MonitorPointComplex &      mpComplex,
            int                                        sampleNo );

    protected:
        ::std::string computeText( );
        
    private:
        friend class MonitorCell;
        carma::monitor::MonitorPointComplex & mpComplex_;
        const 
        carma::monitor::MonitorPointComplex::stringValueReturnType complexRep_;

};


class MonitorCellBool : public MonitorCell {
    friend class MonitorCell;
    
    public:
        void overrideStateAppearances(
            const ::std::string & trueStateText,  CellColor trueStateColour,
            const ::std::string & falseStateText, CellColor falseStateColour );
    
        void clearOverrides( );
        
    protected:
        MonitorCellBool( int                         cellWidth,
                         bool                        setMpWidth,
                         monitor::MonitorPointBool & mpBool,
                         int                         sampleNo );

        virtual ::std::string computeText( );
        
        virtual CellColor computeGoodColor( );

    private:
        monitor::MonitorPointBool & mpBool_;
        
        bool          trueStateTextOverridden_;
        ::std::string trueStateOverrideText_;

        bool          trueStateColourOverridden_;
        CellColor     trueStateOverrideColour_;
        
        bool          falseStateTextOverridden_;
        ::std::string falseStateOverrideText_;

        bool          falseStateColourOverridden_;
        CellColor     falseStateOverrideColour_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
