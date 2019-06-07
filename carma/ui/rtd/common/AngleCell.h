#ifndef CARMA_UI_RTD_COORDINATECELL_H
#define CARMA_UI_RTD_COORDINATECELL_H

//! @file
//!
//! Specialized Cell to display angles (e.g. RA, DEC, AZ, EL) in a 
//! variety of user-specified formats
//!
//! @author Marc Pound
//!
//! $CarmaCopyright$
//!


#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/monitor/monitorPointSpecializations.h"

#include <boost/shared_ptr.hpp>

#include <string>
#include <map>

namespace carma {
namespace ui {
namespace rtd {


   /** @typedef The format in which to display the value */
   typedef enum angleFormatTypeEnum 
   {
       // If you add more types here, be sure to
       // add also to body of computeText() !
       
       /** Display in sexagesimal HH:MM:SS.S */
       FORMAT_HMS,

       /** Display in sexagesimal DD:MM:SS.S */
       FORMAT_DMS,

       /** Display in decimal hours */
       FORMAT_DECIMAL_HOURS,

       /** Display in decimal degrees */
       FORMAT_DECIMAL_DEGREES,

       /** Display in radians*/
       FORMAT_RADIANS

   } angleFormatType ;

   /** @typedef The modulus in which to calculate the value */
   typedef enum modulusTypeEnum {
       /** Modulus 0 to 2PI */
       MOD_TWO_PI,

       /** Modulus -PI to PI */
       MOD_PI,

       /** No Modulus */
       MOD_NONE
   } modulusType;

class AngleCell;
typedef boost::shared_ptr<AngleCell> AngleCellPtr;

//!@ brief A Cell that displays the current value of a MonitorPoint that
//! represents an angle
//!
class AngleCell : public MonitorCell {
    public:
        static AngleCellPtr makeCell( int                     cellWidth,
                                     monitor::MonitorPointDouble & monitorPoint );

        static AngleCellPtr makeCell( int                     cellWidth,
                                     monitor::MonitorPointDouble & monitorPoint,
                                     int                     sampleNo );
        
        //! @brief destructor
        virtual ~AngleCell( );

        // Overrides MonitorCell
        virtual ::std::string computeText( );
	
	/**
	 * Set the display format type for the AngleCell.
	 * The unit of the associated monitor point is determined
	 * from MonitorPoint::getUnits() and Conformable Quantities
	 * are used to convert the value to appropriate units for
	 * display.  Default at construction time is radians.
	 *
	 * @param The enumerate angle format type
	 *
	 * @see carma::services::Units
	 * @see carma::services::Angle
	 * @see carma::services::DecAngle
	 */
	void setAngleFormat(angleFormatType angleFormat );

	/**
	 * The modulus in which to display the value.
	 * For instance if the angle is Declination, use
	 * MOD_PI.
	 * Default at construction time is no modulus (MOD_NONE);
	 * @param modulus The modulus type
	 */
	void setModulus( modulusType modulus );
        
	static std::string formatValue(double mpValue, std::string mpUnits, angleFormatType format, int width, modulusType modulus);

    protected:
        /**
         * Constructor
         * @param cellWidth cell width in characters
         * @param monitorPoint reference to the MonitorPoint to dispaly
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which 
	 *        means use the average of all samples.
         * @see MonitorPointComplex::setStringRepresentation()
         */
        AngleCell( int                     cellWidth,
                   bool                    setMpWidth,
                   monitor::MonitorPointDouble & monitorPoint,
                   int                     sampleNo );

    private:
        static AngleCellPtr makeCell( int                     cellWidth,
                                     bool                    setMpWidth,
                                     monitor::MonitorPointDouble & monitorPoint,
                                     int                     sampleNo );
        // No copying
        AngleCell( const AngleCell & rhs );
        AngleCell& operator=( const AngleCell & rhs );

	// display format
	angleFormatType angleFormat_;

	// modulus format
	modulusType modulus_;

	monitor::MonitorPointDouble * mpReal_;
        
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
