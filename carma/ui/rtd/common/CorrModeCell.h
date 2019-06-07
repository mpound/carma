#include "carma/ui/rtd/common/RtDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/monitor/MonitorPoint.h"
#include <string>

namespace carma {
namespace ui {
namespace rtd {

class CorrModeCell : public MonitorCell {
    public:
        /**
         * Constructor
         * @param cellWidth cell width in characters
         * @param monitorPoint reference to the MonitorPoint to dispaly
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param complexRepresentation Enum value indication how to represent
         *        a complex value as a string.  One of
         *        MonitorPointComplex::{AMP, PHASE, IMAG, REAL, COMPLEX}.
         *        This parameter has no effect if the monitor point type
         *        is not MonitorPointComplex
         * @see MonitorPointComplex::setStringRepresentation()
         */
        CorrModeCell( int  cellWidth,
                      carma::monitor::MonitorPoint & monitorPoint);
        virtual ~CorrModeCell();
        virtual CellColor computeColor();
    protected:
        // No copying
        CorrModeCell( const CorrModeCell & rhs );
        CorrModeCell& operator=( const CorrModeCell & rhs );

};

carma::ui::rtd::CorrModeCell::CorrModeCell( int cellWidth,
                            carma::monitor::MonitorPoint & monitorPoint) :
    carma::ui::rtd::MonitorCell(cellWidth, true, monitorPoint, 1)
{
}

carma::ui::rtd::CorrModeCell::~CorrModeCell()
{
}

carma::ui::rtd::CellColor
carma::ui::rtd::CorrModeCell::computeColor()  
{
    if (isGrayedOut()) return LIGHT_GRAY_TEXT_CELL_COLOR;
    
    carma::monitor::MonitorPoint::VALIDITY validity;
    
    if ( sampleNo_ == 0 )
        validity = mp_.getAveValidity( );
    else
        validity = mp_.getValidity( sampleNo_ - 1 );

    CellColor result = defaultGoodColor_;

    switch ( validity ) {
        case carma::monitor::MonitorPoint::VALID_ERROR_HIGH:
        case carma::monitor::MonitorPoint::VALID_ERROR_LOW:
        case carma::monitor::MonitorPoint::VALID_ERROR:
            result = computeErrorColor( );
            break;

        case carma::monitor::MonitorPoint::VALID_WARNING_HIGH:
        case carma::monitor::MonitorPoint::VALID_WARNING_LOW:
        case carma::monitor::MonitorPoint::VALID_WARNING:
            result = computeWarnColor( );
            break;

        default:
            const ::std::string text = computeText();
            // search for any band offline indicated by "X"
            if ( text.find("X") != ::std::string::npos )
                    result = computeWarnColor( );
            else
                result = computeGoodColor( );
            break;
    }

    return result;
    
}

} // namespace ui
} // namespace rtd
} // namespace carma
