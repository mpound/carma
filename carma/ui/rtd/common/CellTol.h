#ifndef CARMA_UI_RTD_CELLTOL_H
#define CARMA_UI_RTD_CELLTOL_H

#include "carma/ui/rtd/common/RtDisplay.h"


namespace carma {
namespace ui {
namespace rtd {


/**
 * A Cell with a double for data and  dynamic absolute tolerances. 
 * This is useful for cells where the normal range for the data is zero.
 * The background color used for out of range values is red.
 */
class CellFloatTol : public CellFloat {
    public:
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param legit pointer to legitimate data flag
         * @param data reference to the dynamic data
         * @param tol the absolute tolerance for an error
         * @see Format
         */
        CellFloatTol(const char* fmt, int rod, const char* legit, const float& data, 
                const float& tol);
        /**
         * Constructor, assumes data is always legitimate
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param data reference to the dynamic data
         * @param tol the absolute tolerance for an error
         * @see Format
         */
        CellFloatTol(const char* fmt, int rod, const float& data, const float& tol);
        /// Update the background color
        virtual void updateColor();

    private:
        /// Absolute tolerance
        const float& tolerance;        
};   


/**
 * A Cell with a double for data and  dynamic absolute tolerances. 
 * This is useful for cells where the normal range for the data is zero.
 * The background color used for out of range values is red.
 */
class CellDbleTol : public CellDble {
    public:
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param legit pointer to legitimate data flag
         * @param data reference to the dynamic data
         * @param tol the absolute tolerance for an error
         * @see Format
         */
        CellDbleTol(const char* fmt, int rod, const char* legit, const double& data, 
                const double& tol);
        /**
         * Constructor, assumes data is always legitimate
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param data reference to the dynamic data
         * @param tol the absolute tolerance for an error
         * @see Format
         */
        CellDbleTol(const char* fmt, int rod, const double& data, const double& tol);
        /// Update the background color
        virtual void updateColor();

    private:
        /// Absolute tolerance
        const double& tolerance;        
};   


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
