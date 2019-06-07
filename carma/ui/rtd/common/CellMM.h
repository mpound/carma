#ifndef CARMA_UI_RTD_CELLMM_H
#define CARMA_UI_RTD_CELLMM_H

#include "carma/ui/rtd/common/RtDisplay.h"


namespace carma {
namespace ui {
namespace rtd {


/**
 * A Cell with short data and thresholds for setting background color.
 * Static limits, two sets of thresholds (Warn/Err) with
 * two different colors.
 */
class CellShortMM : public CellShort {
    public:
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param legit pointer to legitimate data flag
         * @param data  a reference to the data
         * @see Format
         */
        CellShortMM(const char* fmt, const char* legit, const short& data);
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param data  a reference to the data
         * @see Format
         */
        CellShortMM(const char* fmt, const short& data);
        
        /// Sets limits for background colors
        void setLimits(short warnHi, short warnLo, short errHi, short errLo); 
        /// Update the background color.
        virtual void updateColor();
        /// Set the background color for the warning condition
        void setWarnColor(CellColor color);
        /// Set the background color for the error condition
        void setErrColor(CellColor color);

    private:
        /// Flag to make sure we have set limits
        short  limSet;
        /// Limits for Warn     
        short  warnHi;     
        short  warnLo;
        /// Limits for Err
        short  errHi;      
        short  errLo;      
        /// Warning color
        CellColor   warnColor_;
        /// Error color
        CellColor   errColor_;
};   


/**
 * A CellDble variant with max and min limits to set background colors.
 * The limits are fixed, with two sets of thresholds (Warn/Err) that map
 * to two different colors.
 * The constructors are the same as CellDble with the limits being set
 * with an additional method call.
 */
class CellFloatMM : public CellFloat {
    public:
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param legit pointer to legitimate data flag
         * @param data reference to the dynamic data
         * @see Format
         */
        CellFloatMM(const char* fmt, int rod, const char* legit, const float& data);    
        /**
         * Constructor, data is assumed to always be legitimate
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param data reference to the dynamic data
         * @see Format
         */
        CellFloatMM(const char* fmt, int rod, const float& data);    
        
        /// Sets limits
        void setLimits(float warnHi, float warnLo, float errHi, float errLo); 
        /// Update background color based on limits
        virtual void updateColor();
        /// Sets color to use for warning (yellow is default)
        void setWarnColor(CellColor color);
        /// Sets color to use for error (red is default)
        void setErrColor(CellColor color);

    private:
        /// Flag that indicates that limits have been set
        int    limSet;     
        /// High limit for warning
        float warnHi;     
        /// Low limit for warning
        float warnLo;
        /// High limit for error
        float errHi;     
        /// Low limit for error
        float errLo;      
        /// Warning background color
        CellColor   warnColor_;
        /// Error background color
        CellColor   errColor_;
};   


/**
 * A CellDble variant with max and min limits to set background colors.
 * The limits are fixed, with two sets of thresholds (Warn/Err) that map
 * to two different colors.
 * The constructors are the same as CellDble with the limits being set
 * with an additional method call.
 */
class CellDbleMM : public CellDble {
    public:
        /**
         * Constructor
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param legit pointer to legitimate data flag
         * @param data reference to the dynamic data
         * @see Format
         */
        CellDbleMM(const char* fmt, int rod, const char* legit, const double& data);    
        /**
         * Constructor, data is assumed to always be legitimate
         * @param fmt standard text cell format string
         * @param rod number of places to the right of the decimal point
         * @param data reference to the dynamic data
         * @see Format
         */
        CellDbleMM(const char* fmt, int rod, const double& data);    
        
        /// Sets limits
        void setLimits(double warnHi, double warnLo, double errHi, double errLo); 
        /// Update background color based on limits
        virtual void updateColor();
        /// Sets color to use for warning (yellow is default)
        void setWarnColor(CellColor color);
        /// Sets color to use for error (red is default)
        void setErrColor(CellColor color);

    private:
        /// Flag that indicates that limits have been set
        int    limSet;     
        /// High limit for warning
        double warnHi;     
        /// Low limit for warning
        double warnLo;
        /// High limit for error
        double errHi;     
        /// Low limit for error
        double errLo;      
        /// Warning background color
        CellColor   warnColor_;
        /// Error background color
        CellColor   errColor_;
};   


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
