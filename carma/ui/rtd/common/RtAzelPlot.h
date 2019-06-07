#ifndef CARMA_UI_RTD_RTAZELPLOT_H
#define CARMA_UI_RTD_RTAZELPLOT_H


/*
 * @file
 *
 * Graphical display of current and requested azimuth and elevation, wind speed
 * and direction.
 *
 * @author Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <string>
#include <vector>

#include <carma/ui/rtd/common/RtDisplay.h>

#include <boost/shared_ptr.hpp>


namespace carma {
namespace ui {
namespace rtd {


class RtAzelPlot : public RtObject {
    public:
        typedef enum {
            AZEL_SOURCE_TYPE,
            RADEC_SOURCE_TYPE
        } SourceType;

        /**
         * Constructor.
         * There are no antennae when this is called. They must be
         * added individually.
         * @param sourceType the type of source: 0=AzEl, 1=RaDec
         * @param windSpeed wind speed in mph
         * @param windDir wind direction in degrees
         */
        RtAzelPlot( ::size_t     maxAntennas,
                    SourceType & sourceType,
                    double       windSpeed,
                    double       windDir );
        /**
         * Add another antenna to the display
         * @param legit flag indicating legitimate data
         * @param online flag indicating whether antenna is online
         * @param reqAz requested azimuth in degrees
         * @param reqEl requested elevation in degrees
         * @param actAz actual azimumth in degrees
         * @param actEl actual elevation in degrees
         *
         * Note these paramters must be pointers because the work
         * loop in the server code updates the values.
         */
        void addAnt( const bool *   legit, const bool *   online,
                     const double * reqAz, const double * reqEl,
                     const double * actAz, const double * actEl);

        /// Create the dynamic string representing this object
        virtual void update();
        /// Create the static description of this object and send it to stdout
        virtual void serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj);

        /**
         * Set a new wind speed value
         * @param windSpeed The wind speed in mph.
         */
        void setWindSpeed(double windSpeed);
        /**
         * Set a new direction from which the wind is blowing.
         * @param windSpeed The wind direction in degrees from north.
         */
        void setWindDirection(double windDirection);

    private:
        /**
         * Create a single source position from the source positions of all the antennae.
         * If the max and min of the requested position of all the online, legitimate,
         * antennae is greater than a degree, then source position is not legitimate.
         * If they do agree, compute a mid range value (max+min/2).
         */
        void formSourcePosition();

        class AntInfo {
            public:
                AntInfo( const bool *   legit, const bool *   online,
                         const double * reqAz, const double * reqEl,
                         const double * actAz, const double * actEl );

                bool legit( ) const;
                bool online( ) const;

                double reqAz( ) const;
                double reqEl( ) const;

                double actAz( ) const;
                double actEl( ) const;

            private:
                const bool *   legit_;
                const bool *   online_;
                const double * reqAz_;
                const double * reqEl_;
                const double * actAz_;
                const double * actEl_;
        };

        const ::size_t maxNumAnts_;

        /// Number of characters for each variable
        const int fieldWidth_;

        /// Flag to signify whether requested positions for all antennae agree
        char legitSourcePositionCode_;

        /// Source azimuth
        double sourceAz_;

        /// Source elevation
        double sourceEl_;

        /// An integer, 0=azel/idle, 1+ =other
        const SourceType & sourceType_;

        /// Wind speed in mph
        double windSpeed_;

        /// Wind direction in mph
        double windDir_;

        /// Pointer to final output sting for update
        ::std::string output_;

        /// Formatted text stream buffer
        ::std::ostringstream scratchOSS_;

        ::std::vector< AntInfo > actualAntInfos_;

        /// Counter for debugging operations
        int debugCount_;
};

typedef boost::shared_ptr<RtAzelPlot> RtAzelPlotPtr;

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
