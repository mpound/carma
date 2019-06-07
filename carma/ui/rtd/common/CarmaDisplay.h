#ifndef CARMA_UI_RTD_CARMADISPLAY_H
#define CARMA_UI_RTD_CARMADISPLAY_H


/*
 * @file
 *
 * Class that defines all of the realtime windows for carma.
 *
 * @author Steve Scott 
 *
 * $CarmaCopyright$
 *
 */


#include <carma/ui/rtd/common/WindowList.h>
#include <carma/ui/rtd/common/RtDisplay.h>

#include <boost/shared_ptr.hpp>

namespace carma {
namespace ui {
namespace rtd { 


class RtMenu;
typedef boost::shared_ptr<RtMenu> RtMenuPtr;


/// Class to define the specific set of windows and code words for Carma
class CarmaWindows : public carma::ui::rtd::WindowList {
    public:
        virtual ~CarmaWindows( );

        virtual void load( );
};



class CarmaDisplay : public carma::ui::rtd::RtDisplay {
    public:
        /**
         * Constructor.
         * @param subtitle put on the title bar of the display; this is prefaced
         *                 by the system name
         * @param ut string for ut in time panel
         * @param lst string for lst in time panel
         * @param visibleTimePanel flag to make time panel visible
         */
        CarmaDisplay( const ::std::string & subtitle,
                      const char *          ut,
                      const char *          lst,
                      bool                  visibleTimePanel = true );

        /**
         * Constructor.
         * @param subtitle put on the title bar of the display but is prefaced
         *                 by the system name
         * @param visibleTimePanel flag to make time panel visible
         */
        CarmaDisplay( const ::std::string & subtitle,
                      bool                  visibleTimePanel = true );

        /**
         * Destructor
         */
        virtual ~CarmaDisplay( );

    private:
        RtMenuPtr makeMenu( );

        void init( const ::std::string& subtitle );
 };


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
