#ifndef CARMA_UI_RTD_MONITORTABLEVISITOR_H
#define CARMA_UI_RTD_MONITORTABLEVISITOR_H

#include <string>
#include <boost/shared_ptr.hpp>


namespace carma {

namespace monitor {

class MonitorPoint;

}  // namespace carma::monitor


namespace ui {
namespace rtd { 


class MonitorCell;


class MonitorTableVisitor {
    public:
        virtual ~MonitorTableVisitor( );

        virtual bool acceptMonitorPoint(
            const monitor::MonitorPoint & mp ) const;
        
        virtual int calcCellWidth( const monitor::MonitorPoint & mp ) const;
        
        virtual void postprocessMonitorCell(
            const monitor::MonitorPoint & mp,
            MonitorCell &                 monitorCell ) const;
        
        virtual ::std::string generateRowLabel(
            const monitor::MonitorPoint & mp,
            bool                          showUnits ) const;
        
    protected:
        explicit MonitorTableVisitor( );
};

typedef boost::shared_ptr<MonitorTableVisitor> MonitorTableVisitorPtr;

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
