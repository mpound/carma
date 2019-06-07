#ifndef CARMA_UI_RTD_COMPOSITE_MONITOR_TABLE_VISITOR_H
#define CARMA_UI_RTD_COMPOSITE_MONITOR_TABLE_VISITOR_H

#include "carma/ui/rtd/common/MonitorTableVisitor.h"

#include <vector>

#include <boost/shared_ptr.hpp>

namespace carma {
namespace ui {
namespace rtd { 

/**
 * Composite design pattern implementation for MonitorTableVisitor interface.
 *
 * This class allows a user to define a single visitor composed of 
 * an arbitrary number of additional visitors all while still adhering to 
 * the visitor interface.
 */
class CompositeMonitorTableVisitor : public MonitorTableVisitor {
public:

    /**
     * Create an instance of a CompositeMonitorTableVisitor ahdering to
     * the MonitorTableVisitor interface.
     * @param visitors Vector containing composite visitors.
     */
    CompositeMonitorTableVisitor( 
        const std::vector<MonitorTableVisitorPtr > & visitors );

    /**
     * Destroy class instance.
     */
    ~CompositeMonitorTableVisitor( );

    /**
     * Accept a monitor point.
     * If not redefined, this method ORs containing visitor implementations 
     * (e.g. it's true if any containing visitors are true).
     * @param mp Monitor point to test for acceptance.
     */
    virtual bool acceptMonitorPoint( const monitor::MonitorPoint & mp ) const;

    /**
     * Calculate mp cell width.
     * If not redefined, this method returns the max of the contained visitors.
     * @param mp Monitor point to calculate cell width for.
     */
    virtual int calcCellWidth( const monitor::MonitorPoint & mp ) const;

    /**
     * Call postprocessMonitorCell for all contained visitors.
     * This implementation should NOT be redefined!  It is final in that 
     * any redefinition of it will effectively null and void this class's
     * main purpose.  Unfortunately, C++ does not contain a 'final' keyword
     * which would make this explicit and enforceable.
     * @param mp Monitor point associated with monitor cell.
     * @param monitorCell Cell to apply user defined criteria to.
     */
    void postprocessMonitorCell(
        const monitor::MonitorPoint & mp,
        MonitorCell &                 monitorCell ) const;

    /**
     * Generate a row label for the input monitor point.
     * If not redefined, this method returns the base class implementation
     * (e.g. MonitorTableVisitor::generateRowLabel) which in turn just uses
     * the monitor point short name.
     */
    virtual ::std::string generateRowLabel(
        const monitor::MonitorPoint & mp,
        bool                          showUnits ) const;

private:

    typedef ::std::vector< MonitorTableVisitorPtr > VisitorVector;

    VisitorVector visitors_;

}; // class CompositeMonitorTableVisitor

typedef boost::shared_ptr<CompositeMonitorTableVisitor> CompositeMonitorTableVisitorPtr;

} // namespace carma::ui::rtd
} // namespace carma::ui
} // namespace carma
#endif
