/** @file
 * Visitor to shorten default MonitorPointAbstime display from DATE:TIME to 
 * TIME only.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2013/11/19 03:41:12 $
 * $Id: DisplayTimeOnlyVisitor.h,v 1.2 2013/11/19 03:41:12 iws Exp $
 */
#ifndef CARMA_UI_RTD_DISPLAYTIMEONLYVISITOR_H
#define CARMA_UI_RTD_DISPLAYTIMEONLYVISITOR_H

#include "carma/ui/rtd/common/MonitorTableVisitor.h"

#include <set>

namespace carma {
namespace ui {
namespace rtd {

class MonitorTableVisitor;

/**
 * Visitor to display time only instead of full Date and Time.
 * Mimics the GoodIsGreenVisitor
 */
class DisplayTimeOnlyVisitor : public MonitorTableVisitor {
public:

    DisplayTimeOnlyVisitor( );

    virtual ~DisplayTimeOnlyVisitor( );

    void addToEffectedSet( const monitor::MonitorPoint &mp );

    void postprocessMonitorCell(
        const monitor::MonitorPoint & mp,
        MonitorCell &                 monitorCell ) const;

private:

    ::std::set< ::std::string > effected_;

};

typedef boost::shared_ptr<DisplayTimeOnlyVisitor> DisplayTimeOnlyVisitorPtr;

}}} // End namespace carma::ui::rtd
#endif
