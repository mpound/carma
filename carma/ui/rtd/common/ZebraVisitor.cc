#include "carma/ui/rtd/common/ZebraVisitor.h"
using namespace carma::ui::rtd;

#include "carma/monitor/MonitorPointIterator.h"
using namespace carma::monitor;

ZebraVisitor::ZebraVisitor(const CellColor &goodColor)
    : ColorVisitor::ColorVisitor(goodColor)
{
    // intentionally left empty
}

ZebraVisitor::~ZebraVisitor()
{
    // intentionally left empty
}

void ZebraVisitor::addMonitorContainer(const MonitorContainer &c, const int mod)
{
    MonitorPointIterator it(c, 1);
    int zebra = 0;

    while (it++) {
        if (++zebra % mod) {
            this->addToEffectedSet(it.getMonitorPoint());
        }
    }
}

void ZebraVisitor::addMonitorContainer(MonitorContainer *c, const int mod)
{
    this->addMonitorContainer(*c, mod);
}
