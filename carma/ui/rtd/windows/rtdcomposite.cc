#include <vector>

#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/RtDisplay.h"
#include "carma/ui/rtd/windows/compositeFolders.h"
#include "carma/ui/rtd/windows/CompositeSubarrayDisplay.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;


namespace { // anonymous

carma::ui::rtd::RtFolderPtr folder[5];

void
addSubarrayFolders(CompositeSubarrayDisplay* display)
{

    const int saCount = ControlSubsystem::subarrayCount();
    display->cms().readNewest();
    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const int saIdx = saNo - 1;
        SubarrayStatus *saStatus = display->subarrayStatus(saIdx);
        folder[saIdx] = makeFolderForSubarray(saNo, display, saStatus);
        display->add(folder[saIdx]);
    }
}

void
replaceSubarrayFolders(CompositeSubarrayDisplay *display)
{
    const int saCount = ControlSubsystem::subarrayCount();
    display->cms().readNewest();
    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const int saIdx = saNo - 1;
        SubarrayStatus * saStatus = display->subarrayStatus(saIdx);

        RtFolderPtr oldObj = folder[saIdx];
        RtFolderPtr newObj = makeFolderForSubarray(saNo, display, saStatus);
        folder[saIdx] = newObj;

        display->replace(oldObj, newObj);
    }
}

CompositeSubarrayDisplay* getNewDisplay( void )
{
    typedef CompositeSubarrayDisplay CSD;
    CSD* display = new CSD("Array Composite", true);
    const string file = Program::getConfFile("rtd/help/composite.html");
    display->setSpecificHelpFromTextFile( "Array Composite Window Help", file);
    return display;
}

void
runDisplay() {
    CompositeSubarrayDisplay* display = getNewDisplay();

    addSubarrayFolders( display );

    bool needsReinitialization = false;
    while (display->serveData(needsReinitialization)) {
        const bool corrChanged = display->anyCorrMembershipHasChanged();
        const bool anyChanged = display->anyMembershipHasChanged();

        if (false) {
            std::ostringstream oss;
            oss << "serveData: corrChanged=" << corrChanged
                << " anyChanged=" << anyChanged;
            programLogInfoIfPossible(oss.str());
        }

        needsReinitialization = corrChanged || anyChanged;
        display->cms().readNewest();

        // HACK: replace the subarray folders on re-init
        if (needsReinitialization)
            replaceSubarrayFolders(display);
    }
}


} // namespace < anonymous >


int
Program::main( )
{
    {
        const string baseLogname = getLogname();
        setInstanceLogname( baseLogname + ".rtdcomposite" );
    }

    try {
        runDisplay();
        programLogInfoIfPossible( "RTDcomposite appears to exit cleanly" );
        return EXIT_SUCCESS;

    } catch ( const util::ErrorException& ee ) {
        ostringstream os;
        os << " rtdcomposite caught exception: "
            << ee.getMessage();
        programLogCriticalIfPossible( os.str() );
        return EXIT_FAILURE;
    } catch ( const std::exception& exc ) {
        ostringstream os;
        os << " rtdcomposite caught exception: "
            << exc.what();
        programLogCriticalIfPossible( os.str() );
        return EXIT_FAILURE;
    } catch ( ... ) {
        programLogCriticalIfPossible( "rtdcomposite caught unknown exception" );
        return EXIT_FAILURE;
    }

}

