#ifndef CARMA_UI_RTD_WINDOWS_COMPOSITEFOLDERS_H
#define CARMA_UI_RTD_WINDOWS_COMPOSITEFOLDERS_H
#include <vector>
#include <boost/shared_ptr.hpp>

namespace carma {
namespace ui {
namespace rtd {

class RtFolder;
class RtObject;
class RtBox;
class RtHBox;
class RtVBox;
class CompositeSubarrayDisplay;
class SubarrayStatus;

typedef boost::shared_ptr<RtFolder> RtFolderPtr;
typedef boost::shared_ptr<RtHBox> RtHBoxPtr;
typedef boost::shared_ptr<RtBox> RtBoxPtr;

RtFolderPtr
makeFolderForSubarray(const int saNo,
                      CompositeSubarrayDisplay * display,
                      const SubarrayStatus * saStatus);

RtHBoxPtr
makeSourceInfoBox ( const int saNo,
                    CompositeSubarrayDisplay * display,
                    const SubarrayStatus * saStatus);

RtBoxPtr
makeAntTableBox(const int saNo,
                CompositeSubarrayDisplay * display,
                const SubarrayStatus * saStatus);

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif 
