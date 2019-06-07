#include "carma/antenna/sza/antenna/corba/VisFileHandler.h"

#include "carma/szautil/Exception.h"

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
VisFileHandler::VisFileHandler(std::string dir, bool wideband) : CarmaFileHandler(dir) 
{
  wideband_ = wideband;

  if(wideband)
    setCompletedFileList("/opt/szadata/eml/arcCarma/wbcorrel/convertedFiles");
  else
    setCompletedFileList("/opt/szadata/eml/arcCarma/slcorrel/convertedFiles");
    
  generateFileList();
}

/**.......................................................................
 * Destructor.
 */
VisFileHandler::~VisFileHandler() {}

void VisFileHandler::setWideband(bool wideband)
{
  wideband_ = wideband;
}

bool VisFileHandler::isFileType(sza::util::String& str)
{
  if(wideband_) {
    return str.contains("wbVisBrickData_")   && !str.contains(".write");
  } else {
    return str.contains("slVisBrickData_") && !str.contains(".write");
  }
}
