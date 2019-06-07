#include "carma/antenna/sza/antenna/corba/MpFileHandler.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
MpFileHandler::MpFileHandler(std::string dir) : CarmaFileHandler(dir) 
{
  setCompletedFileList("/opt/szadata/eml/arcCarma/convertedMpFiles");
}

/**.......................................................................
 * Destructor.
 */
MpFileHandler::~MpFileHandler() {}
