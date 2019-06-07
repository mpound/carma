#include "carma/szautil/Directives.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/Corba.h"
#include "carma/antenna/sza/antenna/corba/AntennaProxy.h"

#include "carma/corba/Server.h"
#include "carma/util/Program.h"
#include "carma/util/ExceptionUtils.h"
#endif

#include "carma/antenna/sza/antenna/control/AntennaControl.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/AntennaCorba.h"

#include "carma/szautil/Exception.h"

#if DIR_HAVE_CARMA
using namespace carma::util;
#endif

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
AntennaCorba::AntennaCorba(sza::antenna::control::AntennaControl* parent) : 
  SzaTask(), sza::util::GenericTask<AntennaControlMsg>::GenericTask(),
#if DIR_USE_ANT_CORBA
  // ADB: If you don't want to use the Program Server instance, feed in argc 
  // and argv or pass in a Server reference to the ctor. 
  server_( carma::util::Program::getProgram().getCorbaServer() ),
#endif
  parent_(parent)
{
#if DIR_USE_ANT_CORBA

  antennaProxy_        = 0;
  monitorPointHandler_ = 0;
  antennaProxy_        = new AntennaProxy(parent_->parent_);

  if(!antennaProxy_) {
    ThrowError("Unable to allocate antennaProxy_ object");
  }

  // ORB has to be initialized before we can allocate the monitor subsystem points

  initOrb();
#endif
}

void AntennaCorba::initialize()
{
  monitorPointHandler_ = new CarmaMonitorPointHandler(this);

  if(monitorPointHandler_ == 0) {
    ThrowError("Unable to allocate monitorPointHandler");
  }
}

/**.......................................................................
 * Destructor.
 */
AntennaCorba::~AntennaCorba() 
{
#if DIR_USE_ANT_CORBA

  if(antennaProxy_) {
    delete antennaProxy_;
    antennaProxy_ = 0;
  }

  if(monitorPointHandler_) {
    delete monitorPointHandler_;
    monitorPointHandler_ = 0;
  }

  shutdownORB(true, true, false);

#endif
}

#if DIR_USE_ANT_CORBA
//-----------------------------------------------------------------------
// CORBA service methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Initialize this server process ORB
 */
void AntennaCorba::initOrb()
{
  // Call the proxy servant's registration method
  antennaProxy_->registerObject(parent_->parent_->objectName(), server_);
} 

/**.......................................................................
 * Shutdown this server process ORB
 */
void AntennaCorba::shutdownORB(bool deactivate, bool etherealize, bool wait)
{
  server_.stop();
}

/**.......................................................................
 * Run the comms event loop.
 */
void AntennaCorba::run()
{
  const bool inSeparateThread( false );
  server_.run( inSeparateThread ); 
} 
    
#endif

void AntennaCorba::
writeCarmaSeqNo(unsigned long seq, 
		sza::util::GenericTaskMsg::CarmaSeqNoType type, 
		bool success)
{
  monitorPointHandler_->stageCarmaSeqNo(seq, type, success);
}

void AntennaCorba::writeCarmaMonitorPoints()
{
  monitorPointHandler_->stageCommonMonitorPoints();
  monitorPointHandler_->stageSpecificMonitorPoints();
  monitorPointHandler_->stageSzaAllMonitorPoints();
  monitorPointHandler_->writeMonitorPoints();
}

SzaShareCorba* AntennaCorba::getShare()
{
  return (SzaShareCorba*)parent_->getShare();
}

unsigned int AntennaCorba::getCarmaAntennaIndex()
{
  return parent_->parent_->getAnt()->getCarmaAntennaIndex();
}

AntNum* AntennaCorba::getAnt()
{
  return parent_->parent_->getAnt();
}

void AntennaCorba::initializeAntenna()
{
  antennaProxy_->initializeAntenna();
}
