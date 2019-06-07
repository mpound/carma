/**
 * Carma SignalPathMapper host process.
 * 
 * <dl><dt><b>Author</b></dt><dd>Erik Leitch </dl>
 */

#include "carma/corba/Server.h"
#include "carma/signalpath/SignalPathMapperControlImpl.h"
#include "carma/signalpath/SignalPathMapperControl_skel.h"
#include "carma/signalpath/SignalPathMapperControl_skel_tie.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/Thread.h"

#include "carma/util/Logger.h"
#include "carma/util/Orb.h"
#include "carma/util/Trace.h"

using namespace carma::corba;
using namespace carma::signalpath;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "doname",  "",       "s", USAGE "If specified, use this name to register with the name service instead of the default name"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {}

struct ProgInfo {
  Program* prog_;
  pthread_t id_;
  std::string doName_;
};

static void runDefaultOrb();
static void runSpmControlDoOrb(ProgInfo* progInfo);

#if 0
static THREAD_START(startDefaultOrb);
#endif

static THREAD_START(startSpmControlDoOrb);

/**.......................................................................
 * Runs the default ORB
 */
void runDefaultOrb()
{
  const bool dontRunInSeparateThread = false;
  Program::getProgram().getCorbaServer().run( dontRunInSeparateThread );
}

#if 0
/**.......................................................................
 * Thread which starts the default ORB, used by the monitor system
 */
THREAD_START(startDefaultOrb)
{
  // Get a handle to the object which created us

  ProgInfo* progInfo = (ProgInfo*)arg;

  try {

    runDefaultOrb();

  } catch(...) {
  }

  return 0;
}
#endif

/**.......................................................................
 * @description
 * Provides control interface to the SignalPathMapper object
 */
THREAD_START(startSpmControlDoOrb)
{
  // Get a handle to the object which created us

  ProgInfo* progInfo = (ProgInfo*) arg;

  try {

    runSpmControlDoOrb(progInfo);

  } catch(...) {
  }

  return 0;
}

/**.......................................................................
 * Function which actually gets executed by the startup function above
 */
void runSpmControlDoOrb(ProgInfo* progInfo)
{
  // Get log4cpp logger (aka Category)

  log4cpp::Category& log = progInfo->prog_->getLogger();

  // Instantiate the control object that will do all the work

  SignalPathMapperControlImpl* impl = new SignalPathMapperControlImpl();

  // Now construct a separate Orb for the control DO.  This is to
  // isolate the orb used by the monitor system from the orb used by
  // the control DO.  Otherwise threads used to write the monitor
  // system can get hijacked in CorbaUtils::doWork() to execute
  // another control DO method, and deadlock if that thread is
  // already holding a mutex lock used by the control DO method.

  Orb orb;
  orb.allowRegistrationWithImr(false); // Don't register with the IMR.
				       // The default ORB will do that
  orb.setName("SignalPathControlOrb");
  orb.setImrName(progInfo->prog_->getImrHostname());

  if(!progInfo->prog_->orbInit(&orb)) {
    ThrowError("Error initializing ORB - Make sure command line" 
	       << " contains proper imr keyword.");
  }

  CARMA_CPTRACE(Trace::TRACEALL, "Orb successfully initialized.");

  //------------------------------------------------------------
  // Instruct CorbaUtils to create a 'persistent' POA with the
  // PERSISTENT, USER_ID and NO_IMPLICIT_ACTIVATION policies.
  //------------------------------------------------------------

  PortableServer::POA_var poa = orb.getPOA("SignalPathMapperPOA");

  if(CORBA::is_nil(poa)) {
    ThrowCarmaError("Unable to create the SignalPathMapperPOA");
  } 

  CARMA_CPTRACE(Trace::TRACEALL, "Persistent SignalPathMapperPOA successfully created.");

  //------------------------------------------------------------
  // Create a Device servant 
  //------------------------------------------------------------

  POA_carma::signalpath::SignalPathMapperControl_tie<SignalPathMapperControlImpl>* servant =
    new POA_carma::signalpath::SignalPathMapperControl_tie<SignalPathMapperControlImpl>(impl, poa);

  CARMA_CPTRACE(Trace::TRACEALL, "Servant was successfully created");

  // Create an object id (required when using the USER_ID policy).

  PortableServer::ObjectId_var oid =
    PortableServer::string_to_ObjectId("carma::signalpath::SignalPathMapperControl");

  // Activate (incarnate servant).

  poa->activate_object_with_id(oid, servant);

  //------------------------------------------------------------
  // Publish it on the nameserver - note here that _this does not
  // implicitly activate the servant.  The servant was explicitly
  // activated within the constructor.
  //------------------------------------------------------------

  orb.addObject(progInfo->doName_, servant->_this());

  // Servant is incarnated and published at this stage.

  log << log4cpp::Priority::INFO
      << "Device IOR successfully published on nameserver as '"
      << progInfo->doName_ << "'";

  // Block on orb forever.  The runOrb command takes care of POA
  // activation.

  COUT("About to call run()");
  orb.run("SignalPathMapperPOA");

  // If we get here, the ORB was shutdown via the IMR (not ^C).  
  // At this stage, the reference count on the servant is 1 again as the 
  // Persistent POA and Active Object Map have been destroyed and hence 
  // the POA doesn't need a reference to the servant any longer.
        
  // Delete the servant - NEVER CALL DELETE ON A REFERENCE COUNTED
  // IMPL!  ALWAYS CALL _REMOVE_REF() AND THEN ONLY CALL IT ONCE!!!

  servant->_remove_ref();
}

/**.......................................................................
 * @description
 *
 * Provides control interface to the SignalPathMapper object.  
 *
 * Main thread instantiates and runs the default ORB, which is used by
 * the monitor system, and spawns a thread that instantiates the
 * control DO servant.
 */
int Program::main()
{
  // Get log4cpp logger (aka Category)
  
  log4cpp::Category& log = getLogger();
  
  // Set logger priority for debug, if needed

  if(getDebugLevel() > 0) {
    log.setPriority(log4cpp::Priority::DEBUG);
  }
  
  // Create a handle to the thread which will run the SPM control DO orb

  pthread_t spmControlDoOrbId;

  try {

    // Attempt to spawn the thread which will initialize and run the
    // default orb

    ProgInfo info;
    info.prog_ = this;
    info.id_   = pthread_self();
    
    if(Program::parameterWasSpecified("doname")) {
      info.doName_ = Program::getParameter("doname");
    } else {
      info.doName_ = SIGNALPATHMAPPERCONTROL_NAME;
    }

    if(pthread_create(&spmControlDoOrbId, NULL, &startSpmControlDoOrb, &info) != 0) {
      ThrowCarmaError("Unable to create default Orb thread");
    }

    // Finally, run the default orb

    runDefaultOrb();

    // Cancel the SPM control DO thread too

    pthread_cancel(spmControlDoOrbId);
    return EXIT_SUCCESS;

  } catch (sza::util::Exception& err) {
    log << log4cpp::Priority::ERROR
	<< err.what();
    COUT(err.what());
  } catch (CORBA::Exception &ex) {
    log << log4cpp::Priority::ERROR
	<< ex;
    COUT("CORBA exception");
  } catch (carma::util::ErrorException &eex) {
    log << log4cpp::Priority::ERROR
	<< eex.getErrorMessage();
    COUT(eex.getErrorMessage());
  } catch (...) {
    log << log4cpp::Priority::ERROR
	<< "Unknown exception caught in Program::main()";
    COUT("Unknown exception");
  }

  // Cancel the SPM control DO thread too

  pthread_cancel(spmControlDoOrbId);
  return EXIT_FAILURE;
}
