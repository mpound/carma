#include "carma/corba/Server.h"

#include "carma/correlator/transport/CorrDataRemapperControlImpl.h"
#include "carma/correlator/transport/CorrDataRemapperControl_skel.h"
#include "carma/correlator/transport/CorrDataRemapperControl_skel_tie.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/Program.h"

#include "carma/util/ExceptionUtils.h"
#include "carma/util/Logger.h"
#include "carma/util/ProcessMonitorClient.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include "carma/util/Orb.h"

#include <memory>
#include <iostream>

using namespace carma;
using namespace carma::corba;
using namespace carma::correlator;
using namespace carma::util;
using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "imrsrc",      "",   "s", USAGE "IMR from which to get correlator data. Generally only set when debugging."},
  { "imrdest",     "",   "s", USAGE "IMR on which to publish correlator data (and our DO). Generally only set when debugging."},
  { "produce",     "t",  "b", USAGE "True to publish remapped data.  False to read it from the republished channels"},
  { "noPCS",       "f",  "b", USAGE "True to not use Program corba::Server instance. Should be true if imrdest is non blank. Note if if noPCS is true, then no monitor system will be written." },
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {}

/**
 * @description
 * Provides control interface to the CorrDataRemapper object
 */
int Program::main()
{
  // Get log4cpp logger (aka Category)

  // szuautil shortcuts do not define @logger, so set it here
  // programmatically
  setInstanceLogname("carma.correlator.corrdataremapper");
  log4cpp::Category & log = getLogger();
  
  // Set logger priority for debug, if needed

  if(getDebugLevel() > 0) {
    log.setPriority(log4cpp::Priority::DEBUG);
  }
  
  try {

    // Get the 'produce' keyword.  
    //
    // If true, then the corrdataremapper will publish its control DO,
    // data will be grabbed from the correlator band notification
    // channels, and republished on the astroband notification
    // channels.
    //
    // If false, then data will be grabbed on the astroband
    // notification channels only, no control DO will be served, and
    // no republishing will happen

    bool produce = Program::getBoolParameter("produce");

    // check if an alternate orb has been requested. If so, then 
    // it will be impossible to write a monitor system because 
    // MonitorSystem requires the Program corba::Server to contact
    // the frameScriberPublisher.
    bool noPCS   = Program::getBoolParameter("noPCS");

    // We allow for data to be collected from one notification server,
    // and republished on another.  This is controlled by the imrSrc
    // and imrDest keywords.  If not specified, they default to
    // whatever was specified with the imr system keyword.

    std::string imr, imrSrc, imrDest;

    imr     = Program::getImrHostname();
    imrSrc  = imr;
    imrDest = imr;

    if(Program::parameterWasSpecified("imrsrc")) {
      imrSrc = Program::getParameter("imrsrc");
    }

    if(Program::parameterWasSpecified("imrdest")) {
      imrDest = Program::getParameter("imrdest");
    }

    // Instantiate the corrdataremapper object now

    {
        ostringstream os;
        os << "Instantiating CDRCI with imrSrc = " << imrSrc << " imrDest = " << imrDest << " produce = " << boolalpha << produce  << " noPCS = " << noPCS;
        const string foo = os.str();
        programLogNotice( foo );
        COUT( foo );
    }

    CorrDataRemapperControlImpl* impl = new CorrDataRemapperControlImpl(produce, imrSrc, imrDest, noPCS);
    COUT("Done Instantiating CDRCI");

#if 0
    impl->getDebugger().astroBandNo_ = 1;
    impl->getDebugger().sideband_    = CorrDataRemapperControlImpl::SB_USB;
    impl->getDebugger().inputNo1_    = 1;
    impl->getDebugger().inputNo2_    = 16;
    impl->getDebugger().debug_       = true;
#endif

    if(produce) {

      // Everything from here on is CORBA related

      // Initialize the ORB - the orbInit method parses any command 
      // line options for you - in particular, the imr keyword is parsed
      // and any additional parameters after the -- are appended to orb_init.
      // This is particularly important when a server is started by the IMR
      // as the IMR will append the -ORBServerId and -ORBserver_instance 
      // options to the command line.

        if ( noPCS )  {
          Orb orb;
          orb.allowRegistrationWithImr(true);
          orb.setName("testOrb");
          orb.setImrName(imrDest);

          if (!orbInit(&orb)) {
            log << log4cpp::Priority::ERROR
                << "Error initializing ORB - Make sure command line" 
                << " contains proper imr keyword.";
            return EXIT_FAILURE;
          } else {
            CARMA_CPTRACE(Trace::TRACEALL, "Orb successfully initialized.");
          }

          // Instruct CorbaUtils to create a 'persistent' POA with the
          // PERSISTENT, USER_ID and NO_IMPLICIT_ACTIVATION policies.

          PortableServer::POA_var poa = orb.getPOA("CorrDataRemapperPOA");

          if (CORBA::is_nil(poa)) {
            log << log4cpp::Priority::ERROR
                << "Error creating CorrDataRemapperPOA";
            return EXIT_FAILURE;
          } else {
            CARMA_CPTRACE(Trace::TRACEALL,
                  "Persistent CorrDataRemapperPOA successfully created.");
          }

          // Create a Device servant 

          POA_carma::correlator::CorrDataRemapperControl_tie<CorrDataRemapperControlImpl> * servant =
                new POA_carma::correlator::CorrDataRemapperControl_tie<CorrDataRemapperControlImpl>(impl, poa);

          CARMA_CPTRACE(Trace::TRACEALL, "Servant was successfully created");

          // Create an object id (required when using the USER_ID policy).

          PortableServer::ObjectId_var oid =
                PortableServer::string_to_ObjectId("carma::correlator::CorrDataRemapperControl");

          // Activate (incarnate servant).

          poa->activate_object_with_id(oid, servant);

          // Not sure if this is still true with non-RefCountGuard
          // servants...  The reference count on the servant is now 2!  1
          // for the creation of the servant and a second ref count due to
          // the activation of the servant.

          // Publish it on the nameserver - note here that _this does not
          // implicitly activate the servant.  The servant was explicitly
          // activated within the constructor.

          orb.addObject(CORRDATAREMAPPERCONTROL_NAME, servant->_this());

          // Servant is incarnated and published at this stage.

          log << log4cpp::Priority::INFO
              << "Device IOR successfully published on nameserver as '"
              << CORRDATAREMAPPERCONTROL_NAME << "'";

          // Create a ProcessMonitorClient to ping the process monitor.
          // This is done globally for most procs but must be special 
          // cased here.
          ::std::auto_ptr< ProcessMonitorClient > pmc;
          try {
            pmc = auto_ptr< ProcessMonitorClient >( 
                new ProcessMonitorClient( &orb ) );
          } catch (...) {
            // Log bug stifle and continue on so as not to interfere...
            // If on the RTS, process will display STOPPED.
            programLogErrorIfPossible( getStringForCaught() );
          }

          // Block on orb forever.  The runOrb command takes care of POA
          // activation.

          orb.run();

          // If we get here, the ORB was shutdown via the IMR (not ^C).  
          // At this stage, the reference count on the servant is 1 again as the 
          // Persistent POA and Active Object Map have been destroyed and hence 
          // the POA doesn't need a reference to the servant any longer.
            
          // Delete the servant - NEVER CALL DELETE ON A REFERENCE COUNTED
          // IMPL!  ALWAYS CALL _REMOVE_REF() AND THEN ONLY CALL IT ONCE!!!

          servant->_remove_ref();
        } // if ( noPSC )
        else {
            // Use the normal Program-supplied corba Servant code,
            // which also instantiates a ProcessMonitorClient.
            //
            // This is the default branch of this program.
            //
            carma::corba::Server & server = Program::getCorbaServer();
            server.addServant< POA_carma::correlator::CorrDataRemapperControl_tie >
                ( *impl, CORRDATAREMAPPERCONTROL_NAME );
            server.run( false );
        }

    } else {
     // If we are not producing, don't register a DO, don't start an ORB
      sza::util::Runnable::blockForever();
    }

  } catch (sza::util::Exception& err) {
    COUT(err.what());
    log << log4cpp::Priority::ERROR << err.what();
    return EXIT_FAILURE;
  } catch (CORBA::Exception &ex) {
    log << log4cpp::Priority::ERROR << ex;
    return EXIT_FAILURE;
  } catch (carma::util::ErrorException &eex) {
    log << log4cpp::Priority::ERROR << eex.getErrorMessage();
    return EXIT_FAILURE;
  } catch (...) {
    log << log4cpp::Priority::ERROR
        << "Unknown exception caught in Program::main()";
    return EXIT_FAILURE;
  }
  // Should "never" get here, so if we do, return failure
  return EXIT_FAILURE;
}


    



