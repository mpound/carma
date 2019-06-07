#include "carma/corba/Client.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Orb.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iostream>
#include <fstream>

#include <pthread.h>

using namespace std;
using namespace carma::util;

/**.......................................................................
 * Constructor.
 */
Orb::Orb() 
{
  orb_ = 0;

  if(pthread_mutex_init(&notifyChannelGuard_, NULL)) {
    ThrowCarmaError("pthread_mutex_init()");
  }

  if(pthread_mutex_init(&proxyPushSupplierMapGuard_, NULL)) {
    ThrowCarmaError("pthread_mutex_init()");
  }

  if(pthread_mutex_init(&proxyConsumerMapGuard_, NULL)) {
    ThrowCarmaError("pthread_mutex_init()");
  }

  maxRetries_ = 10;
  allowRegistrationWithImr_ = true;
}

/**.......................................................................
 * Destructor.
 */
Orb::~Orb() 
{
  destroy();
}

/**.......................................................................
 * Destructor method for CORBA
 */
bool Orb::destroy()
{
  if(!isNull()) {

    try {
      orb_->destroy();
      orb_ = 0;
      return true;
    } catch(const CORBA::Exception &err) {
      programLogErrorIfPossible("Orb::destroy - Failed ORB destruction");
      return false;
    }

  }

  // Already NULL -- just return true

  return true;
}

/**.......................................................................
 * Check if our orb has been allocated yet
 */
bool Orb::isNull()
{
  return CORBA::is_nil(orb_);
}

/**.......................................................................
 * Throw an exception if the orb is NULL
 */
void Orb::throwIfNull()
{
  if(isNull()) {
    throw CARMA_EXCEPTION(IllegalArgumentException,
			  "Called with null orb!" );
  }
}

/**.......................................................................
 * Shutdown the ORB
 */
bool Orb::shutdown(bool waitForCompletion) 
{
  if(!isNull()) {
    try {
      orb_->shutdown(waitForCompletion);
      return true;
    } catch ( const CORBA::Exception &err ) {
      programLogErrorIfPossible("Orb::shutdown - Failed ORB shutdown" );
      return false;
    }
  }

  // Already NULL -- return true

  return true;
}

/**.......................................................................
 * Set up this orb
 */
bool Orb::set(int argc, char** argv, const string & imrhost)
{
  std::string imrHost = imrhost;

  if(isNull()) {

    try {
      
      // Make a copy of argc and argv since some routines play with them...

      std::string lastArg = "";
      std::string currArg = "";

      for(int i=0; i < argc; i++) {
	currArg = argv[i];

	// If we are not allowing registration with the IMR, replace -ORBUseImr 1 with -ORBUseImr 0

	if(lastArg == "-ORBUseImr" && currArg == "1" && !allowRegistrationWithImr_)
	  currArg = "0";

	lArgvVec_.push_back(currArg);
	lastArg = argv[i];
      }

      // Substitute an explicit IMR if one was specified

      if(imrhost != "-" && hasUniqueImrName_)
	imrHost = getImrName();

      if(imrHost != "-") {

        // Copy args but also tack on -ORBDefaultInitRef.

        lArgvVec_.push_back( "-ORBDefaultInitRef" );
        string defaultInitRef = "corbaloc::" + imrHost;

        if(imrHost.find(":") == string::npos) 
	  defaultInitRef += ":20000";

        lArgvVec_.push_back(defaultInitRef);
      } 

      // Initialize the ORB

      int lArgc = lArgvVec_.size();
      char* lArgv[lArgc];

      for(int i=0; i < lArgc; i++) {
        lArgv[i] = const_cast<char*>(lArgvVec_.at(i).c_str());
      }

      orb_ = CORBA::ORB_init(lArgc, lArgv, getName().c_str());

      if(isNull()) {
        CARMA_CPTRACE(carma::util::Trace::TRACE3,
		      "Orb::set() - Orb is nil");
        return false;
      }

      carma::corba::Client::applyTimeoutPolicies( orb_ );

    } catch (const CORBA::Exception &ex) {
      CARMA_CPTRACE(carma::util::Trace::TRACE3,
		    "Orb::set() - Can't get ORB: " << ex);
      // Rethrow to a higher context...
      throw;
    }
  }

  return true;
}

/**.......................................................................
 * Obtain Root POA
 */
PortableServer::POA_var Orb::getRootPOA() 
{
  return resolveInit<PortableServer::POA>("RootPOA");
}

/**.......................................................................
 *
 */
string Orb::getPersistentPOAName(const string& poaName)
{
  if(poaName == "RootPOA")
    return poaName;

  // If we're using TAO, look for ORBServerId on the command line and use
  // that, if it doesn't exist, just use the input name.

  string orbServerId  = poaName;
  const int extraArgc = Program::getExtraArgc();
  char** extraArgv    = Program::getExtraArgv();

  for(int arg=0; arg < extraArgc; arg++) {
    if(string(extraArgv[arg]) == "-ORBServerId" && (arg+1 < extraArgc)) {
      orbServerId = string(extraArgv[arg+1]);
      break;
    }
  }

  return orbServerId;
}

/**.......................................................................
 *
 */
PortableServer::POA_var Orb::getPOA(const string & poaNameString)
{    
  PortableServer::POA_var myPOA;

  // obtain Root POA

  PortableServer::POA_var rootPOA = getRootPOA();
    
  if(poaNameString == "RootPOA") 
    return PortableServer::POA::_duplicate(rootPOA);

  const string poaString = getPersistentPOAName(poaNameString);

  programLogInfoIfPossible( "Orb::getPOA() - Using " +
			    poaString + " for persistent POA name." );
    
  try {

    // Try to find the specified POA in the rootPOA

    myPOA = rootPOA->find_POA(poaString.c_str(), true);

  } catch (const PortableServer::POA::AdapterNonExistent&) {

    // If the POA does not exist, then create it

    PortableServer::POAManager_var manager = rootPOA->the_POAManager();

    // Define policy list ... these are standard options for having a
    // POA that will support persistent objects

    CORBA::PolicyList policyList;

    policyList.length(3);

    policyList[0] = 
      rootPOA->create_lifespan_policy( PortableServer::PERSISTENT );

    policyList[1] = 
      rootPOA->create_id_assignment_policy( PortableServer::USER_ID );

    policyList[2] = 
      rootPOA->create_implicit_activation_policy(PortableServer::NO_IMPLICIT_ACTIVATION);

    myPOA = rootPOA->create_POA(poaString.c_str(), manager, policyList);

    // TODO destroy the policy list
  }

  return PortableServer::POA::_duplicate(myPOA);
}

/**.......................................................................
 *
 */
PortableServer::POAManager_var Orb::getPOAManager(const string & poaString) 
{
  // Resolve POA

  PortableServer::POA_var myPOA = getPOA(poaString);
  
  // Get a reference to the POA manager

  PortableServer::POAManager_var manager = myPOA->the_POAManager();

  return manager;
}

/**.......................................................................
 * Register an object.  Defaults kind to empty string.
 */
void Orb::addObject(const string& hierarchicalName, CORBA::Object_ptr objectPtr) 
{
  addObject(hierarchicalName, "", objectPtr);
}

/**.......................................................................
 * Register an object
 */
void Orb::addObject(const string& hierarchicalName, const string& kind, CORBA::Object_ptr objectPtr) 
{
  // Register object with nameserver

  rebind(hierarchicalName, kind, objectPtr);
}

/**.......................................................................
 * Remove an object from the CORBA hierarchy
 */
void Orb::removeObject(const string& hierarchicalName) 
{
  unbind(hierarchicalName);
}

/**.......................................................................
 * Blocking run method for this object
 */
void Orb::run(const string& poaString) 
{
  // Resolve and activate POA

  activatePOA(poaString);

  //  Now enter CORBA event loop

  orb_->run(); 
}

/**.......................................................................
 * Non-blocking work method
 */
void Orb::work() 
{
  if(!isNull() && workPending()) {
    performWork();
  }
}

/**.......................................................................
 * Return true if there is work to be done on this orb
 */
bool Orb::workPending()
{
  return orb_->work_pending();
}

/**.......................................................................
 * Perform the next unit of work on the orb
 */
void Orb::performWork()
{
  orb_->perform_work();
}

/**.......................................................................
 *
 */
void Orb::activatePOA(const string& poaString) 
{
  PortableServer::POAManager_var manager = getPOAManager(poaString);
  manager->activate();
}

/**.......................................................................
 *
 */
void Orb::deactivatePOA(bool etherealize, bool wait, 
			const string & poaString) 
{
  PortableServer::POAManager_var manager = getPOAManager(poaString);
  manager->deactivate(etherealize, wait);
}

/**.......................................................................
 *
 */
void Orb::holdPOA(bool wait, const string & poaString) 
{
  PortableServer::POAManager_var manager = getPOAManager(poaString);
  manager->hold_requests(wait);
}

/**.......................................................................
 *
 */
void Orb::discardPOA(bool wait, const string & poaString) 
{
  PortableServer::POAManager_var manager = getPOAManager(poaString);
  manager->discard_requests(wait);
}

/**.......................................................................
 * Resolve an object name
 */
CORBA::Object_var Orb::resolve(const string& hierarchicalName, const string& kind) 
{
  // first check to make sure there is no ends

  if(hierarchicalName.find( '\0' ) != string::npos ) {
    ThrowCarmaError("HierarchicalName contains NULL characters "
		    "(possibly caused by ends)");
  }

  // Name consists of all naming contexts plus the actual name of the object

  CosNaming::Name name;

  const vector<string> ids = getHierarchyIds(hierarchicalName);
  const int idCount        = ids.size();
  
  name.length(idCount);
  
  for(int i=0; i < idCount; i++) {
    name[i].id   = CORBA::string_dup(ids[i].c_str());
    name[i].kind = CORBA::string_dup(kind.c_str());
  }
    
  // Now resolve the hierarchy of names. This will throw an exception
  // if the NameService fails.  Most importantly it throws exceptions
  // before ->resolve( name ) is called which would result in a
  // segfault.

  CosNaming::NamingContext_var nc = getRootNamingContext();

  // Make sure this guy is legit

  if(CORBA::is_nil(nc))
    ThrowCarmaError("Naming Context is nil!");
        
  CORBA::Object_var objectVar = nc->resolve(name);

  // Relinquish ownership of objectVar

  return objectVar._retn();  
}

/**.......................................................................
 *
 */
CORBA::Object_var Orb::resolveInit(const char* idName) 
{
  throwIfNull();

  CORBA::Object_var objectVar = orb_->resolve_initial_references(idName);

  if(CORBA::is_nil(objectVar)) {
    programLogErrorIfPossible("Orb::resolveInit - objVar is nil!");
    ThrowCarmaError("Orb::resolveInit() - Nil object returned." );
  }
  
  // Relinquish ownership of objectVar

  return objectVar._retn();  
}


/**.......................................................................
 * Resolve an object reference
 */
CORBA::Object_var Orb::resolveName(const string& hierarchicalName, const string& kind) 
{
  // resolve object name from NameService - this throws
  // exceptions like crazy!
	
  CORBA::Object_var objectVar = resolve(hierarchicalName, kind);
	
  if(CORBA::is_nil(objectVar)) {
    ThrowCarmaError("Orb::resolveName() - Unabled to resolve " 
		    << hierarchicalName << ", " << kind);
  }

  return objectVar._retn( );  // Relinquish ownership of objectVar
}

/**.......................................................................
 * Wait to resolve an object reference
 */
CORBA::Object_var Orb::waitToResolveName( const string& hierarchicalName, const string& kind) 
{
  CORBA::Object_var objectVar;
  bool resolvedIt = false;
    
  while(!resolvedIt) {

    try {

      objectVar = resolve(hierarchicalName, kind);
            
      if(!CORBA::is_nil(objectVar)) {
	if(!objectVar->_non_existent())
	  resolvedIt = true;
      }             

    } catch(const CORBA::SystemException& e) {
      cerr    << "DEBUG: "
	      << "(Orb::waitToResolveName): "
	      << "resolving "
	      << "\"" << hierarchicalName << ", "
	      << kind
	      << "\" "
	      << "caught: "
	      << e
	      << endl;
            
      // Just stifle the exception and try again

    } catch ( const CORBA::Exception & e ) {
      cerr    << "DEBUG: "
	      << "(Orb::waitToResolveName): "
	      << "resolving "
	      << "\"" << hierarchicalName << ", "
	      << kind
	      << "\" "
	      << "failed: "
	      << e
	      << endl;
            
      throw;  // Rethrow this exception to a higher context
    } catch ( ... ) {
      cerr    << "DEBUG: "
	      << "(Orb::waitToResolveName): "
	      << "resolving "
	      << "\"" << hierarchicalName << ", "
	      << kind
	      << "\" "
	      << "failed: "
	      << "???"
	      << endl;
            
      throw;  // Rethrow this exception to a higher context
    }

    if(!resolvedIt)
      ::sleep(5);
  }

  // Relinquish ownership of typedVar

  return objectVar._retn(); 
}


/**.......................................................................
 * Resolve an object reference from a string
 */
CORBA::Object_var Orb::resolveFromString(const string & objectString ) 
{
  // Make sure orb is not null.

  throwIfNull();

  // convert stringified object to CORBA object

  CORBA::Object_var objectVar = orb_->string_to_object( objectString.c_str( ) );
	
  if(CORBA::is_nil(objectVar)) {
    ThrowCarmaError("Orb::resolveFromString() - string_to_object "
		    "returned nil reference !");
  }

  // Relinquish ownership of objectVar

  return objectVar._retn();  
}


/**.......................................................................
 * Resolve an object name from a file
 */
CORBA::Object_var Orb::resolveFromFile(const string& fileName, const string& directory ) 
{
  string objectString;

  // Combine file name with directory location

  const string fileString = directory + "/" + fileName;
  std::ifstream inFile(fileString.c_str());

  // Obtain stringified object from fileString

  inFile >> objectString;

  return resolveFromString(objectString);
}

/**.......................................................................
 * Return a vector of hierarchy strings
 */
vector<string> Orb::getHierarchyIds(const string& hierarchicalName) 
{
  // ids are separated by '.'
  // e.g. "carma.ovro1.antenna.cryocompressor" has 3 context ids
  // (carma, ovro1, antenna) and 1 leaf object id (cryocompressor).

  vector<string> ids;

  string::size_type nextIdBeginPos = 0;

  while(true) {

    const string::size_type nextIdEndPos = hierarchicalName.find('.', nextIdBeginPos);
        
    if(nextIdEndPos == string::npos) {
      ids.push_back(hierarchicalName.substr(nextIdBeginPos));
      break;
    }
        
    const string::size_type nextIdLength = nextIdEndPos - nextIdBeginPos;
    ids.push_back(hierarchicalName.substr(nextIdBeginPos, nextIdLength));
    nextIdBeginPos = nextIdEndPos + 1;
  }

  return ids;
}

void Orb::rebind(const string& hierarchicalName, const string& kind, CORBA::Object_ptr objectPtr)
{
  vector<string> ids = getHierarchyIds(hierarchicalName);
    
  if(ids.empty())
    ThrowCarmaError("hierarchicalName decomposed to an empty list of ids");

  const string leafObjectId = ids.back();
  ids.pop_back();
  
  // Get naming context for our object

  CosNaming::NamingContext_var objectNC = getNamingContext(ids, kind);
  CosNaming::Name leafObjectName;
    
  leafObjectName.length(1);
  leafObjectName[0].id   = CORBA::string_dup(leafObjectId.c_str());
  leafObjectName[0].kind = CORBA::string_dup(kind.c_str());

  // Rebind the object to the name within the naming context

  objectNC->rebind(leafObjectName, objectPtr);
}


void Orb::rebind(const string& hierarchicalName, CORBA::Object_ptr objectPtr)
{
  rebind(hierarchicalName, "", objectPtr);
}

void Orb::bind(const string& hierarchicalName, const string& kind, CORBA::Object_ptr objectPtr) 
{
  vector<string> ids = getHierarchyIds(hierarchicalName);

  if(ids.empty())
    ThrowCarmaError("hierarchicalName decomposed to an empty list of ids");

  const string leafObjectId = ids.back();
  ids.pop_back();
    
  // Get naming context for our object

  CosNaming::NamingContext_var objectNC = getNamingContext(ids);
  CosNaming::Name leafObjectName;
    
  leafObjectName.length(1);
  leafObjectName[0].id = CORBA::string_dup(leafObjectId.c_str());

  // Try to bind the object to the name within the naming context

  try {
    objectNC->bind(leafObjectName, objectPtr);
  } catch(const CosNaming::NamingContext::AlreadyBound& ex) {
    CARMA_CPTRACE(carma::util::Trace::TRACE3,
		  "Orb::bind -  Name already bound for "
		  << hierarchicalName);
    throw;
  }
}

void Orb::bind(const string& hierarchicalName, CORBA::Object_ptr objectPtr) 
{
  bind(hierarchicalName, "", objectPtr);
}

void Orb::unbind(const string& hierarchicalName, const string& kind) 
{
  vector<string> ids = getHierarchyIds(hierarchicalName);
    
  if(ids.empty())
    ThrowCarmaError("hierarchicalName decomposed to an empty list of ids");

  const string leafObjectId = ids.back();
  ids.pop_back();
    
  // Get naming context for our object

  CosNaming::NamingContext_var objectNC = getNamingContext(ids);
  CosNaming::Name leafObjectName;
    
  leafObjectName.length(1);
  leafObjectName[0].id   = CORBA::string_dup(leafObjectId.c_str());
  leafObjectName[0].kind = CORBA::string_dup(kind.c_str());

  // Rebind the object to the name within the naming context
    
  objectNC->unbind(leafObjectName);
}

CosNaming::NamingContext_var Orb::getRootNamingContext() 
{
  throwIfNull();
  
  // Try resolving initial references - Guaranteed to return a 
  // valid pointer or throw an exception (A.B. NOT true - fix). 
  
  CosNaming::NamingContext_var rootNC = 0;
  
  try {
    rootNC = resolveInit<CosNaming::NamingContext>("NameService");
  } catch ( const CORBA::SystemException & ex ) {
    CARMA_CPTRACE( carma::util::Trace::TRACE3,
		   "Orb::getRootNamingContext - "
		   << " Cannot resolve NameService\n");
    throw; // Rethrow to a higher context.
  }
  
  // Let's just double check that this isn't nil
  
  if(CORBA::is_nil(rootNC)) 
    ThrowCarmaError("Static root naming context is nil!");
  
  return rootNC;
}

CosNaming::NamingContext_var Orb::getNamingContext(const vector<string>& ids, const std::string& kind) 
{
  const size_t numIds = ids.size();

  CosNaming::NamingContext_var rootNC = getRootNamingContext();
  CosNaming::NamingContext_var answer;
  
  if(numIds == 0)
    return rootNC;
        
  CosNaming::Name name;

  for (size_t i=0; i < numIds; i++) {

    // Starting from rootNamingContext, bind naming contexts on top of
    // each other

    name.length(i+1);
    name[i].id   = CORBA::string_dup(ids[i].c_str());
    name[i].kind = CORBA::string_dup(kind.c_str());
            
    try {

      // do not use rebind_context since it will cause orphaned
      // contexts if already bound
      
      rootNC->bind_new_context(name);
      
    } catch(const CosNaming::NamingContext::AlreadyBound& e) {
      // It's okay if it is already bound.
      // Just continue along making sure we have the whole chain.
    }
  }
  
  // Return the last (sub)naming context we just tried to ensure existed
  // but this time release the var as opposed to returning the entire thing.

  answer = getNarrowedVar<CosNaming::NamingContext>(rootNC->resolve(name));

  return answer._retn(); 
}

CosNaming::NamingContext_var Orb::getNamingContext(const vector<string>& ids) 
{
  CosNaming::NamingContext_var nc = getNamingContext(ids, "");
  return nc._retn();
}

CORBA::ORB_var Orb::getORB()
{
  throwIfNull();
  return orb_;
}

CORBA::ORB_var Orb::duplicateOrb()
{
  throwIfNull();
  return CORBA::ORB::_duplicate(orb_);
}

//=======================================================================
// Notification methods associated with this orb
//=======================================================================

CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
Orb::getNotifyProxyPushSupplier(const string & channelName) 
{
  // first check to make sure there is no ends

  if (channelName.find('\0') != string::npos) 
    ThrowCarmaError("id contains NULL characters (possibly caused by ends)");

  CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
    structuredProxyPushSupplier;

  // check to see if we already have a connected proxy supplier
  // (gProxySuppliers gets reset whenever program exits)

  ScopedLock< ::pthread_mutex_t > lock(proxyPushSupplierMapGuard_);

  if (proxyPushSuppliers_.find(channelName) == proxyPushSuppliers_.end()) {

    // NOTE: do NOT add proxy to nameservice because it currently has
    // a timeout that will automatically destroy and cleanup a proxy
    // that has been down

    // need Consumer Admin to obtain Proxy Supplier

    CosNotifyChannelAdmin::ConsumerAdmin_var
      consumerAdmin = getNotifyConsumerAdmin(channelName);

    // obtain Proxy Pull Supplier

    CosNotifyChannelAdmin::ProxySupplier_var proxySupplier;
    CosNotifyChannelAdmin::ProxyID proxyId;

    try {
      proxySupplier = 
	consumerAdmin->obtain_notification_push_supplier(CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyId);
    } catch(const CosNotifyChannelAdmin::AdminLimitExceeded &err) {
      cerr << "Admin Limit Exceeded" << endl;
      /* DO WE NEED THIS CATCH STATEMENT???*/
    }

    structuredProxyPushSupplier = CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(proxySupplier);

    if(CORBA::is_nil(structuredProxyPushSupplier)) {
      cerr << "Problems obtaining ProxyPushSupplier" << endl;
      ThrowCarmaError("getNotifyProxyPullSupplier() - Obtained "
		      "ProxyPullSupllier is nil!");
    }

    proxyPushSuppliers_.insert(make_pair(channelName, structuredProxyPushSupplier));

  } // end map search of proxyPushSupplier

  return proxyPushSuppliers_[channelName];
}

CosNotifyChannelAdmin::EventChannel_var
Orb::getNotifyChannel(const string & channelName) 
{
  throwIfNull();

  CosNotifyChannelAdmin::EventChannel_var notifyChannel;
  CosNotifyChannelAdmin::EventChannelFactory_var notifyChannelFactory;
  
  // need guard here because want this locked if we need to clean
  // out the IMR

  ScopedLock< ::pthread_mutex_t > lock(notifyChannelGuard_);
    
  // get Notification Channel
  // try to only resolveName twice for the channel

  try {

    notifyChannel = resolveName<CosNotifyChannelAdmin::EventChannel>(channelName);

  } catch (const CosNaming::NamingContext::NotFound &err) {
    // if it gets here, then we need to create a new EventChannel
    // Get reference to Notification Service's Event Channel Factory
    
    notifyChannelFactory = resolveName<CosNotifyChannelAdmin::EventChannelFactory>("NotifyEventChannelFactory");

    if(CORBA::is_nil(notifyChannelFactory)) {

      // Throw an exception to a higher context

      ThrowCarmaError("getNotifyChannel() - Invalid "
		      "CosNotifyChannelAdmin::EventChannelFactory reference");
    }
	
    // Define the parameters necessary for creating a channel
    // note: current design is to use NameService, and not ID

    CosNotification::QoSProperties initialQoS;
    CosNotification::AdminProperties initialAdmin;
    CosNotifyChannelAdmin::ChannelID channelId;  

    // define QoS propteries

    initialQoS.length(6);

    // - connection reliability is necessary for use with IMR

    initialQoS[0].name = CORBA::string_dup("ConnectionReliability");
    initialQoS[0].value <<= CosNotification::Persistent; 

    // - delivery policy - get rid of the oldest ones first

    initialQoS[1].name = CORBA::string_dup("OrderPolicy");
    initialQoS[1].value <<= CosNotification::FifoOrder;

    // - set number of times a failed event will be resent
    // note: this is only for use with a push consumer ... will
    // not work with pull consumer.	 if a notification retries 10
    // times without success, it will destroy the proxy (thus
    // cleaning up the memory)

    initialQoS[2].name = CORBA::string_dup("MaxRetries");
    initialQoS[2].value <<= static_cast< CORBA::ULong >( 10 );

    // - set maximum number of events allowed to queue up in a
    // proxy ... expect 200 at the _most_, so use 500 just to be
    // safe

    initialQoS[3].name = CORBA::string_dup("MaxEventsPerConsumer");
    initialQoS[3].value <<= static_cast< CORBA::Long >( 2000 );

    // - discard policy - get rid of the oldest ones first
    //   - only works with a defined MaxEventsPerConsumer policy

    initialQoS[4].name = CORBA::string_dup("DiscardPolicy");
    initialQoS[4].value <<= CosNotification::FifoOrder;

    // - set timeout for events here in the channel
    //   - only works with a defined MaxEventsPerConsumer policy

    initialQoS[5].name = CORBA::string_dup("Timeout");
    initialQoS[5].value <<= static_cast< TimeBase::TimeT >( 10000000 );
    
    // create notification channel

    try {
      notifyChannel = notifyChannelFactory->create_channel(initialQoS,
							   initialAdmin,
							   channelId);
    } catch (const CosNotification::UnsupportedQoS &err) {
      cerr << "Channel initialized with Unsupported QoS" 
	   << err << endl;
    }
    
    // add object to NameService

    try {

      // bind the channel into nameserv

      bind(channelName, CORBA::Object::_duplicate(notifyChannel));
      
      // bind channelId into nameserv

      ostringstream tempName;
      tempName << channelName << "Channel.id." << channelId;

      // no one should ever try to resolve this, since it's just a holder
      // for the ID ... so use a nil Object

      bind(tempName.str(), CORBA::Object::_nil());

    } catch (const CosNaming::NamingContext::AlreadyBound &ex) {
      // if it gets here, it's because of a race condition between
      // processes, so destroy everything, clean IMR and throw excepction
      
      notifyChannel->destroy();
      
      // at this point, just try to resolve the name again

      notifyChannel = resolveName<CosNotifyChannelAdmin::EventChannel>(channelName);
      
    } catch (const CORBA::SystemException &err) {
      cerr << "DEBUG: Problems finding " << channelName << endl;
      // Throw to higher context...
      throw;
    }
  }

  return notifyChannel;
}

/**.......................................................................
 * Return notify consumer admin object
 */
CosNotifyChannelAdmin::ConsumerAdmin_var
Orb::getNotifyConsumerAdmin(const string & channelName) 
{
  CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin;
  string adminName = channelName+"Channel.consumerAdmin.name";

  try {
    consumerAdmin = resolveName<CosNotifyChannelAdmin::ConsumerAdmin>(adminName);
  } catch (const CosNaming::NamingContext::NotFound &err) {
    // can't find admin in nameserv ... so create a new one
    // get Event Channel

    CosNotifyChannelAdmin::EventChannel_var notifyChannel;
    notifyChannel = getNotifyChannel(channelName);

    CosNotifyChannelAdmin::InterFilterGroupOperator groupOperator = CosNotifyChannelAdmin::OR_OP;
    CosNotifyChannelAdmin::AdminID adminId;
      
    // create new consumer admin for binding to nameserver, rather
    // than binding the default one

    consumerAdmin = notifyChannel->new_for_consumers(groupOperator, adminId);

    try {

      bind(adminName, CORBA::Object::_duplicate(consumerAdmin));

    } catch (const CosNaming::NamingContext::AlreadyBound &ex) {

      // if it gets here, it's because of a race condition, so
      // destroy everything

      consumerAdmin->destroy();

      // just try to resolve again

      consumerAdmin = resolveName<CosNotifyChannelAdmin::ConsumerAdmin>(adminName);
    }
  } catch (...) {

    // end name resolution of consumerAdmin

    throw;
  }

  return consumerAdmin._retn();
}

/**.......................................................................
 *
 */
CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
Orb::getNotifyProxyPushSupplier( const string &               channelName,
				 const string &               proxyName,
				 CosNotifyFilter::ConstraintExpSeq * constraints ) 
{
  CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
    structuredProxyPushSupplier, tempProxy;

  string proxyFullName = 
    channelName+"Channel.consumerAdmin."+proxyName;

  // check to see if we already have a connected proxy supplier
  // (gProxySuppliers gets reset whenever program exits, but should
  // still be in nameserv)

  ScopedLock< ::pthread_mutex_t > lock( proxyPushSupplierMapGuard_ );

  if (proxyPushSuppliers_.find(proxyFullName) == proxyPushSuppliers_.end()) {

    // NOTE: do NOT add proxy to nameservice because it currently has
    // a timeout that will automatically destroy and cleanup a proxy
    // that has been down

    // need Consumer Admin to obtain Proxy Supplier

    CosNotifyChannelAdmin::ConsumerAdmin_var
      consumerAdmin = getNotifyConsumerAdmin(channelName);

    // obtain Proxy Push Supplier

    CosNotifyChannelAdmin::ProxySupplier_var proxySupplier;
    CosNotifyChannelAdmin::ProxyID proxyId;

    try {
      proxySupplier = consumerAdmin->obtain_notification_push_supplier(
								       CosNotifyChannelAdmin::STRUCTURED_EVENT, 
								       proxyId );

    } catch(const CosNotifyChannelAdmin::AdminLimitExceeded &err) {
      cerr << "Admin Limit Exceeded" << endl;
      /* DO WE NEED THIS CATCH STATEMENT???*/
    }

    // set up filters
    if (constraints != 0) {
      CosNotifyChannelAdmin::EventChannel_var
	notifyChannel = getNotifyChannel(channelName);
      CosNotifyFilter::FilterFactory_var filterFactory = 
	notifyChannel->default_filter_factory();
      CosNotifyFilter::Filter_var filter = 
	filterFactory->create_filter("EXTENDED_TCL");
      filter->add_constraints(*constraints);
      proxySupplier->add_filter(filter);
      cout << "Filter: " << (*constraints)[0].constraint_expr << endl;
    }

    structuredProxyPushSupplier = 
      CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(proxySupplier);

    if (CORBA::is_nil(structuredProxyPushSupplier)) {
      cerr << "Problems obtaining ProxyPushSupplier" << endl;
      throw CARMA_EXCEPTION(ErrorException, 
			    "getNotifyProxyPullSupplier() - Obtained "
			    "ProxyPullSupllier is nil!");
    }
    
    // do NOT connect structuredProxyPushSupplier here, since
    // connection done in NotificationConsumer 

    proxyPushSuppliers_.insert( make_pair(proxyFullName, structuredProxyPushSupplier) );

  } // end map search of proxyPushSupplier

  return proxyPushSuppliers_[proxyFullName];
}

// -----------------------------------------------------------------------------
CosNotification::StructuredEvent_var
Orb::createEventForm( const string & typeName,
		      const string & eventName,
		      const string & domainName) 
{
  // create new structured event

  CosNotification::StructuredEvent_var event = 
    new CosNotification::StructuredEvent;

  // fill header information
  // - EVENT TYPE info

  event->header.fixed_header.event_type.domain_name = 
    CORBA::string_dup(domainName.c_str());

  event->header.fixed_header.event_type.type_name = 
    CORBA::string_dup(typeName.c_str());

  // - EVENT NAME
  event->header.fixed_header.event_name =
    CORBA::string_dup(eventName.c_str());

  // apply QoS

  event->header.variable_header.length(1);
  /* this is currently causing HUGE log files to be created
  // set event reliability to persistent
  event->header.variable_header[0].name =
  CORBA::string_dup("CosNotification::EventReliability");
  event->header.variable_header[0].value <<= CosNotification::Persistent;
  */
  // - TimeT in units of 100 ns and we want events discarded after 0.5 s
  //   ... o/w channel gets bogged down and slow
  // try 1s

  event->header.variable_header[0].name = CORBA::string_dup("Timeout");

  event->header.variable_header[0].value <<=
    static_cast< TimeBase::TimeT >( 10000000 );
  
  return event;
}

/**.......................................................................
 * Send a notification
 */
bool Orb::sendNotification(const string &                channelName,
			   const string &                proxyName,
			   CosNotification::StructuredEvent_var event) 
{
  int    retryCount = 0;
  bool   send = true;
  CosNotifyChannelAdmin::StructuredProxyPushConsumer_var notifyProxyPushConsumer;
  const double timeout = 5.0; // Seconds  
  double startTime = Time::MJD();  
 
  do  {
    try  {

      // obtain Proxy Push Consumer for our Push Supplier

      notifyProxyPushConsumer = 
        getNotifyProxyPushConsumer(channelName, proxyName);
      
    } catch (const CORBA::ORB::InvalidName& excep)  {
      double deltaTime = Time::SECONDS_PER_DAY*(Time::MJD() - startTime);
      if (deltaTime > timeout) {
	log4cpp::Category& logger = carma::util::Program::getLogger();
	ostringstream os;
	os << "carma::util::sendNotification: "
	   << "could not obtain reference to proxy push consumer " 
	   << "with channel name " << channelName 
	   << " for " << timeout << " seconds.\n"
	   << "CORBA::ORB::InvalidName exception "
	   << excep._info().c_str();
	logger << log4cpp::Priority::ERROR << os.str();
	send = false;
	retryCount = maxRetries_;
      }    
      else  {
	retryCount++;
	sleep(1);
      }
    } catch (const CosNaming::NamingContext::CannotProceed& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CosNaming::NamingContext::CannotProceed exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CosNaming::NamingContext::InvalidName& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CosNaming::NamingContext::InvalidName exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CosNotification::UnsupportedQoS& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CosNotification::UnsupportedQoS exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CosNotification::UnsupportedAdmin& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CosNotification::UnsupportedAdmin exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CosNotifyChannelAdmin::AdminLimitExceeded& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CosNotifyChannelAdmin::AdminLimitExceeded exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CORBA::SystemException& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not obtain reference to proxy push consumer " 
	 << "with channel name " << channelName << "."
	 << endl
	 << "CORBA::SystemException exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    }

    if (retryCount >= maxRetries_) 
      break;

    try  {
      notifyProxyPushConsumer->push_structured_event(event);
      send = false;

      // disconnect from Proxy Consumer
      //***  notifyProxyPushConsumer->disconnect_structured_push_consumer();
    } catch (const CosEventComm::Disconnected& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not send notification "
	 << "to channel name " << channelName << ". "
	 << "CosEventComm::Disconnected exception "
	 << excep._info().c_str()
	 << endl;
      logger << log4cpp::Priority::ERROR
	     << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    } catch (const CORBA::SystemException& excep)  {
      log4cpp::Category& logger = carma::util::Program::getLogger();
      ostringstream os;
      os << "carma::util::sendNotification: "
	 << "could not send notification "
	 << "to channel name " << channelName 
	 << ", proxy name " << proxyName << ".  "
	 << "CORBA::SystemException exception "
	 << excep._info().c_str();
      logger << log4cpp::Priority::ERROR << os.str();
      if (retryCount < maxRetries_)  {
	retryCount++;
      }  else  {
	send = false;
      }
    }
  } while (send);

  return true;
}

CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
Orb::getNotifyProxyPushConsumer( const string & channelName,
				 const string & proxyName )
{
  const string proxyFullName = 
    channelName + "Channel.supplierAdmin." + proxyName;
    
  CosNotifyChannelAdmin::StructuredProxyPushConsumer_var result;
  {
    const ScopedLock< ::pthread_mutex_t > lock( proxyConsumerMapGuard_ );
        
    // check to see if we already have a connected proxy consumer
    // (gProxyConsumers is lost when program exits ... but should still
    // be in nameserv)

    const ProxyConsumerMap::const_iterator iProxyConsumer =
      proxyConsumers_.find( proxyFullName );

    if ( iProxyConsumer != proxyConsumers_.end() )
      result = iProxyConsumer->second;
    else {
      bool proxyConnected = true;
      CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
	structuredProxyPushConsumer, tempProxy;

      // check if we have a proxy in the nameserv
      try {
	tempProxy = resolveName<CosNotifyChannelAdmin::StructuredProxyPushConsumer >(proxyFullName);
              
	// if we're here, that means the proxy is still alive even
	// though the consumer has died

	proxyConsumers_.insert( make_pair(proxyFullName, tempProxy) );
      } catch (const CosNaming::NamingContext::NotFound &err) {
	proxyConnected = false;
      } catch (const CORBA::SystemException &ex) {
	proxyConnected = false;
      } // end nameserv search for proxy
        
      if ( !proxyConnected ) {

	CosNotifyChannelAdmin::SupplierAdmin_var
	  supplierAdmin = getNotifySupplierAdmin(channelName);
                
	// obtain Proxy Push Consumer

	CosNotifyChannelAdmin::ProxyConsumer_var proxyConsumer;
	CosNotifyChannelAdmin::ProxyID proxyId;
                
	proxyConsumer = 
	  supplierAdmin->obtain_notification_push_consumer(
							   CosNotifyChannelAdmin::STRUCTURED_EVENT,
							   proxyId );
                
	structuredProxyPushConsumer = 
	  CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow(proxyConsumer);
                
	if (CORBA::is_nil(structuredProxyPushConsumer)) {
	  ThrowCarmaError("getNotifyProxyPushConsumer() -"
			  " Could not obtain ProxyPushConsumer -"
			  " reference is nil!" );
	}
                
	// connect push supplier to proxy push consumer passing nil
	// as argument since we are not interested (i believe) in
	// the following:
	// 1. when it is about to be disconnected
	// 2. when there is a change in the set of events to which
	//    consumers are currently subscribed

	structuredProxyPushConsumer->connect_structured_push_supplier(
								      CosNotifyComm::StructuredPushSupplier::_nil() );
                
	// add proxy to map and nameserv

	addObject(proxyFullName, CORBA::Object::_duplicate(structuredProxyPushConsumer));
	proxyConsumers_[ proxyFullName]  = structuredProxyPushConsumer;
      }
        
      result = proxyConsumers_[proxyFullName];
            
      programLogInfoIfPossible("getNotifyProxyPushConsumer added " +
			       proxyFullName + " to global gProxyConsumers map" );
    }
  }
    
  return result;
}

CosNotifyChannelAdmin::SupplierAdmin_var
Orb::getNotifySupplierAdmin(const string& channelName) 
{
  CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin;
  string adminName = channelName+"Channel.supplierAdmin.name";

  // try to find Admin in Name Server
  try {
    supplierAdmin = resolveName<CosNotifyChannelAdmin::SupplierAdmin>(adminName);
  } catch (const CosNaming::NamingContext::NotFound &err) {

    // can't find admin in nameserv ... so create a new one
    // get Event Channel

    CosNotifyChannelAdmin::EventChannel_var notifyChannel;
    notifyChannel = getNotifyChannel(channelName);
      
    CosNotifyChannelAdmin::InterFilterGroupOperator groupOperator = CosNotifyChannelAdmin::OR_OP;
    CosNotifyChannelAdmin::AdminID adminId;

    // need to create new admin to bind with nameserver (since
    // binding the default admin would be bad)

    supplierAdmin = notifyChannel->new_for_suppliers(groupOperator, adminId);

    try {
      bind(adminName, CORBA::Object::_duplicate(supplierAdmin));
    } catch (const CosNaming::NamingContext::AlreadyBound &ex) {
      // if it gets here, it's because of a race condition, so
      // destroy everything
      supplierAdmin->destroy();

      // at this point, just try to resolve again

      supplierAdmin = resolveName<CosNotifyChannelAdmin::SupplierAdmin>(adminName);
    }
  } // end resolveName
    
  return supplierAdmin._retn();
}

std::string Orb::getName()
{
  if(hasUniqueName_) {
    return name_;
  } else {
    return "ORB";
  }
}

void Orb::setName(std::string name)
{
  name_          = name;
  hasUniqueName_ = true;
}

std::string Orb::getImrName()
{
  if(hasUniqueImrName_) {
    return imrName_;
  } else {
    ThrowCarmaError("No IMR name has been specified");
    return "";
  }
}

void Orb::setImrName(std::string name)
{
  imrName_          = name;
  hasUniqueImrName_ = true;
}

void Orb::allowRegistrationWithImr(bool allow)
{
  allowRegistrationWithImr_ = allow;
}
