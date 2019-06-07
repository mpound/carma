// $Id: Orb.h,v 1.3 2013/01/08 05:07:32 abeard Exp $

#ifndef CARMA_UTIL_ORB_H
#define CARMA_UTIL_ORB_H

/**
 * @file Orb.h
 * 
 * Tagged: Tue Feb 22 18:45:20 PST 2011
 * 
 * @version: $Revision: 1.3 $, $Date: 2013/01/08 05:07:32 $
 * 
 * @author username: Erik Leitch
 */
#include "carma/corba/corba.h"
#include <orbsvcs/CosNamingC.h>
#include <tao/PortableServer/PortableServer.h>

#include <tao/TimeBaseC.h>
#include <orbsvcs/CosNotifyFilterC.h>
#include <orbsvcs/NotifyExtC.h>

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <string>
#include <vector>

namespace carma {
  namespace util {

    class Orb {
    public:

      typedef enum {
	FAIL_ON_NIL_INPUT,
	NIL_INPUT_OKAY
      } NilInputDisposition;
      
      typedef enum {
	FAIL_ON_INCOMPATIBLE_TYPES,
	INCOMPATIBLE_TYPES_OKAY
      } IncompatibleTypesDisposition;
      
      /**
       * Constructor.
       */
      Orb();

      /**
       * Destructor.
       */
      virtual ~Orb();

      // Set the ID of this ORB

      bool set(int argc, char** argv, const std::string& imrhost);
      bool isNull();
      bool shutdown(bool waitForCompletion);

      friend bool ProgramBase::orbInit(Orb* orb);

      CORBA::ORB_var duplicateOrb();
      CORBA::ORB_var getORB();

      void setName(std::string name);
      std::string getName();

      void setImrName(std::string imrName);
      std::string getImrName();

      void allowRegistrationWithImr(bool allow);

    public:

      // A unique name for this orb

      std::string name_;
      bool hasUniqueName_;

      // The IMR to connect to 

      std::string imrName_;
      bool hasUniqueImrName_;

      bool allowRegistrationWithImr_;

      CORBA::ORB_var orb_;
      std::vector<std::string> lArgvVec_;

      ::pthread_mutex_t notifyChannelGuard_;
      ::pthread_mutex_t proxyPushSupplierMapGuard_;
      ::pthread_mutex_t proxyConsumerMapGuard_;

      typedef std::map<std::string, CosNotifyChannelAdmin::StructuredProxyPushSupplier_var> ProxyPushSupplierMap;
      ProxyPushSupplierMap  proxyPushSuppliers_;

      typedef std::map<std::string, CosNotifyChannelAdmin::StructuredProxyPushConsumer_var > ProxyConsumerMap;
      ProxyConsumerMap proxyConsumers_;

      void throwIfNull();
      bool destroy();
      std::string getPersistentPOAName(const std::string& poaName);
      PortableServer::POA_var        getRootPOA(); 
      PortableServer::POA_var        getPOA(const std::string& poaNameString = "RootPOA");
      PortableServer::POAManager_var getPOAManager(const std::string& poaString = "RootPOA"); 

      void addObject(const std::string& hierarchicalName, CORBA::Object_ptr objectPtr); 
      void addObject(const std::string& hierarchicalName, const std::string& kind, CORBA::Object_ptr objectPtr);
      void removeObject(const std::string& hierarchicalName); 

      void run(const std::string& poaString = "RootPOA"); 
      void work(); 
      bool workPending();
      void performWork();

      void activatePOA(const std::string& poaString);
      void deactivatePOA(bool etherealize, bool wait, const std::string& poaString); 
      void holdPOA(bool wait, const std::string& poaString); 
      void discardPOA(bool wait, const std::string& poaString); 

      CORBA::Object_var resolve(const std::string& hierarchicalName, const std::string& kind); 
      CORBA::Object_var resolveInit(const char *   idName ); 
      CORBA::Object_var resolveName(const std::string & hierarchicalName, const std::string& kind); 
      CORBA::Object_var waitToResolveName(const std::string& hierarchicalName, const std::string& kind); 
      CORBA::Object_var resolveFromString(const std::string& objectString); 
      CORBA::Object_var resolveFromFile(const std::string& fileName, const std::string& directory); 

      std::vector<std::string> getHierarchyIds(const std::string& hierarchicalName); 

      void unbind(const std::string& hierarchicalName, const std::string& kind = "");
      void bind(const std::string& hierarchicalName, CORBA::Object_ptr objectPtr);
      void bind(const std::string& hierarchicalName, const std::string& kind, CORBA::Object_ptr objectPtr);
      void rebind(const std::string& hierarchicalName, const std::string& kind, CORBA::Object_ptr objectPtr);
      void rebind(const std::string& hierarchicalName, CORBA::Object_ptr objectPtr);

      CosNaming::NamingContext_var getRootNamingContext();
      CosNaming::NamingContext_var getNamingContext(const std::vector<std::string>& ids);
      CosNaming::NamingContext_var getNamingContext(const std::vector<std::string>& ids, const std::string& kind);

      //------------------------------------------------------------
      // Notification methods
      //------------------------------------------------------------

      int maxRetries_;

      CosNotifyChannelAdmin::ConsumerAdmin_var
	getNotifyConsumerAdmin(const std::string & channelName);

      CosNotifyChannelAdmin::SupplierAdmin_var
	getNotifySupplierAdmin(const std::string& channelName);

      CosNotifyChannelAdmin::EventChannel_var
	getNotifyChannel(const std::string & channelName);

      CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
	getNotifyProxyPushSupplier(const std::string & channelName);
	
      CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
	getNotifyProxyPushSupplier( const std::string &                channelName,
				    const std::string &                proxyName,
				    CosNotifyFilter::ConstraintExpSeq* constraints = 0);

      CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
	getNotifyProxyPushConsumer(const std::string& channelName,
				   const std::string& proxyName);

      CosNotification::StructuredEvent_var
	createEventForm( const std::string& typeName,
			 const std::string& eventName,
			 const std::string& domainName = "CARMA");
	
      bool sendNotification(const std::string&                channelName,
			    const std::string&                proxyName,
			    CosNotification::StructuredEvent_var event);

      //-----------------------------------------------------------------------
      // Templates
      //-----------------------------------------------------------------------

      template<typename T, typename S>
	typename T::_var_type getNarrowedVar(const S& s,
					     const NilInputDisposition nilInputDisposition= FAIL_ON_NIL_INPUT, 
					     const IncompatibleTypesDisposition incompatibleTypesDisposition= FAIL_ON_INCOMPATIBLE_TYPES) 
      {
	typename T::_var_type tVar;
    
	if(CORBA::is_nil(s)) {
	  if(nilInputDisposition == FAIL_ON_NIL_INPUT) {
            ThrowCarmaError("Input is NIL.");
	  }
	} else {
	  try {
            tVar = T::_narrow(s);
            
            if((incompatibleTypesDisposition == FAIL_ON_INCOMPATIBLE_TYPES) && 
	       CORBA::is_nil(tVar)) {
	      ThrowCarmaError("Actual type incompatible with target type.");
            }
	    
	  } catch (const CORBA::SystemException& e) {
 	    CARMA_CPTRACE(carma::util::Trace::TRACE3,
			  "Orb::getNarrowedVar< T >( ) failed: "
			  << e);
            throw;  // Rethrow this exception to a higher context
	  } catch (const CORBA::Exception& e) {
	    CARMA_CPTRACE(carma::util::Trace::TRACE3,
			  "Orb::getNarrowedVar< T >( ) failed: "
			  << e);
            throw;  // Rethrow this exception to a higher context
	  } catch (...) {
	    CARMA_CPTRACE(carma::util::Trace::TRACE3,
			  "Orb::getNarrowedVar< T >( ) failed: "
			  << "???");
            throw;  // Rethrow this exception to a higher context
	  }
	}
    
	return tVar._retn();  // Relinquish ownership of tVar
      }

      template<typename T>
	typename T::_var_type resolveInit(const char* idName) 
      {
	CORBA::Object_var objectVar = resolveInit(idName);
	typename T::_var_type typedVar = getNarrowedVar<T>(objectVar);
	return typedVar._retn( );  // Relinquish ownership of typedVar
      }

      template<typename T>
	typename T::_var_type resolveName(const std::string& hierarchicalName, const std::string& kind = "") 
      {
	CORBA::Object_var objectVar = resolveName(hierarchicalName, kind);
	typename T::_var_type typedVar = getNarrowedVar<T>(objectVar);
	return typedVar._retn();  // Relinquish ownership of typedVar
      }

      template<typename T>
	typename T::_var_type waitToResolveName(const std::string& hierarchicalName, const std::string& kind = "") 
      {
	CORBA::Object_var objectVar = waitToResolveName(hierarchicalName, kind);
	typename T::_var_type typedVar = getNarrowedVar<T>(objectVar);
	return typedVar._retn( );  // Relinquish ownership of typedVar
      }

      template<typename T>
	typename T::_var_type resolveFromString(const std::string& objectString) 
      {
	CORBA::Object_var objectVar = resolveFromString(objectString);
	typename T::_var_type typedVar = getNarrowedVar<T>(objectVar);
	return typedVar._retn( );  // Relinquish ownership of typedVar
      }

      template<typename T>
	typename T::_var_type resolveFromFile(const std::string& fileName, const std::string& directory) 
      {
	CORBA::Object_var objectVar = resolveFromFile(fileName, directory);
	typename T::_var_type typedVar = getNarrowedVar<T>(objectVar);
    	return typedVar._retn( );  // Relinquish ownership of typedVar
      }


    }; // End class Orb

  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_ORB_H
