
// $Id: NotificationConsumer.h,v 1.13 2013/01/08 05:07:32 abeard Exp $

#ifndef NOTIFICATIONCONSUMER_H
#define NOTIFICATIONCONSUMER_H

#include <orbsvcs/CosNotifyChannelAdminC.h>
#include <orbsvcs/CosNotifyCommS.h>

namespace carma {
   namespace util {

     class Orb;

      /**
       * A base class that accepts named events from a Push supplier.
       * This is the basis for event driven clients, with the push
       * method being executed when the event is received.  This class
       * is usually extended and the push method is overridden.
       * At the time of construction, this consumer object is bound 
       * to a notification channel, and an instance of a notification queue
       * (through the consumerName).
       */
      class NotificationConsumer : 
	virtual public POA_CosNotifyComm::StructuredPushConsumer
	{
	  
	public:

	  /**
	   * Constructor for Notification Consumer - creates ORB internally
	   * @param orb input pointer to orb
	   * @param channelName name of channel for notifications
	   * @param consumerName name of the consumer
	   * @throw carma::util::ErrorException if resolved object is nil.
	   * @throw CORBA::ORB::InvalidName if NameService is not found
	   * @throw CORBA::SystemException for failed _narrow on NameService
	   * @throw CosNaming::NamingContext::CannotProceed
	   * @throw CosNaming::NamingContext::InvalidName if the
	   * CosNaming::Name created from hierarchicalName has an invalid element
	   * @throw CORBA::ORB::InvalidName if RootPOA is not found
	   * @throw CORBA::SystemException for failed _narrow on RootPOA
	   * @throw PortableServer::POA::InvalidPolicy if policyList
	   * contains an invalid policy
	   * @throw PortableServer::POA::AdapterAlreadyExists if POA
	   * with given poaString has already been craeted
	   * @throw CORBA::Exception cannot resolve ORB
	   *
	   */	  
	  NotificationConsumer(Orb* localOrb,
			       const std::string channelName, 
	                       const std::string consumerName);

	  ~NotificationConsumer();
	  
	  // IDL to C++ mapping

	  /**
	   * Callback function that is called by the notification server
	   * when notifications arrive for this consumer.
	   */
	  virtual void push_structured_event(const CosNotification::StructuredEvent &event) 
	    throw(CosEventComm::Disconnected, CORBA::SystemException);
	  
	  /**
	   * Callback function that gets called by the notification
	   * server if it decides to disconnect the push consumer. (DO
	   * NOT override this method!!!)
	   */
	  virtual void disconnect_structured_push_consumer() 
	    throw(CORBA::SystemException);
	  
	  virtual void offer_change(const CosNotification::EventTypeSeq&,
				    const CosNotification::EventTypeSeq&)
	    throw(CosNotifyComm::InvalidEventType, CORBA::SystemException);

	  virtual PortableServer::POA_ptr _default_POA();
	  
	  /**
	   * Blocking call that activates the callback for 
	   * collecting events from the notification server.
           *
	   * @throw CARMA_ERROR
	   * @throw PortableServer::POAManager::AdapterInactive
	   */
	  virtual void run();
	  
	  /**
	   * Method to test whether consumer is active. Intended
	   * to be used in a multithreaded environment since run() 
	   * is a blocking call.
	   */
	  bool isActive();

	  /**
	   * Method to deactivate an active consumer. Intended
	   * to be used in a multithreaded environment since run() 
	   * is a blocking call.
	   */
	  void deactivateConsumer();

	protected:

	  Orb* localOrb_;
	  std::string localOrbName_;

	  CORBA::ORB_var orb_;    // The ORB.
	  PortableServer::POA_var poa_;    // My POA.
          CosNotifyChannelAdmin::StructuredProxyPushSupplier_var proxyPushSupplier_;
	  const std::string channelName_;
	  const std::string consumerName_;

	  /**
	   * create a proxy push supplier for our push consumer
	   * @throw carma::util::ErrorException if resolved object is nil.
	   * @throw CORBA::ORB::InvalidName if NameService is not found
	   * @throw CORBA::SystemException for failed _narrow on NameService
	   * @throw CosNaming::NamingContext::CannotProceed
	   * @throw CosNaming::NamingContext::InvalidName if the
	   * CosNaming::Name created from hierarchicalName has an invalid element
	   */
	  void setProxyPushSupplier ();

	private:

	  /**
	   *  method for activating POA
	   */
	  void activatePoa();

	  bool isActive_;

	}; // End class NotificationConsumer

   } // End namespace util
} // End namespace carma

#endif // eventconsumer_h
