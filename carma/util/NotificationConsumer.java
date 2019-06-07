// $Id: NotificationConsumer.java,v 1.10 2013/01/31 21:59:02 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.util;

import org.omg.CORBA.*;
import org.omg.PortableServer.*;
import org.omg.PortableServer.POAPackage.*;
import org.omg.CosNotification.*;
import org.omg.CosNotifyChannelAdmin.*;
import org.omg.CosNotifyComm.*;
import java.util.*;
import carma.util.CorbaUtils;

/**
 * A base class that accepts named events from a Push supplier.
 * This is the basis for event driven clients, with the push
 * method being executed when the event is received.  This class
 * is usually extended and the push method is overridden.
 * At the time of construction, this consumer object is bound
 * to a notification channel, and an instance of a notification queue.
 */
public class NotificationConsumer
    extends StructuredPushConsumerPOA implements Runnable {
    protected org.omg.CORBA.ORB orb_;
    protected org.omg.PortableServer.POA poa_;
    protected org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier proxyPushSupplier_;


    /**
     * use if multiple consumers in a thread and want to specify own orb
     * @param orb CORBA orb
     * @param channelName name of the event channel
     * @throws org.omg.PortableServer.POAPackage.AdapterAlreadyExists
     * @throws org.omg.PortableServer.POAPackage.InvalidPolicy
     */
    public NotificationConsumer(org.omg.CORBA.ORB orb, String channelName)
        throws org.omg.PortableServer.POAPackage.AdapterAlreadyExists,
               org.omg.PortableServer.POAPackage.InvalidPolicy,
               org.omg.CORBA.ORBPackage.InvalidName {

        // random proxy name
        Random r = new Random();
        String proxyName = Long.toString(Math.abs(r.nextLong()), 36);
        String poaName = channelName + "." + proxyName + "." + "JPushConsumer";

        CorbaUtils cu = new CorbaUtils(orb);

        orb_ = orb;
        poa_ = cu.getPOA(poaName);
        proxyPushSupplier_ = cu.getNotifyProxyPushSupplier(channelName);
    }

    /**
     * Return POA object.  Must be defined for classes inheriting from
     * StructuredPushConsumerPOA
     */
    public org.omg.PortableServer.POA _default_POA() {
        return poa_;
    }

    /**
     * Callback used by ORBacus Notification Server to push events to
     * consumer.  Should be overridden by derived class
     * @throws org.omg.CosEventComm.Disconnected
     */
    public void push_structured_event(org.omg.CosNotification.StructuredEvent event)
        throws org.omg.CosEventComm.Disconnected {

        // if the server receives a Disconnected exception, it will
        // begin a cleanup process on the server-side
        Debug.print(this, Debug.INFO, "NotificationConsumer::push_structured_event: default implementation");
        throw new org.omg.CosEventComm.Disconnected();
    }

    /**
     * Callback method used by the ORBacus Notification.  Should NOT
     * be overridden in derived class
     */
    public void disconnect_structured_push_consumer() {
        byte[] oid = null;

        try {
            oid = poa_.servant_to_id(this);
        } catch (org.omg.PortableServer.POAPackage.ServantNotActive e) {
            e.printStackTrace();
        } catch (org.omg.PortableServer.POAPackage.WrongPolicy e) {
            e.printStackTrace();
        }

        try {
            poa_.deactivate_object(oid);
        } catch (org.omg.PortableServer.POAPackage.ObjectNotActive e) {
            e.printStackTrace();
        } catch (org.omg.PortableServer.POAPackage.WrongPolicy e) {
            e.printStackTrace();
        }

        // stop the current ORB immediately (do not block)
        orb_.shutdown(false);
    }

    /**
     * Method used by the ORBacus notification server to inform the
     * consumer that it will be changing the type of event it will be
     * sending.
     */
    public void offer_change(org.omg.CosNotification.EventType[] added,
                             org.omg.CosNotification.EventType[] removed)
    {
        // intentionally left blank
        Debug.print(this, Debug.INFO, "NotificationConsumer::offer_change called");
    }

    /*------------------------------------------------------------------------*/
    /* Runnable Interface                                                     */
    /*------------------------------------------------------------------------*/

    /**
     * Blocking call that activates the callback for collecting events
     * from the ORBacus notification server.
     */
    public void run() {
        org.omg.PortableServer.POAManager manager = poa_.the_POAManager();

        String objString = "pushConsumerObject";
        byte[] objectId = objString.getBytes();

        try {
            poa_.activate_object_with_id(objectId, this);
        } catch (org.omg.PortableServer.POAPackage.ServantAlreadyActive ex) {
            ex.printStackTrace();
        } catch (org.omg.PortableServer.POAPackage.ObjectAlreadyActive ex) {
            ex.printStackTrace();
        } catch (org.omg.PortableServer.POAPackage.WrongPolicy ex) {
            ex.printStackTrace();
            // do nothing
        }

        org.omg.CosNotifyComm.StructuredPushConsumer structuredPushConsumer = this._this();

        try {
            proxyPushSupplier_.connect_structured_push_consumer(structuredPushConsumer);
        } catch (org.omg.CosEventChannelAdmin.AlreadyConnected ex) {
            ex.printStackTrace();
        } catch (org.omg.CosEventChannelAdmin.TypeError ex) {
            // do nothing
            ex.printStackTrace();
        } catch (org.omg.CORBA.OBJECT_NOT_EXIST ex) {
            ex.printStackTrace();
        }

        try {
            manager.activate();

            // DO is active: block forever on the ORB
            orb_.run();
        } catch (org.omg.PortableServer.POAManagerPackage.AdapterInactive ex) {
            // do nothing
            ex.printStackTrace();
        }
    }

} // end class NotificationConsumer
