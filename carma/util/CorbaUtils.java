// $Id: CorbaUtils.java,v 1.21 2013/01/31 21:59:02 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.util;

import org.omg.CORBA.*;
import org.omg.CosNaming.*;
import org.omg.CosNaming.NamingContextPackage.*;
import org.omg.CosNotification.*;
import org.omg.CosNotifyChannelAdmin.*;
import org.omg.CosNotifyFilter.*;
import org.omg.PortableServer.*;
import java.util.*;

/**
 * Simple, sane class which has some helper routines for interacting
 * with a CORBA ORB.
 */
public class CorbaUtils {
    private final ORB orb_;
    private NamingContextExt rootnc_;

    /*------------------------------------------------------------------------*/
    /* STATIC PUBLIC INTERFACE                                                */
    /*------------------------------------------------------------------------*/

    /**
     * Generic method to get a new CORBA ORB. This will use system defaults for
     * any arguments which are passed in as null.
     */
    public static ORB getNewORB(String[] args, Properties props, String imrhost) {
        // if args are null, use a dummy argument array
        if (args == null)
            args = new String[0];

        // if properties are not present, use system properties
        if (props == null)
            props = System.getProperties();

        // add properties for JacORB
        props.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
        props.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
        props.setProperty("jacorb.log.default.verbosity", "2");
        props.setProperty("jacorb.use_imr", "off");

        // add imrhost if requested (this method sucks)
        if (imrhost != null) {
            String[] newArgs = new String[args.length + 2];
            System.arraycopy(args, 0, newArgs, 0, args.length);
            newArgs[args.length + 0] = "-ORBDefaultInitRef";
            newArgs[args.length + 1] = "corbaloc::" + imrhost;
            args = newArgs;

            props.setProperty("ORBInitRef.NameService", "corbaloc::" + imrhost + "/NameService");
        }

        // create the ORB
        return org.omg.CORBA.ORB.init(args, props);
    }

    /*------------------------------------------------------------------------*/
    /* PUBLIC INTERFACE                                                       */
    /*------------------------------------------------------------------------*/

    public CorbaUtils(ORB orb) {
        orb_ = orb;
    }

    /**
     * Return Proxy Push Supplier (for a Push Consumer)
     */
    public org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier
        getNotifyProxyPushSupplier(String channelName) {
        org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier proxyPushSupplier = null;

        org.omg.CosNotifyChannelAdmin.ConsumerAdmin consumerAdmin = getNotifyConsumerAdmin(channelName);
        org.omg.CosNotifyChannelAdmin.ProxySupplier proxySupplier = null;
        org.omg.CORBA.IntHolder proxyId = new org.omg.CORBA.IntHolder();

        try {
            proxySupplier = consumerAdmin.obtain_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyId);
        } catch (org.omg.CosNotifyChannelAdmin.AdminLimitExceeded e) {
            e.printStackTrace();
        }

        proxyPushSupplier = org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper.narrow(proxySupplier);

        if (proxyPushSupplier == null) {
            throw new RuntimeException ("ProxyPushSupplier is null!");
        }

        return proxyPushSupplier;
    }

    public POA getPOA(String poaString)
        throws org.omg.PortableServer.POAPackage.AdapterAlreadyExists,
               org.omg.PortableServer.POAPackage.InvalidPolicy,
               org.omg.CORBA.ORBPackage.InvalidName {

        POA myPOA = null;
        POA rootPOA = getRootPOA();

        if (!poaString.equals("RootPOA")) {
            try {
                myPOA = rootPOA.find_POA(poaString, true);
            } catch (org.omg.PortableServer.POAPackage.AdapterNonExistent ex) {
                Debug.print(this, Debug.INFO, "getPOA: create POA: " + poaString);
                // if it doesn't find it, then create it
                POAManager manager = getRootPOAManager();

                // define policy list for POAs
                Policy[] policyList = new Policy[3];
                policyList[0] = rootPOA.create_id_assignment_policy(org.omg.PortableServer.IdAssignmentPolicyValue.USER_ID);
                policyList[1] = rootPOA.create_implicit_activation_policy(org.omg.PortableServer.ImplicitActivationPolicyValue.NO_IMPLICIT_ACTIVATION);
                policyList[2] = rootPOA.create_lifespan_policy(org.omg.PortableServer.LifespanPolicyValue.PERSISTENT);

                myPOA = rootPOA.create_POA(poaString, manager, policyList);
            } // end try ... catch
        } else {
            myPOA = rootPOA;
        }

        return myPOA;
    }

    /**
     *  Returns a CORBA.Object
     *
     *  This class does not work the same as CorbaUtils.cc, you still need
     *  to narrow the object after resolution.
     *
     *  @throw CosNaming.NamingContext.NotFound
     *  @throw CosNaming.NamingContext.CannotProceed
     *  @throw CosNaming.NamingContext.InvalidName
     */
    public org.omg.CORBA.Object resolveName(String id)
        throws org.omg.CosNaming.NamingContextPackage.CannotProceed,
               org.omg.CosNaming.NamingContextPackage.InvalidName,
               org.omg.CosNaming.NamingContextPackage.NotFound,
               org.omg.CORBA.ORBPackage.InvalidName {

        org.omg.CosNaming.NamingContextExt namingContext = getNamingContext(id);

        // Bind name with the Naming Service
        NameComponent[] name = new NameComponent[1];
        name[0] = new NameComponent();

        // object begins right after the last "."
        int objectIndex = id.lastIndexOf(".")+1;
        name[0].id = id.substring(objectIndex);
        name[0].kind = "";

        return namingContext.resolve(name);
    }

    /*------------------------------------------------------------------------*/
    /* PRIVATE INTERFACE                                                      */
    /*------------------------------------------------------------------------*/

    /**
     *  Returns the Root POA
     */
    private POA getRootPOA() throws org.omg.CORBA.ORBPackage.InvalidName {
        return POAHelper.narrow(orb_.resolve_initial_references("RootPOA"));
    }

    /**
     *  Returns the Root POA Manager
     */
    private POAManager getRootPOAManager() throws org.omg.CORBA.ORBPackage.InvalidName {
        return getRootPOA().the_POAManager();
    }

    /**
     *  Return Notification Consumer Admin
     */
    private org.omg.CosNotifyChannelAdmin.ConsumerAdmin getNotifyConsumerAdmin(String channelName) {

        org.omg.CosNotifyChannelAdmin.ConsumerAdmin consumerAdmin = null;
        String adminName = channelName+"Channel.consumerAdmin.name";

        try {
            consumerAdmin = org.omg.CosNotifyChannelAdmin.ConsumerAdminHelper.narrow(resolveName(adminName));
        } catch(org.omg.CosNaming.NamingContextPackage.CannotProceed ex) {
            ex.printStackTrace();
        } catch(org.omg.CosNaming.NamingContextPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch(org.omg.CORBA.ORBPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch(org.omg.CosNaming.NamingContextPackage.NotFound ex) {
            Debug.print(this, Debug.INFO, "getNotifyConsumerAdmin: create new one");
            // can't find in nameserv, so make a new one
            org.omg.CosNotifyChannelAdmin.EventChannel notifyChannel = getNotifyChannel(channelName);
            org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator groupOperator =
                org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator.OR_OP;
            org.omg.CORBA.IntHolder adminId = new org.omg.CORBA.IntHolder();
            consumerAdmin = notifyChannel.new_for_consumers(groupOperator, adminId);
            addObject(adminName, consumerAdmin);
        }

        return consumerAdmin;
    }

    /**
     *  Returns the RootNamingContex for the given ORB
     */
    private NamingContextExt getRootNamingContext() throws org.omg.CORBA.ORBPackage.InvalidName {

        // Obtain the root context if we do not have it cached already
        if (rootnc_ == null) {
            org.omg.CORBA.Object rootObj = orb_.resolve_initial_references("NameService");
            rootnc_ = NamingContextExtHelper.narrow(rootObj);
        }

        return rootnc_;
    }

    /**
     *  Returns naming context (will NOT bind new contexts)
     */
    private org.omg.CosNaming.NamingContextExt getNamingContext(String id)
        throws org.omg.CosNaming.NamingContextPackage.CannotProceed,
               org.omg.CosNaming.NamingContextPackage.InvalidName,
               org.omg.CosNaming.NamingContextPackage.NotFound,
               org.omg.CORBA.ORBPackage.InvalidName {

        org.omg.CosNaming.NamingContextExt namingContext = null;
        org.omg.CosNaming.NamingContextExt rootNamingContext;

        rootNamingContext = getRootNamingContext();

        // contexts separated with '.'
        String [] contexts = id.split("\\.");

        int nContexts = contexts.length - 1; // don't include last element

        // if there are no contexts, just return the root
        if (nContexts == 0)
            return rootNamingContext;

        // if this passes, then the id was null and we shouldn't be continuing
        if (nContexts < 0)
            throw new RuntimeException("CorbaUtils.removeObject: id is null");

        org.omg.CosNaming.NameComponent[] contextName = new org.omg.CosNaming.NameComponent[nContexts];

        for (int i = 0; i < nContexts; i++) {
            contextName[i] = new org.omg.CosNaming.NameComponent();
            contextName[i].id = contexts[i];
            contextName[i].kind = ""; // need this to avoid java.lang.NullPointerException
        }

        for (int i = 0; i < nContexts; i++) {
            int nSubContexts = i+1;
            org.omg.CosNaming.NameComponent[] tempContextName = new org.omg.CosNaming.NameComponent[nSubContexts];
            for (int j = 0; j < nSubContexts; j++) {
                tempContextName[j] = new org.omg.CosNaming.NameComponent();
                tempContextName[j] = contextName[j];
            }

            try {
                rootNamingContext.bind_new_context(tempContextName);
            } catch (org.omg.CosNaming.NamingContextPackage.AlreadyBound ex) {
                // do nothing
            }
        }

        try {
            // get Naming Context to put object into
            namingContext = org.omg.CosNaming.NamingContextExtHelper.narrow(rootNamingContext.resolve(contextName));
        } catch (Exception e) {
            System.out.println("CorbaUtils.java: problems obtaining "+id);
            e.printStackTrace();
        }

        return namingContext;
    }

    /**
     *  Return Notification channel
     *  @throw RuntimeException
     */
    private org.omg.CosNotifyChannelAdmin.EventChannel
        getNotifyChannel(String channelName) {
        org.omg.CosNotifyChannelAdmin.EventChannel notifyChannel = null;
        boolean createNewChannel = false;

        try {
            notifyChannel = org.omg.CosNotifyChannelAdmin.EventChannelHelper.narrow(resolveName(channelName));
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch (org.omg.CORBA.ORBPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.CannotProceed ex) {
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.NotFound ex) {
            // if it can't find it, create one
            createNewChannel = true;
        } catch (java.lang.NullPointerException ex) {
            // if it can't find it, create one
            ex.printStackTrace();
            System.exit(1);
        } // end try ... catch

        if (createNewChannel == true) {
            Debug.print(this, Debug.INFO, "getNotifyChannel: create new channel");
            org.omg.CosNotifyChannelAdmin.EventChannelFactory channelFactory = null;
            org.omg.CORBA.Object cfObj = null;

            // the TAO method: use the name service
            try {
                cfObj = resolveName("NotifyEventChannelFactory");
            } catch (Exception e) {
                e.printStackTrace();
            }

            // the ORBacus method: use the IMR
            try {
                cfObj = (cfObj != null) ? cfObj : orb_.resolve_initial_references("NotificationService");
            } catch (org.omg.CORBA.ORBPackage.InvalidName e) {
                e.printStackTrace();
            }

            // oops, unable to find the notification service
            if (cfObj == null) {
                throw new RuntimeException("Unable to locate NotificationService");
            }

            // narrow the object to the correct type
            try {
                channelFactory = org.omg.CosNotifyChannelAdmin.EventChannelFactoryHelper.narrow(cfObj);
            } catch (Exception e) {
                e.printStackTrace();
            }

            if(channelFactory == null) {
                throw new RuntimeException("NotificationService initial reference is nil");
            }

            Property[] initialQoS = new Property[0];
            Property[] initialAdmin = new Property[0];
            org.omg.CORBA.IntHolder channelId = new org.omg.CORBA.IntHolder();

            try {
                notifyChannel = channelFactory.create_channel(initialQoS, initialAdmin, channelId);
            } catch (org.omg.CosNotification.UnsupportedQoS e) {
                e.printStackTrace();
            } catch (org.omg.CosNotification.UnsupportedAdmin e) {
                e.printStackTrace();
            }

            addObject(channelName, notifyChannel);
        }
        return notifyChannel;
    }

    /**
     *  Used to add CORBA objects to the NameService which allow
     *  CORBA clients to locate via the name.
     *  @param name Name clients will use to find remote CORBA object
     */
    private boolean addObject(String id, org.omg.CORBA.Object obj) {
        org.omg.CosNaming.NamingContextExt namingContext = null;

        String [] contexts = id.split("\\.");
        int nContexts = contexts.length - 1; // don't include last element
        // if this passes, then the id was null and we shouldn't be continuing
        if (nContexts < 0) {
            throw new RuntimeException("CorbaUtils.addObject: id is null");
        }

        // get Naming Context to put object into
        // TODO: determine whether we should exit here because of errors ...?
        try {
            namingContext = getNamingContext(id);
        } catch (org.omg.CosNaming.NamingContextPackage.CannotProceed ex) {
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.NotFound ex) {
            ex.printStackTrace();
        } catch (org.omg.CORBA.ORBPackage.InvalidName ex) {
            ex.printStackTrace();
        }

        // Bind name with the Naming Service
        NameComponent[] name = new NameComponent[1];
        name[0] = new NameComponent();

        // we lazily use nContexts as the index, knowing that this
        // refers to the last element in the contexts array (which
        // is the name of the object)
        name[0].id = contexts[nContexts];
        name[0].kind = "";

        // TODO: determine whether we should exit because of errors ...?
        try {
            namingContext.rebind(name, obj);
        } catch (org.omg.CosNaming.NamingContextPackage.NotFound ex) {
            // should not get here since we are creating the context by hand
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.InvalidName ex) {
            ex.printStackTrace();
        } catch (org.omg.CosNaming.NamingContextPackage.CannotProceed ex) {
            ex.printStackTrace();
        }

        return true;
    }
}
