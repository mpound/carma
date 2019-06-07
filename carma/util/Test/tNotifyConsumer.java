package carma.util.Test;

import carma.util.CorbaUtils;

/**
 * $Id: tNotifyConsumer.java,v 1.3 2013/01/31 21:59:04 iws Exp $
 *
 * tNotifyConsumer.java - test code for a push consumer using java
 *
 * usage: java -Xbootclasspath/p:$CLASSPATH tNotifyConsumer [imrhost] [channelId]
 *
 */


public class tNotifyConsumer {

    public static void main (String[] args) {

        if (args.length < 1) {
            System.out.println("usage: java -Xbootclasspath/p:$CLASSPATH tNotifyConsumer [imrhost] [channelId]");
            return;
        }

        String imrhost = new String(args[0]);
        String id = new String(args[1]);
            
        org.omg.CORBA.ORB orb = carma.util.CorbaUtils.getNewORB(args, null, imrhost);
        String channelName = "MyChannel"+id;

        LocalPushConsumer localConsumer = null;
        try {
            localConsumer = new LocalPushConsumer(orb, channelName);
        } catch (org.omg.PortableServer.POAPackage.AdapterAlreadyExists ex) {
            ex.printStackTrace();
            System.exit(1);
        } catch (org.omg.PortableServer.POAPackage.InvalidPolicy ex) {
            ex.printStackTrace();
            System.exit(1);
        } catch (org.omg.CORBA.ORBPackage.InvalidName ex) {
            ex.printStackTrace();
            System.exit(1);
        }

        localConsumer.run();
    }
}

// vim: set ts=4 sts=4 sw=4 et:
