package carma.ui.jrtd.rtd;

/** 
 * This contains the fundamental version information that must be synchronized
 * between the java clients and the C++ server.
 * The information on the latest client version for the server to use
 * in informing the user is /opt/rt/conf/rtd/version.tab.
 * The version of the server and client must match through the tenths digit or
 * the client will not start.
 */
public class Version {
    static private final String clientVersion = "2.0.0";

    static public String getClientVersion() {
        return clientVersion;
    }
}

// vim: set ts=4 sts=4 sw=4 et:
