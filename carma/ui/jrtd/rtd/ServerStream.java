package carma.ui.jrtd.rtd;

import carma.ui.jrtd.event.*;
import carma.ui.jrtd.ui.*;
import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import java.lang.*;
import java.util.*;
import java.net.*;
import java.io.*;

import rtdproto.RTD.UIMessageRequestCode;
import rtdproto.RTD.UIMessageReplyCode;
import rtdproto.RTD.UIMessageRequest;
import rtdproto.RTD.UIMessageReply;

import rtdproto.RTD.AuthenticationRequest;
import rtdproto.RTD.AuthenticationReply;
import rtdproto.RTD.AuthenticationStatusCode;

// XZ / LZMA compression and decompression
import org.tukaani.xz.*;

// ZLIB compression and decompression
import java.util.zip.Inflater;
import java.util.zip.DataFormatException;

/**
 * Manages the connection to the data server that defines and updates
 * the display.
 *
 * A packet is defined by a single rtdproto.RTD.UIMessageReply
 * structure (Google Protocol Buffer).
 *
 * Some windows, like the observing log, keep a read pending to the server.
 * A close of the socket will take over 20 seconds to complete. So the proper
 * shutdown protocol is to send an Exit command to the server, which sends
 * a reply and causes the read to complete, then the close completes.
 *
 * @author Steve Scott
 * @version $Revision: 1.23 $
 */
public class ServerStream {
    private static final Debug debug = new Debug("ServerStream", false);

    private final Parameters params;
    private DataOutputStream os;
    private DataInputStream is;
    private Socket sock;

    public ServerStream(final Parameters params) {
        this.params = params;
    }

    /**
     * Send a message to the server and block until a reply arrives.
     *
     * There are a few cases where more than one thread uses the same
     * ServerStream to send different commands and get a response back.
     * An example is the control window, that sends commands from one thread
     * and then sends update requests from another.
     *
     * @param req the message to send to the server
     * @return the protocol buffer describing the reply
     */
    public synchronized UIMessageReply sendRecv(UIMessageRequest req) throws MyNetworkException {
        debug.println("sendRecv: req=" + req);

        byte[] buf = this.sendRecvBytes(req.toByteArray());
        buf = decompressZLIB(buf);

        try {
            final UIMessageReply msg = UIMessageReply.parseFrom(buf);
            debug.println("sendRecv: msg.code=" + msg.getCode());
            return msg;
        } catch (Exception ex) {
            final String msg = "Unable to parse UIMessageReply";
            throw new MyNetworkException(msg, ex);
        }
    }

    /**
     * Open a new connection to the rtdmaster process and authenticate before
     * returning.
     *
     * After this routine returns successfully, the Java client is connected
     * to the C++ server process for the child window.
     */
    public synchronized boolean connect(final String windowName) throws MyNetworkException {
        final String serverName = params.getRtdHost();
        final int portNumber = params.getRtdPort();

        try {
            debug.println("Connect to " + serverName + " at " + portNumber);
            sock = new Socket(serverName, portNumber);

            // attempt some speedups
            sock.setReceiveBufferSize(64 * 1024);
            sock.setSendBufferSize(64 * 1024);

            // disable Nagle's algorithm: we always send a very small amount of
            // data (<1 minimum MTU packet) so we force TCP to always send it
            // immediately
            sock.setTcpNoDelay(true);

            os = new DataOutputStream(sock.getOutputStream());
            is = new DataInputStream(sock.getInputStream());
        } catch(UnknownHostException ex) {
            final String msg = "Unknown host: " + serverName;
            throw new MyNetworkException(msg, ex);
        } catch (ConnectException ex) {
            final String msg = "Unable to connect to host: " + serverName + ":" + portNumber;;
            throw new MyNetworkException(msg, ex);
        } catch(IOException ex) {
            final String msg = "Unable to connect to host: " + serverName + ":" + portNumber;;
            throw new MyNetworkException(msg, ex);
        } catch (Exception ex) {
            final String msg = "Unknown Exception when connecting to: " + serverName + ":" + portNumber;
            throw new MyNetworkException(msg, ex);
        }

        debug.println("connection successful");
        final AuthenticationRequest req = AuthenticationRequest.newBuilder()
            .setUsername(System.getProperty("user.name"))
            .setVersion(Version.getClientVersion())
            .setWindow(windowName)
            .build();

        byte[] buf = this.sendRecvBytes(req.toByteArray());
        buf = decompressZLIB(buf);

        final AuthenticationReply reply;
        try {
            reply = AuthenticationReply.parseFrom(buf);
        } catch (Exception ex) {
            throw new MyNetworkException("unable to parse reply from server", ex);
        }

        if (reply.getCode() == AuthenticationStatusCode.AUTH_SUCCESS)
            return true;

        // upgrade is required, send a message to the user
        {
            final String msg = "RTD Client Version Mismatch";
            final StringBuffer sb = new StringBuffer();
            sb.append("The RTD server replied that it does not know how to\n");
            sb.append("speak with your client. Please upgrade your client to\n");
            sb.append("the latest version available from:\n");
            sb.append("\n");
            sb.append("http://cedarflat.mmarray.org/observing/tools/rtd.html\n");
            sb.append("\n");
            sb.append("Sorry for the inconvenience.\n");
            throw new MyNetworkException(msg, sb.toString());
        }
    }

    /**
     * Shutdown our connecton to the C++ server by sending a zero-byte message.
     */
    public synchronized void close() {
        debug.println("shutting down");

        try {
            final UIMessageRequest req = UIMessageRequest.newBuilder()
                .setCode(UIMessageRequestCode.REQ_EXIT)
                .setSleeptime(0)
                .build();

            this.sendRecvBytes(req.toByteArray());

            // zero byte message is a shutdown message
            //os.writeInt(0);
            sock.close();

            sock = null;
            os = null;
            is = null;
        } catch (Exception ex) {
            // stifle any errors which happen here, they don't matter
        }
    }

    /**
     * Send a message to the server and block until a reply arrives.
     *
     * This is a very low level routine which deals exclusively with byte
     * arrays. This is not meant to be used by general users of this class:
     * specialized sendRecv() message pairs should be used instead.
     *
     * The message format is a 32-bit integer (in network byte order) containing
     * the size of the bytes remaining in the message, followed by the data
     * bytes themselves.
     *
     * @param req the bytes to send
     * @return the bytes received or null
     */
    private synchronized byte[] sendRecvBytes(final byte[] req) throws MyNetworkException {
        debug.println("sendRecv: req=" + req);

        try {
            os.writeInt(req.length);
            os.write(req);
        } catch (Exception ex) {
            final String msg = "Error sending message to server";
            throw new MyNetworkException(msg, ex);
        }

        debug.println("sendRecv: reading from input stream");
        try {
            final int nBytes = is.readInt();
            debug.println("sendRecv: receiving length=" + nBytes);
            if (nBytes > (1 << 20)) {
                debug.println("sendRecv: server sends packet too large: " + nBytes + " bytes");
                final String badData = debugReadServerReply(nBytes);
                throw new RuntimeException("Bad Data From Server: " + badData);
            }

            final byte[] buf = new byte[nBytes];
            is.readFully(buf, 0, nBytes);

            debug.println("sendRecv: received length=" + buf.length);
            debug.println("sendRecv: received bytes=" + buf);

            return buf;
        } catch (Exception ex) {
            final String msg = "Error while reading reply from server";
            throw new MyNetworkException(msg, ex);
        }
    }

    /**
     * Debug helper to help catch and debug server errors.
     *
     * Developers constantly forget that it is completely illegal to send
     * any sort of output to stdout or stderr from RTD programs. This includes
     * all classes that they use, which can be far removed from the RTD code
     * itself.
     *
     * To help debug this problem, when we detect ASCII in the "length"
     * parameter of the RTD protocol, we continue reading until we encounter
     * a '\0' or '\n'. This should help track down any offender with ease.
     */
    private synchronized String debugReadServerReply(final int nBytes) {
        final StringBuffer sb = new StringBuffer();

        // decode the 4 byte integer into a character string
        sb.append((char)((nBytes & 0xff000000) >> 24));
        sb.append((char)((nBytes & 0x00ff0000) >> 16));
        sb.append((char)((nBytes & 0x0000ff00) >>  8));
        sb.append((char)((nBytes & 0x000000ff) >>  0));

        try {
            while (true) {
                final char c = (char)is.readByte();
                if (c == '\n' || c == '\0')
                    break;

                sb.append(c);
            }
        } catch (Exception ex) {
            // stifle: the protocol is dead, we don't care what happens
        }

        return sb.toString();
    }

    /**
     * Compress a buffer of bytes using LZMA
     * @param src the buffer of bytes to compress
     */
    private static byte[] compressLZMA(final byte[] src) {
        try {
            final ByteArrayOutputStream out = new ByteArrayOutputStream();
            final XZOutputStream xzout = new XZOutputStream(out, new LZMA2Options());

            try {
                xzout.write(src, 0, src.length);
            } finally {
                xzout.close();
                return out.toByteArray();
            }
        } catch (XZFormatException ex) {
            System.err.println("XZFormatException: " + ex);
            System.exit(1);
        } catch (CorruptedInputException ex) {
            System.err.println("CorruptedInputException: " + ex);
            System.exit(1);
        } catch (UnsupportedOptionsException ex) {
            System.err.println("UnsupportedOptionsException: " + ex);
            System.exit(1);
        } catch (EOFException ex) {
            System.err.println("EOFException: " + ex);
            System.exit(1);
        } catch (IOException ex) {
            System.err.println("IOException: " + ex);
            System.exit(1);
        }

        return null;
    }

    /**
     * Decompress a buffer of bytes using LZMA
     * @param src the buffer of bytes to decompress
     */
    private static byte[] decompressLZMA(final byte[] src) {
        try {
            InputStream in = new ByteArrayInputStream(src);
            try {
                final int memlimit = 32 << 20; // 32 Megabytes
                in = new XZInputStream(in, memlimit);

                final ByteArrayOutputStream out = new ByteArrayOutputStream();
                byte[] buf = new byte[4096];

                while (true) {
                    final int len = in.read(buf);
                    if (len > 0)
                        out.write(buf, 0, len);

                    // end of file is reached
                    if (len == -1)
                        return out.toByteArray();
                }
            } finally {
                in.close();
            }
        } catch (XZFormatException ex) {
            System.err.println("XZFormatException: " + ex);
            System.exit(1);
        } catch (CorruptedInputException ex) {
            System.err.println("CorruptedInputException: " + ex);
            System.exit(1);
        } catch (UnsupportedOptionsException ex) {
            System.err.println("UnsupportedOptionsException: " + ex);
            System.exit(1);
        } catch (EOFException ex) {
            System.err.println("EOFException: " + ex);
            System.exit(1);
        } catch (IOException ex) {
            System.err.println("IOException: " + ex);
            System.exit(1);
        }

        return null;
    }

    private static byte[] decompressZLIB(final byte[] src) throws MyNetworkException {
        try {
            final Inflater inflater = new Inflater();
            inflater.setInput(src);

            final ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buf = new byte[4096];

            while (true) {
                final int len = inflater.inflate(buf);
                if (len > 0)
                    out.write(buf, 0, len);

                if (inflater.finished())
                    return out.toByteArray();
            }
        } catch (DataFormatException ex) {
            final String msg = "Error during ZLIB decompression";
            throw new MyNetworkException(msg, ex);
        }
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
