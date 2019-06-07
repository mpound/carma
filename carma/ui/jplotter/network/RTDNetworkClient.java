package carma.ui.jplotter.network;

import java.io.*;
import java.util.*;
import java.net.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;

// XZ Decompression
import org.tukaani.xz.*;

enum DataType {
	BOOL,					// 7368 monitor points
	COMPLEX_FLOAT,			// 7392 monitor points
	DATE,					// 1 monitor point (array.frame.utc[0])
	DOUBLE,					// 11063 monitor points
	FLOAT,					// 41681 monitor points
	INT,					// 43859 monitor points
	SHORT,					// 5957 monitor points
	STRING,					// 5455 monitor points
	UCHAR,					// 1924 monitor points
	UINT,					// 2 monitor points (array.frame.nsnap[0], array.frame.record[0])
}

class DataEntry {
	private int id;
	private final String name;
	private final DataType type;
	private final int nitems;
	private int nusers; // number of users (reference counter)

	public DataEntry(String s) {
		final String[] elements = s.split(" ");
		if (elements.length != 3)
			throw new RuntimeException("unable to parse DataEntry");

		this.id = Integer.valueOf(elements[0]);
		this.name = stripDimension(elements[1]);
		this.type = DataType.valueOf(elements[2]);
		this.nitems = parseDimension(elements[1]);
		this.nusers = 0;
	}

	public DataEntry(int id, String name, DataType type) {
		this.id = id;
		this.name = name;
		this.type = type;
		this.nitems = parseDimension(this.name);
		this.nusers = 0;
	}

	@Override
	public String toString() {
		if (this.nitems > 1)
			return id + " " + name + "[0-" + (this.nitems + 1) + "] " + type;

		return id + " " + name + "[0] " + type;
	}

	public void setId(final int id) {
		this.id = id;
	}

	public int getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public DataType getType() {
		return this.type;
	}

	public int getNumItems() {
		return this.nitems;
	}

	public void addUser() {
		this.nusers++;
	}

	public void remUser() {
		this.nusers--;
	}

	public int getNumUsers() {
		return this.nusers;
	}

	public boolean isGraphable() {
		switch (this.type) {
		case BOOL:
		case COMPLEX_FLOAT:
		case DOUBLE:
		case FLOAT:
		case INT:
		case SHORT:
		case UCHAR:
		case UINT:
			return true;
		default:
			return false;
		}
	}

	private int parseDimension(final String name) {
		final int openParen = name.lastIndexOf("[");
		final int closeParen = name.lastIndexOf("]");

		if (openParen == -1 || closeParen == -1)
			throw new RuntimeException("dimension brackets not found");

		if (closeParen <= (openParen + 1))
			throw new RuntimeException("dimension brackets overlap");

		String dimension = name.substring(openParen + 1, closeParen);

		final int dashMark = dimension.lastIndexOf("-");
		if (dashMark != -1)
			dimension = dimension.substring(dashMark + 1);

		return Integer.parseInt(dimension) + 1;
	}

	private String stripDimension(final String name) {
		final int openParen = name.lastIndexOf("[");

		if (openParen == -1)
			return name;

		return name.substring(0, openParen);
	}
}

enum OperationType {
	ADD,
	REM,
};

class EntryOperation {
	public final OperationType type;
	public final DataEntry entry;

	public EntryOperation(OperationType type, DataEntry entry) {
		this.type = type;
		this.entry = entry;
	}
}

class NameOperation {
	 public final OperationType type;
	 public final String name;

	 public NameOperation(OperationType type, String name) {
		 this.type = type;
		 this.name = name;
	 }
}

public final class RTDNetworkClient implements Runnable {
	// Message Types
	private final static int MESSAGE_REGISTER_MAP = 1;
	private final static int MESSAGE_DATA_BYTES = 2;
	private final static int MESSAGE_ADDREG = 3;
	private final static int MESSAGE_REMREG = 4;
	private final static int MESSAGE_ADDREG_ACK = 5;
	private final static int MESSAGE_REMREG_ACK = 6;

	// Network Connection Parameters
	private final String hostname;
	private final int port;

	// TCP Socket
	// LOCKING: only changed through connect()/disconnect() which are
	// LOCKING: synchronized methods
	private Socket socket = null;

	// Shutdown Notification
	// LOCKING: only written by shutdown(), only read by run() thread
	// LOCKING: Java primitives are already atomic
	private AtomicBoolean shutdown = new AtomicBoolean(false);

	// Data Listeners
	// LOCKING: synchronized add/remove/notify
	private final ArrayList<GraphDataListener> listeners = new ArrayList<GraphDataListener>();

	// Internal Add/Remove Queue
	// LOCKING: synchronized add/remove/connect
	private final Queue<NameOperation> nameQueue = new ConcurrentLinkedQueue<NameOperation>();

	// Internal Add/Remove "Waiting for ACK" Queue
	// LOCKING: only changed through run() thread
	private final Queue<EntryOperation> ackQueue = new ConcurrentLinkedQueue<EntryOperation>();

	// Internal DATA_BYTES Decoder
	// LOCKING: only changed through the run() thread
	private final List<DataEntry> decodeEntries = new ArrayList<DataEntry>();

	// Map for quick lookup of register name -> DataEntry
	// LOCKING: only changed through the run() thread
	private Map<String, DataEntry> availableMap = new HashMap<String, DataEntry>(100000);

	// List for providing to clients for searches
	// LOCKING: provided by lock below
	private final Lock searchLock = new ReentrantLock();
	private List<String> searchList = new ArrayList<String>();

	/* ---------------------------------------------------------------------- */
	/* Public API                                                             */
	/* ---------------------------------------------------------------------- */

	public RTDNetworkClient(final String hostname, final int port) {
		this.hostname = hostname;
		this.port = port;

		// this is the frame number, and should always be present
		this.addRegister("array.frame.record");
	}

	public void shutdown() {
		// set the shutdown flag
		this.shutdown.set(true);

		// make any blocking readers of this socket throw a SocketException
		this.disconnect();
	}

	/**
	 * Start receiving data for a register.
	 */
	public synchronized void addRegister(final String name) {
		this.nameQueue.add(new NameOperation(OperationType.ADD, name));
	}

	/**
	 * Stop receiving data for a register.
	 */
	public synchronized void remRegister(final String name) {
		this.nameQueue.add(new NameOperation(OperationType.REM, name));
	}

	/*
	 * TODO FIXME:
	 *
	 * When this changes, you need to construct a new DataEvent with a COPY
	 * of the list and send it to whoever cares. They should be in charge of
	 * maintaining their own copy, and correctly sending back any
	 * DataEntry they wish to receive.
	 *
	 * A good double check should be that any requested DataEntry ID matches
	 * one in the availableList list. Just in case something stale gets through.
	 */
	public List<String> getRegisterList() {
		this.searchLock.lock();
		try {
			return new ArrayList<String>(this.searchList);
		} finally {
			this.searchLock.unlock();
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Runnable Interface                                                     */
	/* ---------------------------------------------------------------------- */

	public void run() {
		// keep reconnecting while instructed
		while (this.shutdown.get() == false) {

			{
				// notify listeners that we are disconnected
				final GraphDataEvent gde = new GraphDataEvent(this, false);
				notifyGraphDataListeners(gde);
			}

			// (re)connect to the server, automatically reconstructing the
			// previous state of communications
			try {
				this.connect();
			} catch (UnknownHostException e) {
				System.err.println("Don't know about host: " + this.hostname);
				this.sleep(1000);
				continue;
			} catch (IOException e) {
				System.err.println("Couldn't get I/O for the connection to: " + this.hostname);
				this.sleep(1000);
				continue;
			}

			{
				// notify listeners that we are connected
				final GraphDataEvent gde = new GraphDataEvent(this, true);
				notifyGraphDataListeners(gde);
			}

			// process data as it arrives
			try {
				// create input/output streams
				final DataInputStream in = new DataInputStream(this.socket.getInputStream());
				final DataOutputStream out = new DataOutputStream(this.socket.getOutputStream());

				// run the receive/transmit loop until an error occurs
				while (this.shutdown.get() == false) {
					handle_rx(in);
					handle_tx(out);
				}
			} catch (SocketException ex) {
				System.err.println("handle_rx SocketException: " + ex);
			} catch (IOException ex) {
				System.err.println("handle_rx IOException: " + ex);
			}
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Public GraphDataListener API                                           */
	/* ---------------------------------------------------------------------- */

	public synchronized void addGraphDataListener(final GraphDataListener listener) {
		listeners.add(listener);
	}

	public synchronized void removeGraphDataListener(final GraphDataListener listener) {
		listeners.remove(listener);
	}

	private synchronized void notifyGraphDataListeners(final GraphDataEvent gde) {
		// notify all waiting listeners
		for (final GraphDataListener l : listeners) {
			l.graphDataChanged(gde);
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Private API                                                            */
	/* ---------------------------------------------------------------------- */

	/**
	 * Simplified wrapper around Thread.sleep().
	 */
	private void sleep(int milliseconds) {
		try {
			Thread.sleep(milliseconds);
		} catch (InterruptedException e) {
			// Restore the interrupted status
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Fully skip a number of bytes.
	 *
	 * This works just like DataInputStream::readFully(), except that bytes are
	 * skipped instead.
	 *
	 * @param n the number of bytes to skip
	 */
	private static void skipFully(final DataInputStream in, int n) throws IOException {
		while (n > 0) {
			n -= in.skipBytes(n);
		}
	}

	/**
	 * Connect to the specified host/port and setup Input/Output streams.
	 *
	 * This method includes the code to automatically re-send all of the
	 * necessary ADDREG messages to reconstruct the previous state of
	 * communications with the server as if nothing had happened.
	 */
	private synchronized void connect() throws IOException {

		// Dump the existing register map
		this.availableMap.clear();

		// Pretend the server ACK'd everything we requested it to do
		for (final EntryOperation eop : this.ackQueue) {
			switch (eop.type) {
			case ADD:
				this.decodeEntries.add(eop.entry);
				break;
			case REM:
				this.decodeEntries.remove(eop.entry);
				break;
			}
		}

		// Drop all entries: we have processed them
		this.ackQueue.clear();

		// Create a new list of ADD/REM commands such that we will duplicate
		// the existing state that we had before we were disconnected
		final List<NameOperation> regList = new ArrayList<NameOperation>();

		// Add each current entry to the list of monitor points to use.
		// The user count is retained by adding each point the correct
		// number of times it is currently used.
		for (final DataEntry entry : this.decodeEntries) {
			for (int i = 0; i < entry.getNumUsers(); i++) {
				regList.add(new NameOperation(OperationType.ADD, entry.getName()));
			}
		}

		this.decodeEntries.clear();

		// Add the pending name entry queue as well
		for (final NameOperation nameop : this.nameQueue) {
			regList.add(nameop);
		}

		this.nameQueue.clear();

		// Move everything from the temporary register list onto the queue
		// which will be transmitted when we reconnect
		for (final NameOperation nameop : regList) {
			this.nameQueue.add(nameop);
		}

		// reconnect the socket
		this.socket = new Socket(this.hostname, this.port);

		// attempt some extra speed
		this.socket.setReceiveBufferSize(64 * 1024);
		this.socket.setSendBufferSize(64 * 1024);
	}

	/**
	 * Disconnect the Socket and shutdown Input/Output streams.
	 */
	private synchronized void disconnect() {
		if (this.socket != null) {
			try {
				this.socket.close();
			} catch (IOException ex) {
				// stifle
			} finally {
				this.socket = null;
			}
		}
	}

	private static List<String> decompressXZMessage(final byte[] buf) {
		List<String> list = new ArrayList<String>();

		try {
			InputStream in = new ByteArrayInputStream(buf);
			try {
				in = new XZInputStream(in);
				in = new BufferedInputStream(in);
				Scanner scanner = new Scanner(in, "UTF-8");
				while (scanner.hasNextLine()) {
					list.add(scanner.nextLine());
				}
				scanner.close();
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

		return list;
	}

	/* ---------------------------------------------------------------------- */
	/* Individual Message Handling                                            */
	/* ---------------------------------------------------------------------- */

	/**
	 * Handle a REGISTER_MAP mesage
	 *
	 * @param buf the buffer holding the XZ-compressed bytes to decode
	 */
	private void handle_message_register_map(final byte[] buf) {
		//System.out.println("--- Register Map ---");

		// If the availableMap is already populated, then we should
		// disconnect and re-connect to ensure that all object references
		// are re-allocated and looked up with the standard code path.
		//
		// This makes the code much simpler than trying to update everything
		// that is currently in flight in our conversation with the server.
		//
		// It also tests that the transparent reconnect functionality works
		// as expected.
		if (!this.availableMap.isEmpty()) {
			this.disconnect();
			return;
		}

		final List<String> rawList = decompressXZMessage(buf);

		// The locking here ONLY protects searchList. The extra stuff under the
		// lock is just a side effect of making this code efficient by going
		// through the loop only once.
		this.searchLock.lock();
		try {
			this.searchList.clear();

			for (final String elem : rawList) {
				// Erik's format has an extra ".Subsystem" compared to the
				// CARMA representation that users are familiar with
				final String s = elem.replace(".Subsystem.", ".");

				final DataEntry entry = new DataEntry(s);

				// skip non-graphable entries
				if (!entry.isGraphable())
					continue;

				// update the availableMap
				this.availableMap.put(entry.getName(), entry);

				// update the searchList
				this.searchList.add(entry.getName());
			}
		} finally {
			this.searchLock.unlock();
		}
	}

	/**
	 * Handle a DATA_BYTES message
	 *
	 * @param buf the buffer holding the data bytes payload to decode
	 */
	private void handle_message_data_bytes(final byte[] buf) {
		//System.out.println("--- Data Bytes ---");

		// Data Storage for this data set
		GraphDataEvent gde = new GraphDataEvent(this, true);

		try {
			final DataInputStream in = new DataInputStream(new ByteArrayInputStream(buf));

			// skip message size bytes
			skipFully(in, 4);

			for (DataEntry e : decodeEntries) {
				final GraphDataEvent.Datum datum = gde.getDatum(e.getName());
				switch (e.getType()) {
				case BOOL:
					for (int i = 0; i < e.getNumItems(); i++) {
						final boolean bool = in.readBoolean();
						//System.out.println(e + ": " + bool);
						datum.addData((bool ? 1 : 0));
					}
					datum.setValidity(in.readBoolean());
					break;
				case COMPLEX_FLOAT:
					for (int i = 0; i < e.getNumItems(); i++) {
						final float real = in.readFloat();
						final float imag = in.readFloat();
						//System.out.println(e + ": " + real + " , " + imag);

						// TODO FIXME: we choose the "amplitude" instead of the
						// TODO FIXME: "phase" because the monitor system does so
						datum.addData(Math.atan2(imag, real));
					}
					datum.setValidity(in.readBoolean());
					break;
				case DATE:
					for (int i = 0; i < e.getNumItems(); i++) {
						final int mjdsec = in.readInt();
						final int mjdmsec = in.readInt();
						//System.out.println(e + ": " + mjdsec + " , " + mjdmsec);
						// NOT GRAPHABLE
					}
					datum.setValidity(in.readBoolean());
					break;
				case DOUBLE:
					for (int i = 0; i < e.getNumItems(); i++) {
						final double dbl = in.readDouble();
						//System.out.println(e + ": " + dbl);
						datum.addData(dbl);
					}
					datum.setValidity(in.readBoolean());
					break;
				case FLOAT:
					for (int i = 0; i < e.getNumItems(); i++) {
						final float flt = in.readFloat();
						//System.out.println(e + ": " + flt);
						datum.addData(flt);
					}
					datum.setValidity(in.readBoolean());
					break;
				case INT:
					for (int i = 0; i < e.getNumItems(); i++) {
						final int pint = in.readInt();
						//System.out.println(e + ": " + pint);
						datum.addData(pint);
					}
					datum.setValidity(in.readBoolean());
					break;
				case SHORT:
					for (int i = 0; i < e.getNumItems(); i++) {
						final short shrt = in.readShort();
						//System.out.println(e + ": " + shrt);
						datum.addData(shrt);
					}
					datum.setValidity(in.readBoolean());
					break;
				case STRING:
					final byte[] bytes = new byte[e.getNumItems()];
					in.readFully(bytes);
					final String str = new String(bytes, "US-ASCII");
					//System.out.println(e + ": " + str);
					// NOT GRAPHABLE
					datum.setValidity(in.readBoolean());
					break;
				case UCHAR:
					for (int i = 0; i < e.getNumItems(); i++) {
						final int chr = in.readUnsignedByte();
						//System.out.println(e + ": " + chr);
						datum.addData(chr);
					}
					datum.setValidity(in.readBoolean());
					break;
				case UINT:
					for (int i = 0; i < e.getNumItems(); i++) {
						final int uint = in.readInt();
						//System.out.println(e + ": " + uint);
						datum.addData(uint);
					}
					datum.setValidity(in.readBoolean());
					break;
				default:
					System.err.println("*** ERROR: unknown type ***");
					break;
				}
			}

			// notify all listeners about new data
			this.notifyGraphDataListeners(gde);

		} catch (IOException e) {
			System.err.println("IOException in ByteArrayInputStream: " + e);
		}
	}

	/**
	 * Handle an ADDREG_ACK message
	 *
	 * @param buf an empty buffer (no bytes). This parameter is unused.
	 */
	private void handle_message_addreg_ack(final byte[] buf) {
		//System.out.println("--- Add Register ACK ---");
		final EntryOperation eop = this.ackQueue.remove();
		if (eop.type != OperationType.ADD)
			throw new RuntimeException("handle_addreg_ack: bad operation order");

		decodeEntries.add(eop.entry);
	}

	/**
	 * Handle a REMREG_ACK message
	 *
	 * @param buf an empty buffer (no bytes). This parameter is unused.
	 */
	private void handle_message_remreg_ack(final byte[] buf) {
		//System.out.println("--- Rem Register ACK ---");
		final EntryOperation eop = this.ackQueue.remove();
		if (eop.type != OperationType.REM)
			throw new RuntimeException("handle_remreg_ack: bad operation order");

		decodeEntries.remove(eop.entry);
	}

	/* ---------------------------------------------------------------------- */
	/* Message Transmission Helpers                                           */
	/* ---------------------------------------------------------------------- */

	private void send_message_addreg(final DataOutputStream out, final DataEntry entry) throws IOException {
		ackQueue.add(new EntryOperation(OperationType.ADD, entry));

		out.writeInt(16);				// 16 bytes total
		out.writeInt(8);				// 8 bytes follow
		out.writeInt(MESSAGE_ADDREG);	// message type 3 (add mp)
		out.writeInt(entry.getId());	// monitor point id
		out.flush();					// force output now
	}

	private void send_message_remreg(final DataOutputStream out, final DataEntry entry) throws IOException {
		ackQueue.add(new EntryOperation(OperationType.REM, entry));

		out.writeInt(16);
		out.writeInt(8);
		out.writeInt(MESSAGE_REMREG);
		out.writeInt(entry.getId());
		out.flush();
	}

	/* ---------------------------------------------------------------------- */
	/* Toplevel Message Handler                                               */
	/* ---------------------------------------------------------------------- */

	/**
	 * Handle a single received toplevel message.
	 *
	 * @param in the input data stream (RX messages from server)
	 */
	private void handle_rx(final DataInputStream in) throws IOException {
		//System.out.println("\nWaiting for input");

		// total message size
		final int total_message_size = in.readInt();
		//System.out.printf("Total Message Size: %d\n", total_message_size);
		if (total_message_size <= 4)
			return;

		// remaining message size
		final int remain_message_size = in.readInt();
		//System.out.printf("Remaining Message Size: %d\n", remain_message_size);
		if (remain_message_size <= 0)
			return;

		// sanity check against malicious applications (2MB limit)
		if (remain_message_size > (2 << 20)) {
			System.out.printf("Excessive size (malicious server?): %d bytes, skipping\n", remain_message_size);
			skipFully(in, remain_message_size);
			return;
		}

		// message type
		final int message_type = in.readInt();
		//System.out.printf("Message Type: %d\n", message_type);

		// payload size
		final int payload_size = remain_message_size - 4;

		// read payload
		final byte[] buf = new byte[payload_size];
		//System.out.printf("Reading %d byte payload\n", payload_size);
		in.readFully(buf);
		//System.out.printf("Finished reading payload\n");

		/*
		for (int i = 0; i < payload_size; i++) {
			System.out.printf("buf[%d]=%x\n", i, buf[i]);
		}
		*/

		switch (message_type) {
		case MESSAGE_REGISTER_MAP:
			handle_message_register_map(buf);
			break;
		case MESSAGE_DATA_BYTES:
			handle_message_data_bytes(buf);
			break;
		case MESSAGE_ADDREG_ACK:
			handle_message_addreg_ack(buf);
			break;
		case MESSAGE_REMREG_ACK:
			handle_message_remreg_ack(buf);
			break;
		default:
			System.out.println("*** UNKNOWN MESSAGE TYPE: " + message_type);
			for (int i = 0; i < payload_size; i++) {
				System.out.printf("buf[%d]=%x\n", i, buf[i]);
			}
			break;
		}
	}

	/**
	 * Find an entry in the availableMap.
	 *
	 * @param name the register name to find
	 * @return the DataEntry or null
	 */
	private DataEntry findAvailableEntry(final String name) {
		return this.availableMap.get(name);
	}

	/**
	 * Check that an OperationType/DataEntry pair is present in the ACK queue.
	 *
	 * @param type the OperationType to find
	 * @param entry the DataEntry to find
	 * @return true if the entry is in the ack queue, false otherwise
	 */
	private boolean isEntryInAckQueue(final OperationType type, final DataEntry entry) {
		for (final EntryOperation eop : this.ackQueue) {
			if (eop.type == type && eop.entry == entry) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Handle transmission of any pending messages.
	 *
	 * @param out the output data stream (TX messages to server)
	 */
	private void handle_tx(final DataOutputStream out) throws IOException {

		// No available entries means nothing to look up, return early
		if (this.availableMap.isEmpty())
			return;

		// Look up and add/remove registers
		while (true) {
			final NameOperation nameop = this.nameQueue.poll();
			if (nameop == null)
				break;

			// find the DataEntry corresponding to this name
			final DataEntry entry = this.findAvailableEntry(nameop.name);
			if (entry == null) {
				System.err.println("register not available: " + nameop.name);
				continue;
			}

			if (nameop.type == OperationType.ADD) {
				// if it is in the decode list, increment user count, done
				if (this.decodeEntries.contains(entry)) {
					entry.addUser();
					continue;
				}

				// if it is in the ack queue, increment user count, done
				if (isEntryInAckQueue(OperationType.ADD, entry)) {
					entry.addUser();
					continue;
				}

				// not in either queue, send add message to server
				entry.addUser();
				send_message_addreg(out, entry);
			}

			if (nameop.type == OperationType.REM) {
				// the entry should definitely be in the decode list
				if (!this.decodeEntries.contains(entry)) {
					System.err.println("register not being decoded: " + nameop.name);
					continue;
				}

				// decrement the user count
				entry.remUser();
			}
		}

		// search the decoder for stale entries
		for (final DataEntry entry : this.decodeEntries) {
			// skip entries which have users
			if (entry.getNumUsers() != 0)
				continue;

			// if the entry is in the ack queue, then we have already processed
			// it appropriately
			if (isEntryInAckQueue(OperationType.REM, entry))
				continue;

			// we have not processed it yet, send rem message to server
			send_message_remreg(out, entry);
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
