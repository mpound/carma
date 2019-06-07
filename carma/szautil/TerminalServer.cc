#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/SerialClient.h"
#include "carma/szautil/String.h"
#include "carma/szautil/StringUtils.h"
#include "carma/szautil/TcpClient.h"
#include "carma/szautil/TcpListener.h"
#include "carma/szautil/TerminalServer.h"
#include "carma/szautil/Vector.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor for a socket connection
 */
TerminalServer::TerminalServer(std::string ipAddress, unsigned port, bool spawnThread) : Runnable(spawnThread, runFn)
{
  initMembers();

  setTcpIp(ipAddress, port);

  spawn(this);
}

/**.......................................................................
 * Constructor for a serial connection
 */
TerminalServer::TerminalServer(unsigned baudRate, std::string serialPort, bool spawnThread) : Runnable(spawnThread, runFn)
{
  initMembers();

  setSerial(baudRate, serialPort);

  spawn(this);
}

/**.......................................................................
 * Listen for connection requests on the specified port
 */
void TerminalServer::listen(unsigned port, unsigned nClients) 
{
  // Listen on the requested socket.
  
  if(nClients > 0) {
    listener_ = new TcpListener(port, nClients);
    fdSet_.registerReadFd(listener_->getFd());
  }
}

/**.......................................................................
 * Initialize members
 */
void TerminalServer::initMembers()
{
  listener_   =  0;
  file_       =  0;
  port_       =  0;
  serialPort_ =  0;
  signalTask_ =  0;
  stop_       =  false;
  tcpipPort_  =  0;
  pending_    =  false;
  timeOut_    =  0;
  hammering_  =  false;
  waitingToHammer_  =  false;

  hammers_.insert(hammers_.end(), "7H"); // keyboard mode
  hammers_.insert(hammers_.end(), "*1"); // Table 1
  hammers_.insert(hammers_.end(), "0");  // Set execution interval to 0
  hammers_.insert(hammers_.end(), "*0"); // recompile

  //  for(std::list<std::string>::iterator iham=hammers_.begin();
  //      iham != hammers_.end(); iham++) {
  //    cout << *iham << endl;
  //  }

  timeVal_.setTime(0, 0);

  // Initialize the logfile prefix to something
  
  logFile_.setPrefix("TerminalServer");
  logFile_.setDirectory(".");

  // Finally, spawn a thread for managing signal handling

  signalTask_ = new SignalTask(true);

  // And install a signal handler for SIGINT

  signalTask_->sendInstallSignalMsg(SIGINT, &shutDown, this);
}

/**.......................................................................
 * Destructor.
 */
TerminalServer::~TerminalServer() 
{
  // Shut down the signal task

  if(signalTask_ != 0) {
    delete signalTask_;
    signalTask_ = 0;
  }

  // Free any memory allocated in this class

  for(std::list<NetTermHandler*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++) 
    delete *iclient;

  // Free the listener

  if(listener_ != 0) {
    delete listener_;
    listener_ = 0;
  }
}

/**.......................................................................
 *  Set up a TCP/IP connection
 */
void TerminalServer::setTcpIp(std::string host, unsigned port)
{
  std::cout << "Connecting to port: " << port << std::endl;

  tcpipPort_ = new TcpClient(host, port);
  tcpipPort_->connectToServer(true);
  
  port_ = tcpipPort_;

  initPort();
  fdSet_.registerReadFd(port_->getFd());
}

/**.......................................................................
 *  Set up a serial port connection
 */
void TerminalServer::setSerial(unsigned baudRate, std::string port)
{
  serialPort_ = new SerialClient(port, baudRate);
  serialPort_->connect();
  
  port_ = serialPort_;
  
  initPort();
}

/**.......................................................................
 * Block in select
 */
void TerminalServer::run()
{
  LogStream errStr;

  do {

    // Block in select() until one or more file descriptors are readable
  
    if(select(fdSet_.size(), fdSet_.readFdSet(), NULL, NULL, timeOut_) < 0) {
      errStr.appendSysError(true, "select()");
      throw Error(errStr);
    }
    
    // Service requests received over the socket connection
    
    if(listener_ != 0 && fdSet_.isSetInRead(listener_->getFd()))
      acceptConnection();
    
    // If stdin was set, read the command and send it to the port
    
    if(fdSet_.isSetInRead(0))
      readFromStdin();
    
    // If we received data from the serial port, read it and fan it out
    // to connected clients
    
    if(port_ != 0 && fdSet_.isSetInRead(port_->getFd()))
      readFromPort();
    
    // Check connected clients for data
    
    checkClients();

    // Did we time out?  Check if we have an open file.
    
    if(file_ != 0)
      readFromFile();

    // If we are hammering the datalogger, chuck CR at it until it
    // responds

    if(waitingToHammer_) {
      cout << "hammering" << endl;
      std::string cr("");
      writeToPort(cr);
    }

  } while(!stop_);
}

/**.......................................................................
 * Set whether or not to register stdin to be listened to
 */
void TerminalServer::listenToStdin(bool listen)
{
  if(listen)
    fdSet_.registerReadFd(0);
  else
    fdSet_.clearFromReadFdSet(0);
}

/**.......................................................................
 * Set whether or not to log server traffic
 */
void TerminalServer::log(bool log)
{
  // Open the logfile
  
  logFile_.open();
}

/**.................................................................................
 * Set the logfile prefix
 */
void TerminalServer::setLogFilePrefix(const std::string& prefix)
{
  logFile_.setPrefix(prefix);
}

/**.................................................................................
 * Set the logfile directory
 */
void TerminalServer::setLogFileDirectory(const std::string& dir)
{
  logFile_.setDirectory(dir);
}

/**.......................................................................
 * Strip arbitrary characters from received lines
 */
void TerminalServer::strip(std::string strip)
{
  port_->strip(strip);
}

/**.......................................................................
 * Dont strip arbitrary characters from received lines
 */
void TerminalServer::dontStrip(std::string dontStrip)
{
  port_->dontStrip(dontStrip);
}

/**.......................................................................
 * Strip unprintable characters from received lines
 */
void TerminalServer::stripUnprintable(bool strip)
{
  port_->stripUnprintable(strip);
}

/**.......................................................................
 * Append arbitrary characters to written lines
 */
void TerminalServer::append(std::string append)
{
  port_->append(append);
}

/**.......................................................................
 * Respond to a connection request from a client
 */
void TerminalServer::acceptConnection()
{
  int fd = -1;
  
  // Allow the caller to connect.  The fd returned will be configured
  // for blocking I/O.
  
  fd = listener_->acceptConnection(true);
  
  // Attach this file descriptor to the first empty slot in the list
  // of descriptors from which we are waiting for responses.
  
  NetTermHandler* client = new NetTermHandler(fd);

  clients_.insert(clients_.begin(), client);
  
  // And register the descriptor to be watched for input.  The
  // antenna should send us its ID after the connection is made.
  
  fdSet_.registerReadFd(fd);
  
  return;
}

/**.......................................................................
 * Check clients for data to be read
 */
void TerminalServer::checkClients() 
{
  std::vector<NetTermHandler*> disconnectedClients_;

  for(std::list<NetTermHandler*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++) 
  {
    NetTermHandler* client = *iclient;

    if(fdSet_.isSetInRead(client->getFd())) {
      readFromClient(client);

      // If after processing messages from this client, the client is
      // disconneted, mark it for removal

      if(client->getFd() < 0)
	disconnectedClients_.push_back(client);
    }
  }

  // Finally, remove any clients that were disconnected after reading

  for(std::vector<NetTermHandler*>::iterator iclient=disconnectedClients_.begin();
      iclient != disconnectedClients_.end(); iclient++) 
  {
    NetTermHandler* client = *iclient;

    clients_.remove(client);

    delete client;
  }
}

/**.......................................................................
 * Read data from a client socket
 */
void TerminalServer::readFromClient(NetTermHandler* client) 
{
  LogStream errStr;

  // Perform a blocking read on the client

  switch(client->read()) {

    // With the client configured for blocking I/O, we should only
    // ever get NET_READ_DONE back
    
  case NetReadStr::NET_READ_DONE:
    
    // Check the type of the message that was just read

    switch(client->getLastMsgType()) {
      
      // A line of text was read from a client

    case NetTermHandler::LINE:
      {
	std::string line = client->getLastLine();

	// Write the line read from the client to the port

	writeToPort(line);

	// Write the line read from the client to stdout

	writeToStdout(line);

	// And log it to the file

	logFile_.append(line);
      }
      break;

      // A client wishes to disconnect

    case NetTermHandler::DISCONNECT:

      logFile_.append("A client has disconnected");

      fdSet_.clearFromReadFdSet(client->getFd());

      break;
    default:
      errStr.appendMessage(true, "Unrecognized client message");
      throw Error(errStr);
      break;
    }
    break;

  default:
    errStr.appendMessage(true, "Error reading client data");
    throw Error(errStr);
    break;
  }
}

/**.......................................................................
 * Read data from stdin
 */
void TerminalServer::readFromStdin()
{
  Port stdinPort(0);
  std::string line = stdinPort.readString();

  // If this was a special command, return without doing anything.  If
  // not, send it to the port

  if(!parseCommand(line))
    port_->writeString(line);

  // Make sure the string contains exactly one newline

  std::string logLine = line;
  sza::util::strip(logLine, '\n');
  logLine += '\n';

  // And append it to the log file

  logFile_.prepend("(sent) ");
  logFile_.append(logLine);
}

/**.......................................................................
 * Write data to stdout
 */
void TerminalServer::writeToStdout(std::string& line)
{
  LogStream errStr;

  // Write to stdout

  cout << line;
}

/**.......................................................................
 * Read data from a client socket
 */
void TerminalServer::readFromPort()
{
  LogStream errStr;

  // Read any bytes waiting at the port.  If a newline was
  // encountered, it will have been added to the string returned by
  // Port::readString()

  std::string line = port_->readString();

  DBPRINT(true, Debug::DEBUG3, "Read line: " << line << " from the port");

  // And forward the result to any connected clients

  for(std::list<NetTermHandler*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++) 
  {
    NetTermHandler* client = *iclient;
    
    client->sendLine(line);
  }

  // Write the line read from the client to the port

  writeToStdout(line);

  // Finally, echo the line to the log file. Append a newline to
  // whatever the port put there

  logFile_.prepend("(rcvd) ");
  logFile_.append(line);

  // If we were waiting for a response, mark it as received

  String str(line);

  if(pending_ && str.contains('\n'))
    pending_ = false;

  // If we are trying to get in, don't do anything -- just send the
  // next command to the datalogger

  if(hammering_) {
    cout << "got a response" << endl;
    waitingToHammer_ = false;
    return stepHammer();
  }
}

/**.......................................................................
 * Write data to the port
 */
void TerminalServer::writeToPort(std::string& line)
{
  LogStream errStr;

  if(port_ == 0)
    return;

  // Write to the port

  port_->writeString(line);

  // Finally, echo the line to the log file

  logFile_.append(line);
}

/**.......................................................................
 * Write data to the port
 */
void TerminalServer::writeToPort(Vector<unsigned char>& bytes)
{
  LogStream errStr;
  
  if(port_ == 0)
    return;

  // Write to the port

  port_->writeBytes(bytes);
}

/**.......................................................................
 * A shutdown method
 */
SIGNALTASK_HANDLER_FN(TerminalServer::shutDown)
{
  TerminalServer* server = (TerminalServer*) args;

  DBPRINT(true, Debug::DEBUG3, "Inside shutDown");

  server->stop_ = true;
}

/**.......................................................................
 * Initialize the port
 */
void TerminalServer::initPort()
{
  Port stdoutPort(1);

  stdoutPort.setNoBuf();
  
  port_->dontStrip("\n");
  fdSet_.registerReadFd(port_->getFd());
}

/**.......................................................................
 * Open a file to download to the port
 */
void TerminalServer::openFile(std::string fileName)
{
  LogStream errStr;

  if((file_=fopen(fileName.c_str(), "r"))==0) {
    errStr.initMessage(true);
    errStr << "Error opening: " << fileName;
    errStr.appendSysError(true, "open()");
    errStr.report();
    return;
  } 

  // Set the timeout in select to be zero
  
  timeOut_ = timeVal_.timeVal();
}

/**.......................................................................
 * Read from a file
 */
void TerminalServer::readFromFile()
{
  // If we are still waiting for a response to the last line sent,
  // don't do anything

  if(pending_)
    return;

  char buff[101];

  if(fgets(buff, 100, file_) == NULL) {
    if(file_ != NULL) {
      fclose(file_);
      file_ = 0;
      timeOut_ = 0;
    }
    return;
  }

  std::string line(buff);
  
  DBPRINT(true, Debug::DEBUG3, "Sending line: " << line << " to the port");
  
  // Send it to the port unmodified
  
  port_->writeString(line);
  
  // Make sure the string contains exactly one newline
  
  std::string logLine = line;
  sza::util::strip(logLine, '\n');
  logLine += '\n';
  
  // And append it to the log file
  
  logFile_.prepend("(sent) ");
  logFile_.append(logLine);
  
  // Set the pending flag.  We will only send the next line once the
  // port has responded
  
  pending_ = true;
}

bool TerminalServer::parseCommand(std::string line)
{
  unsigned istr;
  std::string::size_type idx;

  if((idx=line.find("TS:DL")) != std::string::npos) {
    for(istr=6; istr != line.size()-1; istr++) {
      if(isspace(line[istr])) {
	istr--;
	break;
      }
    }
    std::string fileName = line.substr(6, istr-6+1);
    openFile(fileName);
    return true;
  } else if((idx=line.find("TS:HAMMER")) != std::string::npos) {
    initHammer();
    return true;
  }

  // Not a special command

  return false;
}

/**.......................................................................
 * Begin a sequence to crack open the datalogger's head
 */
void TerminalServer::initHammer()
{
  LogStream errStr;

  hammering_ = true;
  waitingToHammer_ = true;

  hammerStep_ = hammers_.begin();

  // Set the timeout in select to be zero
  
  timeVal_.setMicroSeconds(100000);
  timeOut_ = timeVal_.timeVal();
}

/**.......................................................................
 * Called when the datalogger sends a response
 */
void TerminalServer::stepHammer()
{
  LogStream errStr;

  cout << "stepping" << endl;

  if(hammerStep_ != hammers_.end()) {
    writeToPort(*hammerStep_);
    hammerStep_++;
  } else {

    // Set the timeout in select to be zero
  
    timeOut_ = 0;
    hammering_ = false;
  }
}

RUN_FN(TerminalServer::runFn)
{
  TerminalServer* runnable = (TerminalServer*) arg;

  std::cout << "About to call run" << std::endl;

  runnable->run();
}
