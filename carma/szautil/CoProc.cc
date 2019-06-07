#include "carma/szautil/CoProc.h"
#include "carma/szautil/Exception.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <signal.h>
#include <stdio.h>

#include <iostream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CoProc::CoProc(std::string exe)
{
  std::vector<std::string> args = split(exe);

  std::vector<char*> argv;
  argv.resize(args.size()+1);

  unsigned i;
  for(i=0; i < args.size(); i++) {
    argv[i] = &args[i][0];
  }
  argv[i] = 0;

  fork(argv[0], &argv[0], 0, 0, 0);
}

/**.......................................................................
 * Constructor.
 */
CoProc::CoProc(char *exe)
{
  std::vector<std::string> args = split(exe);

  std::vector<char*> argv;
  argv.resize(args.size()+1);

  unsigned i;
  for(i=0; i < args.size(); i++) {
    argv[i] = &args[i][0];
  }
  argv[i] = 0;

  fork(argv[0], &argv[0], 0, 0, 0);
}

/**.......................................................................
 * Constructor.
 */
CoProc::CoProc(char *exe, FILE** stdInFp, FILE** stdOutFp, FILE** stdErrFp) 
{
  std::vector<std::string> args = split(exe);

  std::vector<char*> argv;
  argv.resize(args.size()+1);

  unsigned i;
  for(i=0; i < args.size(); i++) {
    argv[i] = &args[i][0];
  }
  argv[i] = 0;

  fork(argv[0], &argv[0], stdInFp, stdOutFp, stdErrFp);
}

/**.......................................................................
 * Constructor.
 */
CoProc::CoProc(char *exe, char** argv,
	       FILE** stdInFp, FILE** stdOutFp, FILE** stdErrFp) 
{
  fork(exe, argv, stdInFp, stdOutFp, stdErrFp);
}

void CoProc::fork(char *exe, char** argv, 
		  FILE** stdInFp, FILE** stdOutFp, FILE** stdErrFp) 
{
  pid_      = 0;

  stdIn_  = new Pipe();
  stdOut_ = new Pipe();
  stdErr_ = new Pipe();

  if(stdInFp)
    *stdInFp  = stdIn_->writeFp();

  if(stdOutFp)
    *stdOutFp = stdOut_->readFp();

  if(stdErrFp)
    *stdErrFp = stdErr_->readFp();

  // Create a child process.  But don't have the parent wait for it!

#if MAC_OSX == 0
  signal(SIGCLD, SIG_IGN); // Ignore child status signals
#else
  signal(SIGCHLD, SIG_IGN); // Ignore child status signals
#endif

  pid_ = ::fork();

  if(pid_ < 0) {
    ThrowSysError("Unable to fork");
  } else if(pid_ > 0) {     /* Parent */

    //    COUT("Parent closing...");
    stdIn_->closeForRead();
    stdOut_->closeForWrite();
    stdErr_->closeForWrite();

  } else {                 /* Child */

    //    COUT("Child closing...");
    stdIn_->closeForWrite();
    stdOut_->closeForRead();
    stdErr_->closeForRead();
    
    // Make the child ends of the pipes refer to stdin and stdout.

    if(dup2(stdIn_->readFd(),   STDIN_FILENO)  < 0 || 
       dup2(stdOut_->writeFd(), STDOUT_FILENO) < 0 ||
       dup2(stdErr_->writeFd(), STDERR_FILENO) < 0) {

      ThrowSysError("dup2()");
    };
    
    // Overlay the child process with the desired executable.
    
    execvp(exe, argv);
    ThrowSysError("execvp()");
  }
}

/**.......................................................................
 * Destructor.
 */
CoProc::~CoProc() 
{
  if(stdIn_) {
    delete stdIn_;
    stdIn_ = 0;
  }
  
  if(stdOut_) {
    delete stdOut_;
    stdOut_ = 0;
  }
  
  if(stdErr_) {
    delete stdErr_;
    stdErr_ = 0;
  }

  // Send the kill signal to the child process

  kill(pid_, SIGKILL);
}


CoProc::Pipe* CoProc::stdIn()
{
  return stdIn_;
}

CoProc::Pipe* CoProc::stdOut()
{
  return stdOut_;
}

CoProc::Pipe* CoProc::stdErr()
{
  return stdErr_;
}


/**.......................................................................
 * Pipe constructor
 */
CoProc::Pipe::Pipe() {
  if(::pipe(fd_) < 0)
    ThrowSysError("pipe()");
}

FILE* CoProc::Pipe::readFp() 
{
  FILE* fp = fdopen(fd_[0], "r");

  if(fp==0) {
    close();
    ThrowSysError("fdopen()");
  }

  return fp;
}

FILE* CoProc::Pipe::writeFp() 
{
  FILE* fp = fdopen(fd_[1], "w");

  if(fp==0) {
    close();
    ThrowSysError("fdopen()");
  }

  return fp;
}

int CoProc::Pipe::readFd()
{
  return fd_[0];
}

int CoProc::Pipe::writeFd()
{
  return fd_[1];
}

void CoProc::Pipe::close()
{
  closeForRead();
  closeForWrite();
}

void CoProc::Pipe::closeForWrite()
{
  //  COUT("CoProc closing for write: " << fd_[1]);
  if(fd_[1] >= 0) {
    ::close(fd_[1]);
    fd_[1] = -1;
  }
  ::close(fd_[1]);
}

void CoProc::Pipe::closeForRead()
{
  //  COUT("CoProc closing for read: " << fd_[0]);
  if(fd_[0] >= 0) {
    ::close(fd_[0]);
    fd_[0] = -1;
  }
}

CoProc::Pipe::~Pipe() 
{
  close();
}

/**.......................................................................
 * Split a command string into separate tokens
 */
std::vector<std::string> CoProc::split(std::string command)
{
  enum TokState {
    START,       // We are at the head of the string
    TOK_DQUOTE,  // We are reading a double-quoted token
    TOK_SQUOTE,  // We are reading a double-quoted token
    TOK_WSPACE   // We are reading a white-space separated token
  };
  
  TokState state=START;

  std::vector<std::string> args;
  std::ostringstream os;

  char lastToken, c;

  for(unsigned i=0; i < command.size(); i++) {

    c = command[i];

    switch (state) {

      // If we are at the head of the string, disregard tokens until a
      // non-whitespace char is found

    case START:

      if(!isspace(c)) {
	os.str("");
	os << c;
	if(c=='\'')
	  state = TOK_SQUOTE;
	else if(c=='\"')
	  state = TOK_DQUOTE;
	else
	  state = TOK_WSPACE;
      }
      break;

    case TOK_WSPACE:

      // If we are reading a token, keep adding chars to the stream
      // until a space is found

      if(!isspace(c)) {
	os << c;
      } else {
	args.push_back(os.str());
	state = START;
      }

      break;

    case TOK_DQUOTE:

      // If we are reading a quoted token, keep adding chars to the
      // stream until the matching quote is found

      if(c != '\"') {
	os << c;
      } else {
	os << c;
	args.push_back(os.str());
	state = START;
      }

      break;

    case TOK_SQUOTE:

      // If we are reading a quoted token, keep adding chars to the
      // stream until the matching quote is found

      if(c != '\'') {
	os << c;
      } else {
	os << c;
	args.push_back(os.str());
	state = START;
      }

      break;

    default:
      break;
    }

    lastToken = c;
  }

  // If we reached the end of the string while reading a white-space
  // token, we are at the end of the token

  if(state==TOK_WSPACE)
    args.push_back(os.str());

  return args;
}

int CoProc::readFd()
{
  return stdOut_->readFd();
}

int CoProc::writeFd()
{
  return stdIn_->writeFd();
}
