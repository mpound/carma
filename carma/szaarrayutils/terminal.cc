#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <math.h>

#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>

#include "tcpip.h"

#include "lprintf.h"
#include "szacontrol.h"

#include "carma/szautil/FdSet.h"
#include "carma/szautil/TimeOut.h"

/*
 * Useful enumerators for opening & closing socket connections 
 */
enum {
  CLOSE=0,
  OPEN=1
};
/*
 * An object of the following type is used to hold the state of
 * the terminal I/O thread.
 */
struct Term {

  ControlProg *cp;  /* The state-object of the control program */

  sza::util::Pipe *pipe;       /* An externally allocated control-input pipe */

  // A list of emails to be notified when the pager is activated

  std::vector<std::string>* emails_;

  // True if the pager is enabled

  bool pagingEnabled_;

  // True if the pager should be auto-reenabled

  bool pagerAutoEnable_;

  // The interval, in seconds, after which the pager should be
  // re-enabled after it is activated

  double pagerAutoEnableIntervalInSeconds_;

  // True if the audio alarm is enabled

  bool audioAlarmEnabled_;

  // The time that the pager was last disabled

  sza::util::TimeVal* timePagerWasDisabled_;

  // An object for handling periodic timeouts associated with the
  // pager

  sza::util::TimeOut* timeOut_;

  void registerTimeout();
  void sendPeriodicPage();
  void autoReenablePager();
};

// Local commands for manipulating pager device resources

static int activate_reg_pager(Term *term, char* reg=NULL, char* comment=NULL, bool on=true);
static int activate_msg_pager(Term *term, char* msg=NULL, bool on=true);
static int pagerAddEmailAddress(Term *term, bool add, char *email);
static int enable_pager(Term *term, bool enable, unsigned mask=sza::array::PAGE_ENABLE, char* msg=0,
			bool activateAudioAlarm=true);

/*.......................................................................
 * Create the state object of a terminal I/O thread.
 *
 * Input:
 *
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe on which to listen for control messages.
 *
 * Output:
 *
 *  return        void *   The new Term object, or NULL on error.
 */
CP_NEW_FN(new_Term)
{
  Term *term=NULL; /* The object to be returned */
  
  // Allocate the container.

  if((term=(Term *)malloc(sizeof(Term)))==NULL) {
    lprintf(stderr, "new_Term: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can be safely
  // passed to del_Term().

  term->cp   = cp;
  term->pipe = pipe;

  term->pagingEnabled_     = true;
  term->audioAlarmEnabled_ = true;

  term->emails_ = 0;
  term->emails_ = new std::vector<std::string>();
  if(term->emails_ == 0)
    ThrowError("Unable to allocate emails array");

  // Members for automatic re-enabling of the pager

  term->timePagerWasDisabled_ = 0;
  term->timePagerWasDisabled_ = new sza::util::TimeVal();
  if(term->timePagerWasDisabled_ == 0)
    ThrowError("Unable to allocate pager disabled timer");

  term->pagerAutoEnableIntervalInSeconds_ = 30;

  // A timeout, to sound the audio alarm when the pager has been
  // activated, also used to automatically reactivate the pager

  term->timeOut_ = 0;
  term->timeOut_ = new sza::util::TimeOut();
  if(term->timeOut_ == 0)
    ThrowError("Unable to allocate timeout object");

  term->timeOut_->setIntervalInSeconds(10);

  return term;
}
/*.......................................................................
 * Delete the state-object of a term thread.
 *
 * Input:
 *  obj         void *  The Term object to be deleted.
 *
 * Output:
 *  return      void *  The deleted Term object (always NULL).
 */
CP_DEL_FN(del_Term)
{
  Term *term = (Term* )obj;

  if(term) {

    if(term->emails_) {
      delete term->emails_;
      term->emails_ = 0;
    }

    if(term->timeOut_) {
      delete term->timeOut_;
      term->timeOut_ = 0;
    }

    if(term->timePagerWasDisabled_) {
      delete term->timePagerWasDisabled_;
      term->timePagerWasDisabled_ = 0;
    }

    // Finally, free the container itself

    free(term);
  }
  return NULL;
}
/*.......................................................................
 * Return the term resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *
 * Output:
 *  return      Term *   The term resource object.
 */
static Term *cp_Term(ControlProg *cp)
{
  return (Term* )cp_ThreadData(cp, CP_TERM);
}

/*.......................................................................
 * Attempt to send a message to a term thread.
 *
 * Input:
 *
 *  cp       ControlProg *  The state-object of the control program.
 *  msg      TermMessage *  The message to be sent. This must have been
 *                          filled by one of the pack_term_<type>()
 *                          functions.
 *  timeout  long           The max number of milliseconds to wait for the
 *                          message to be sent, or -1 to wait indefinitely.
 *
 * Output:
 *
 *  return   PipeState      The status of the transaction:
 *
 *                            PIPE_OK    - The message was read successfully.
 *                            PIPE_BUSY  - The send couldn't be accomplished
 *                                         without blocking (only returned
 *                                         when timeout=PIPE_NOWAIT).
 *                            PIPE_ERROR - An error occurred.
 */
PipeState send_TermMessage(ControlProg *cp, TermMessage *msg,
			   long timeout)
{
  return cp_Term(cp)->pipe->write(msg, sizeof(*msg), timeout);
}

/*.......................................................................
 * Send a shutdown message to the term thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 *
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Term)
{
  TermMessage msg;   /* The message to be sent */
  return pack_term_shutdown(&msg) ||
    send_TermMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *  msg     TermMessage *  The message object to be prepared.
 *
 * Output:
 * return   int            0 - OK.
 *                         1 - Error.
 */
int pack_term_shutdown(TermMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_term_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = TERM_SHUTDOWN;
  return 0;
}

/**.......................................................................
 * Return a list of pager email addresses.
 */
std::vector<std::string>* getPagerEmailList(ControlProg *cp)
{
  return cp_Term(cp)->emails_;
}

/*.......................................................................
 * Prepare a port message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *
 *    msg    TermMessage *  The message object to be prepared.
 *    dev    PagerDev       The device whose IP address we are changing
 *    ip     char *         The new IP address
 *
 * Output:
 *
 *   return  int            0 - OK.
 *                          1 - Error.
 */
int pack_pager_ip(TermMessage *msg, PagerDev dev, char *ip)
{
  size_t len;
  
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_term_port: NULL argument.\n");
    return 1;
  };
  msg->type = TERM_IP;

  if(ip) {
    len = strlen(ip);

    if(len > CP_FILENAME_MAX) {
      lprintf(stderr,"IP address too long\n");
      return 1;
    }
    strncpy(msg->body.ip, ip, len);
    msg->body.ip[len] = '\0';
  } else {
    msg->body.ip[0] = '\0';
  }
  msg->dev = dev;

  return 0;
}

/**.......................................................................
 * Prepare a pager enable message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *
 *    msg    TermMessage *  The message object to be prepared.
 *    dev    PagerDev       The device whose IP address we are changing
 *    ip     char *         The new IP address
 *
 * Output:
 *
 *   return  int            0 - OK.
 *                          1 - Error.
 */
int pack_pager_enable(TermMessage *msg, bool enable)
{
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_pager_enable: NULL argument.\n");
    return 1;
  };

  msg->type = TERM_ENABLE;
  msg->body.enable.enable = enable;

  return 0;
}

/**.......................................................................
 * Prepare a pager enable message for subsequent transmission to the
 * term thread.
 */
int packPagerAutoEnable(TermMessage *msg, unsigned seconds)
{
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_pager_enable: NULL argument.\n");
    return 1;
  };

  msg->type = TERM_AUTO_INTERVAL;
  msg->body.autoInterval.seconds = seconds;

  return 0;
}

/**.......................................................................
 * Prepare a pager enable message for subsequent transmission to the
 * term thread.
 */
int packPagerAutoEnable(TermMessage *msg, bool enable)
{
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_pager_enable: NULL argument.\n");
    return 1;
  };

  msg->type = TERM_AUTO_ENABLE;
  msg->body.autoEnable.enable = enable;

  return 0;
}

/*.......................................................................
 * Prepare an email message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *
 *    msg    TermMessage *  The message object to be prepared.
 *    dev    PagerDev       The device whose IP address we are changing
 *    ip     char *         The new IP address
 *
 * Output:
 *
 *   return  int            0 - OK.
 *                          1 - Error.
 */
int pack_pager_email(TermMessage *msg, bool add, char *email)
{
  size_t len;
  
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_pager_email: NULL argument.\n");
    return 1;
  };

  msg->type = TERM_EMAIL;
  msg->body.email.add = add;

  if(email) {
    len = strlen(email);

    if(len > CP_FILENAME_MAX) {
      lprintf(stderr,"Email address too long\n");
      return 1;
    }
    strncpy(msg->body.email.address, email, len);
    msg->body.email.address[len] = '\0';
  } else {

    // Don't allow addition of null addresses

    if(add) 
      ThrowError("No Email address specified");

    msg->body.email.address[0] = '\0';
  }
    
  return 0;
}


/**.......................................................................
 * Prepare a pager message for subsequent transmission to the
 * term thread.
 */
int pack_term_reg_page(TermMessage *msg, char* reg, char* comment, bool on)
{
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_term_page: NULL argument.\n");
    return 1;
  };

  if(reg) {
    unsigned len = strlen(reg);

    if(len > CP_FILENAME_MAX) {
      lprintf(stderr,"Register name too long\n");
      return 1;
    }
    strncpy(msg->body.page.msg, reg, len);
    msg->body.page.msg[len] = '\0';
  } else {
    msg->body.page.msg[0] = '\0';
  }

  if(comment) {
    unsigned len = strlen(comment);

    if(len > CP_FILENAME_MAX) {
      lprintf(stderr,"Comment name too long\n");
      return 1;
    }
    strncpy(msg->body.page.comment, comment, len);
    msg->body.page.comment[len] = '\0';
  } else {
    msg->body.page.comment[0] = '\0';
  }

  msg->type         = TERM_REG_PAGE;
  msg->body.page.on = on;

  return 0;
}

/**.......................................................................
 * A handler to be called when a register goes out of range
 */
MONITOR_CONDITION_HANDLER(sendRegPage)
{
  ControlProg* cp = (ControlProg*) arg;
  send_reg_page_msg(cp, (char*)message.c_str(), (char*)comment.c_str());
}

/*.......................................................................
 * Prepare a pager message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *
 *    msg      TermMessage *  The message object to be prepared.
 *    activate int            1 -- turn the pager on
 *                            0 -- turn the pager off
 * Output:
 *
 *   return     int           0 - OK.
 *                            1 - Error.
 */
int send_reg_page_msg(ControlProg* cp, char* reg, char* comment)
{
  unsigned waserr = 0;

  // Only send the page if paging is enabled

  if(cp_Term(cp)->pagingEnabled_) {
    TermMessage msg;

    waserr |= pack_term_reg_page(&msg, reg, comment);
    waserr |= send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR;
  }

  return waserr;
}


/*.......................................................................
 * Prepare a pager message for subsequent transmission to the
 * term thread.
 *
 * Input:
 *
 *    msg      TermMessage *  The message object to be prepared.
 *    activate int            1 -- turn the pager on
 *                            0 -- turn the pager off
 * Output:
 *
 *   return     int           0 - OK.
 *                            1 - Error.
 */
int pack_term_msg_page(TermMessage *msg, std::string txt, bool on)
{
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_term_msg_page: NULL argument.\n");
    return 1;
  };

  unsigned len = txt.size();

  if(len > CP_FILENAME_MAX) {
    lprintf(stderr,"Register name too long\n");
    return 1;
  }

  strncpy(msg->body.page.msg, (char*)txt.c_str(), len);
  msg->body.page.msg[len] = '\0';

  msg->type         = TERM_MSG_PAGE;
  msg->body.page.on = on;

  return 0;
}

/*.......................................................................
 * This is the entry-point of the term thread.
 *
 * Input:
 *  arg          void *  A pointer to the Term state object pointer,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(term_thread)
{
  Term *term = (Term* )arg; /* The state-object of the current thread */
  TermMessage msg; /* An message reception container */
  /*
   * Enable logging of the scheduler's stdout and stderr streams.
   */
  if(log_thread_stream(term->cp, stdout) ||
     log_thread_stream(term->cp, stderr)) {
    cp_report_exit(term->cp);
    return NULL;
  };
  /*
   * Wait for commands from other threads.
   */
  sza::util::FdSet fdSet;
  fdSet.registerReadFd(term->pipe->readFd());

  int waserr = 0;
  int nready;
  while(!waserr) {
      
    nready = select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, 
		    term->timeOut_->tVal());

    switch (nready) {
    case -1:

      perror("term_event_loop: select error");
      waserr = 1;
      break;

    case 0:
      
      term->registerTimeout();
      break;

    default:

      if(term->pipe->read(&msg, sizeof(msg), PIPE_WAIT) != PIPE_OK)
	break;

      // Interpret the message.
	
      switch(msg.type) {
      case TERM_SHUTDOWN: /* A shutdown message to the term thread */
	cp_report_exit(term->cp); /* Report the exit to the control thread */
	return NULL;
	break;
      case TERM_EMAIL:
	pagerAddEmailAddress(term, msg.body.email.add, msg.body.email.address);
	break;
      case TERM_ENABLE:
	enable_pager(term, msg.body.enable.enable, sza::array::PAGE_ENABLE, 
		     0, false);
	break;
      case TERM_REG_PAGE:
	activate_reg_pager(term, msg.body.page.msg, msg.body.page.comment, 
			   msg.body.page.on);
	break;
      case TERM_MSG_PAGE:
	activate_msg_pager(term, msg.body.page.msg, msg.body.page.on);
	break;
      case TERM_AUTO_INTERVAL:
	term->pagerAutoEnableIntervalInSeconds_ = msg.body.autoInterval.seconds;
	break;
      case TERM_AUTO_ENABLE:

	term->pagerAutoEnable_ = msg.body.autoEnable.enable;

	// If paging was disabled when this command arrived, activate
	// the timeout pager now

	if(!term->pagingEnabled_) {
	  term->timeOut_->activate(true);
	}

	break;
      default:
	lprintf(stderr, "term_thread: Unknown command-type received.\n");
	break;
      };
      break;
    };
  };

  lprintf(stderr, "Term thread exiting after pipe read error.\n");
  cp_report_exit(term->cp);

  return NULL;
}

/*.......................................................................
 * De/Activate the pager.
 *
 * term      *  Term  The parent container
 * activate     int   True (1) to turn the pager on
 *                    False (0) to turn it off
 *
 * 
 *
 */
static int activate_reg_pager(Term *term, char* reg, char* comment, bool on)
{
  int waserr=0;
  std::ostringstream os;

  if(!on) {
    term->audioAlarmEnabled_ = false;
    return 0;
  }

  //------------------------------------------------------------
  // Just exit immediately if paging is not allowed
  //------------------------------------------------------------

  if(!term->pagingEnabled_)
    return 0;

  //------------------------------------------------------------
  // Disallow further pages until the pager is re-enabled
  //------------------------------------------------------------

  waserr |= enable_pager(term, false, sza::array::PAGE_ENABLE|sza::array::PAGE_REG, reg);

  for(unsigned i=0; i < term->emails_->size(); i++) {

    os.str("");

    os << "echo \"Pager was activated ";

    if(reg==0 || reg[0]=='\0')
      os << "(no register was specified)\" ";
    else
      os << "(by register " << reg << ")";

    // Add the comment if it was specified

    os << std::endl << std::endl << comment << "\" ";

    os << "| Mail -s \"SZA Alert\" "
       << term->emails_->at(i);

    system(os.str().c_str());
  }

  //------------------------------------------------------------
  // Print to the log file as well
  //------------------------------------------------------------

  os.str("");

  os << "echo \"Pager was activated ";

  if(reg==0 || reg[0]=='\0')
    os << "(no register was specified)\" ";
  else
    os << "(by register " << reg << ")\" ";
  
  lprintf(stderr, "%s\n", (char*)os.str().c_str());
  
  //------------------------------------------------------------
  // Lastly, regardless of whether audio paging is enabled or not
  // (since now we will use the timer to reactivate the pager as well),
  // set a timer to send periodic pages
  //------------------------------------------------------------

  term->timeOut_->activate(true);

  return waserr;
}

/**.......................................................................
 * Enable/disable the pager
 *
 * term     Term* The parent container
 * enable   bool  True to enable the pager
 *                False to disable it
 *
 * 
 *
 */
static int enable_pager(Term *term, bool enable, unsigned mask, char* msg, bool activateAudioAlarm)
{
  // Set the new paging state

  term->pagingEnabled_ = enable;

  // Tell remote monitor clients about our new status
  
  if(sendPagingState(term->cp, enable, mask, msg))
    return 1;

  // If we are re-enabling the pager, deactivate the timeout, and
  // re-enable the audio alarm.
  //
  // If we are de-activating the pager, set a timeout for
  // auto-reenable of the pager, and record the time the pager was
  // deactivated

  if(enable) {
    term->timeOut_->activate(false);
    term->audioAlarmEnabled_ = true;
  } else {

    term->timeOut_->activate(true);
    term->timePagerWasDisabled_->setToCurrentTime();

    // If we are manually disabling the pager, don't activate the audio alarm

    if(!activateAudioAlarm)
      term->audioAlarmEnabled_ = false;
  }
  
  return 0;
}

/**.......................................................................
 * De/Activate the pager.
 *
 * term      *  Term  The parent container
 * activate     int   True (1) to turn the pager on
 *                    False (0) to turn it off
 *
 * 
 *
 */
static int activate_msg_pager(Term *term, char* msg, bool on)
{
  int waserr=0;
  std::ostringstream os;

  //------------------------------------------------------------
  // If the 'off' command was issued, deactivate the audio alarm
  //------------------------------------------------------------

  if(!on) {
    term->timeOut_->activate(false);
    return 0;
  }

  //------------------------------------------------------------
  // Just exit immediately if paging is not allowed
  //------------------------------------------------------------

  if(!term->pagingEnabled_)
    return 0;

  //------------------------------------------------------------
  // Else disallow further pages until the pager is re-enabled, and
  // send the current page
  //------------------------------------------------------------

  waserr |= enable_pager(term, false, sza::array::PAGE_ENABLE|sza::array::PAGE_MSG, msg);

  for(unsigned i=0; i < term->emails_->size(); i++) {

    os.str("");

    os << "echo \"" << msg << "\" ";

    os << "| Mail -s \"SZA Alert\" "
       << term->emails_->at(i);

    system(os.str().c_str());
  }

  //------------------------------------------------------------
  // Print to the log file as well
  //------------------------------------------------------------

  os.str("");
  os << "Pager was activated: " << msg;
  lprintf(stderr, "%s\n", (char*)os.str().c_str());
  
  //------------------------------------------------------------
  // Lastly, if audio paging is enabled, set a timer to send periodic
  // pages
  //------------------------------------------------------------

  term->timeOut_->activate(true);

  return waserr;
}

/*.......................................................................
 * Change the device we use to communicate with the pager
 *
 * Input:
 *  dev      char *   The device to use when creating subsequent
 *                    term files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int pagerAddEmailAddress(Term *term, bool add, char* email)
{
  if(add)
    term->emails_->push_back(email);
  else {

    if(email==NULL || email[0]=='\0') {
      term->emails_->clear();
      return 0;
    }

    std::vector<std::string>::iterator iString;

    for(iString=term->emails_->begin(); iString != term->emails_->end();
	iString++)
      if(*iString == email)
	break;
    if(iString != term->emails_->end())
      term->emails_->erase(iString);
  }

  return 0;
}

/**.......................................................................
 * Function called when a pager timeout has occurred
 */
void Term::registerTimeout()
{
  // Activate the periodic audio alarm, if it is enabled

  if(audioAlarmEnabled_) {
    sendPeriodicPage();
  }

  // Check if the pager should be re-enabled

  if(pagerAutoEnable_ && !pagingEnabled_) {
    sza::util::TimeVal curr, diff;
    curr.setToCurrentTime();

    diff = curr - *timePagerWasDisabled_;

    if(diff.getTimeInSeconds() > pagerAutoEnableIntervalInSeconds_) {
      autoReenablePager();
    }
  }
 
  // And reset for the next timeout.  Note that we don't deactivate
  // the timeout anymore until the pager is re-enabled, since it is
  // used both to fire the audio alarm and to re-activate the pager.
  // If the pager was re-enabled on this call, the timeout will
  // already have been deactivated in enable_pager() at this point.

  timeOut_->reset();
}

void Term::sendPeriodicPage()
{
  std::ostringstream os;
  os << "/home/szadaq/sza/audio/pager.csh";

  int status = 0;
  status = system(os.str().c_str());

  if(status < 0) {
    ReportSimpleError("System call: \'" << os.str() << "\' exited with status: " << status);
  }
}

void Term::autoReenablePager()
{
  std::ostringstream os;

  for(unsigned i=0; i < emails_->size(); i++) {

    os.str("");

    os << "echo \"" << "The pager is being re-enabled because more than " 
       << pagerAutoEnableIntervalInSeconds_ << " seconds have elapsed " << std::endl
       << "since the pager was disabled, and no-one has re-enabled the pager"
       << "\" ";

    os << "| Mail -s \"SZA Pager Re-enabled\" "
       << emails_->at(i);

    system(os.str().c_str());
  }

  enable_pager(this, true); 
}
