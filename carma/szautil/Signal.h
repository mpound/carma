#ifndef SIGNAL_H
#define SIGNAL_H

/**
 * @file Signal.h
 * 
 * Tagged: Fri Nov 14 12:39:35 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <signal.h>     // Needed for SIGPIPE, etc.
#include <setjmp.h>     // Needed for jmp_buf, etc.

#define SIG_HANDLER(fn)  void  (fn)(int sigNo)
#define SIG_ACTION(fn)   void  (fn)(int sigNo, siginfo_t* sigInfo, void* arg)

#if !HAVE_RT
#define SIGRTMAX 31
#endif

namespace sza {
  namespace util {
    
    /** 
     * Class to encapsulate signal handling.
     */
    class Signal {
    public:
      
      /**
       * Constructor with no sigset or handler.
       */
      Signal(int sigNo);
      
      /**
       * Handler constructor with no sigset.
       */
      Signal(int sigNo, SIG_HANDLER(*handler));
      
      /**
       * Handler constructor with sigset.
       */
      Signal(int sigNo, SIG_HANDLER(*handler), sigset_t* sigSet);
      
      /**
       * Action constructor with no sigset.
       */
      Signal(int sigNo, SIG_ACTION(*action));
      
      /**
       * Action constructor with sigset.
       */
      Signal(int sigNo, SIG_ACTION(*action), sigset_t* sigSet);
      
      /**
       * Install a handler, in case we want to declare a global
       * Signal type before the handler is declared.
       */
      void installHandler(SIG_HANDLER(*handler));
      
      /**
       * Overloaded method to install an action as the handler, in
       * case we want to declare a global Signal type before the
       * handler is declared.
       */
      void installHandler(SIG_ACTION(*action));
      
      /**
       * Destructor
       */
      ~Signal();
      
    private:
      
      /**
       * A static pointer to ourselves for use in our signal handler.
       */
      static Signal* signal_;
      
      /**
       * The signal we are handling.
       */
      int sigNo_;
      
      /**
       * The signal mask to block.
       */
      sigset_t sigSet_;
      
      /**
       * A flag to record if the above set is empty.
       */
      bool sigSetIsEmpty;
      
      /**
       * The function which wil be called when the signal handled
       * by this object is delivered.
       */
      static SIG_ACTION(action_);
      
      /**
       * Pointer to the user-supplied signal handler/action for
       * this object.  This should be a static function defined by
       * callers of this class, and passed as an argument to the
       * Signal constructor.  This function will be called by our
       * action_ function above, on receipt of the signal.
       *
       * Note: it is not important to catch exceptions inside the
       * user-supplied handler, since any exceptions that may be
       * generated will be caught by action_, above, which is the
       * handler that actually gets called when the signal is
       * delivered.
       */
      SIG_HANDLER(*userhandler_);
      
      SIG_ACTION(*useraction_);
      
      /**
       * Main constructor.
       *
       * @param sigNo The signal number associated with this
       * object.  
       *
       * @param handler The function to be called on receipt of
       * this signal (handler).
       *
       * @param action The function to be called on receipt of
       * this signal (action).
       *
       * @param sigSet The set of signals to block while our signal
       * handler is executing, in addition to sigNo, which is
       * automatically blocked.  A NULL argument will be
       * interpreted as an empty set (same as passing a set on
       * which sigemptyset() has been called).
       *
       * @throws Exception
       */
      void privateConstructor(int sigNo, SIG_HANDLER(*handler), 
			      SIG_ACTION(*action), sigset_t* sigSet);
      /**
       * Call sigaction with the user-specified handler and signal
       * mask.
       */
      void setUpSigAction();
      
    }; // End class Signal
    
  }; // End namespace util
  }; // End namespace sza

#endif // End #ifndef 
