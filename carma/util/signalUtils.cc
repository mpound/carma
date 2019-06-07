#include "carma/util/signalUtils.h"

#include <sstream>
#include <cerrno>

#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


const char *
carma::util::getTextForSignalNumber( const int signalNumber )
{
    // SIGIOT has the same value as SIGABRT on our CentOS 4.1 systems
    compileTimeCheck< SIGABRT == SIGIOT >();
    
    switch ( signalNumber ) {
        case SIGABRT:
            return "SIGABRT/SIGIOT - Abort signal from abort(3) or IOT Trap (BSD)";

        case SIGHUP:     return "SIGHUP - hangup signal";
        case SIGINT:     return "SIGINT - Keyboard interrupt (^C)";
        case SIGQUIT:    return "SIGQUIT - keybord quit";
        case SIGILL:     return "SIGILL - Illegal instruction";
        case SIGTRAP:    return "SIGTRAP - Trace/breakpoint trap";
        case SIGBUS:     return "SIGBUS - BUS Error";
        case SIGFPE:     return "SIGFPE - Floating point exception";
        case SIGKILL:    return "SIGKILL - Kill, unblockable";
        case SIGUSR1:    return "SIGUSR1 - User-defined signal # 1";
        case SIGSEGV:    return "SIGSEGV - Invalid memory reference";
        case SIGUSR2:    return "SIGUSR2 - User-defined signal # 2";
        case SIGPIPE:    return "SIGPIPE - Broken pipe";
        case SIGALRM:    return "SIGALRM - Timer signal";
        case SIGTERM:    return "SIGTERM - Termination signal";
        case SIGSTKFLT:  return "SIGSTKFLT - Stack fault";
        case SIGCHLD:    return "SIGCHLD - Child stopped or terminated";
        case SIGCONT:    return "SIGCONT - Continue if stopped";
        case SIGSTOP:    return "SIGSTOP - Stop process";
        case SIGTSTP:    return "SIGTSTP - Stop typed at tty (^Z)";
        case SIGTTIN:    return "SIGTTIN - tty input for background process";
        case SIGTTOU:    return "SIGTTOU - tty output for background process";
        case SIGURG:     return "SIGURG - Urgent condition on socket";
        case SIGXCPU:    return "SIGXCPU - CPU limit exceeded";
        case SIGXFSZ:    return "SIGXFSZ - File size limit exceeded";
        case SIGVTALRM:  return "SIGVTALRM - Virtual alarm clock";
        case SIGPROF:    return "SIGPROF - Profiling alarm clock ";
        case SIGWINCH:   return "SIGWINCH - Window size change";
        case SIGIO:      return "SIGIO - I/O now possible";
        case SIGPWR:     return "SIGPWR - Power failure";
        case SIGSYS:     return "SIGSYS - Bad system call";
    }
    
    return "< unknown >";
}


const char *
carma::util::getTextForSiginfoSigno( const siginfo_t & siginfo ) {
    return getTextForSignalNumber( siginfo.si_signo );
}


const char *
carma::util::getTextForSiginfoCode( const siginfo_t & siginfo )
{
    switch ( siginfo.si_code ) {
        case SI_USER:     return "SI_USER - kill, sigsend or raise";
        case SI_KERNEL:   return "SI_KERNEL - the kernel";
        case SI_QUEUE:    return "SI_QUEUE - sigqueue";
        case SI_TIMER:    return "SI_TIMER - timer expired";
        case SI_MESGQ:    return "SI_MESGQ - mesq state changed";
        case SI_ASYNCIO:  return "SI_ASYNCIO - AIO completed";
        case SI_SIGIO:    return "SI_SIGIO - queued SIGIO";
    }
    
    if ( siginfo.si_signo == SIGILL ) {
        switch ( siginfo.si_code ) {
            case ILL_ILLOPC:
                return "ILL_ILLOPC - illegal opcode";
                
            case ILL_ILLOPN:
                return "ILL_ILLOPN - illegal operand";
                
            case ILL_ILLADR:
                return "ILL_ILLADR - illegal adressing mode";
                
            case ILL_ILLTRP:
                return "ILL_ILLTRP - illegal trap";
                
            case ILL_PRVOPC:
                return "ILL_PRIVOPC - privileged opcode";
                
            case ILL_PRVREG:
                return "ILL_PRVREG - privileged register";
                
            case ILL_COPROC:
                return "ILL_COPROC - coprocessor error";
                
            case ILL_BADSTK:
                return "ILL_BADSTK - internal stack error";
        }
    }
    
    if ( siginfo.si_signo == SIGFPE ) {
        switch ( siginfo.si_code ) {
            case FPE_INTDIV:
                return "FPE_INTDIV - integer divide by zero";
                
            case FPE_INTOVF:
                return "FPE_INTOVF - integer overflow";
                
            case FPE_FLTDIV:
                return "FPE_FLTDIV - floating point divide by zero";
                
            case FPE_FLTOVF:
                return "FPE_FLTOVF - floating point overflow";
                
            case FPE_FLTUND:
                return "FPE_FLTUND - floating point underflow";
                
            case FPE_FLTRES:
                return "FPE_FLTRES - floating point inexact result";
                
            case FPE_FLTINV:
                return "FPE_FLTINV - floating point invalid operation";
                
            case FPE_FLTSUB:
                return "FPE_FLTSUB - subscript out of range";
        }
    }
    
    if ( siginfo.si_signo == SIGSEGV ) {
        switch ( siginfo.si_code ) {
            case SEGV_MAPERR:
                return "SEGV_MAPERR - address not mapped to object";
                
            case SEGV_ACCERR:
                return "SEGV_ACCERR - invalid permissions for mapped object";
        }
    }

    if ( siginfo.si_signo == SIGBUS ) {
        switch ( siginfo.si_code ) {
            case BUS_ADRALN:
                return "BUS_ADRALN - invalid address alignment";
                
            case BUS_ADRERR:
                return "BUS_ADRERR - non-existent physical address";
                
            case BUS_OBJERR:
                return "BUS_OBJERR - object specific hardware error";
        }
    }
    
    if ( siginfo.si_signo == SIGTRAP ) {
        switch ( siginfo.si_code ) {
            case TRAP_BRKPT:
                return "TRAP_BRKPT - process breakpoint";
                
            case TRAP_TRACE:
                return "TRAP_TRACE - process trace trap";
        }
    }
    
    return "< unknown >";
}


string
carma::util::getStringForSiginfo( const siginfo_t & siginfo ) {
    ostringstream oss;
    
    oss << "si_signo=" << siginfo.si_signo
        << " (" << getTextForSiginfoSigno( siginfo ) << "), "
        << "si_errno=" << siginfo.si_errno << ", "
        << "si_code=" << siginfo.si_code
        << " (" << getTextForSiginfoCode( siginfo ) << ")";
        
    switch ( siginfo.si_signo ) {
        case SIGILL:
        case SIGFPE:
        case SIGSEGV:
        case SIGBUS:
            oss << ", si_addr=" << siginfo.si_addr;
            break;
            
        default:
            break;
    }
    
    return oss.str();
}


bool
carma::util::isSignalInSignalSet( const sigset_t & signalSet,
                                  const int        signalNumber )
{
    const int sigismemberResult = ::sigismember( &signalSet, signalNumber );

    if ( sigismemberResult == 1 )
        return true;
        
    if ( sigismemberResult == 0 )
        return false;
        
    const int savedErrorNo = errno;
    
    ostringstream oss;

    oss << "sigismember for signal number " << signalNumber
        << " (" << getTextForSignalNumber( signalNumber ) << ")"
        << " returned " << sigismemberResult
        << " (errno=" << savedErrorNo << ", "
        << strerror( savedErrorNo ) << ")";

    throw CARMA_ERROR( oss.str() );
}


void
carma::util::setSignalSetToEmpty( sigset_t & signalSet )
{
    const int sigemptysetResult = ::sigemptyset( &signalSet );

    if ( sigemptysetResult != 0 ) {
        const int savedErrorNo = errno;

        ostringstream oss;
    
        oss << "sigemptyset returned " << sigemptysetResult
            << " (errno=" << savedErrorNo << ", "
            << strerror( savedErrorNo ) << ")";
    
        throw CARMA_ERROR( oss.str() );
    }
}


void
carma::util::setSignalSetToFull( sigset_t & signalSet )
{
    const int sigfillsetResult = ::sigfillset( &signalSet );

    if ( sigfillsetResult != 0 ) {
        const int savedErrorNo = errno;

        ostringstream oss;
    
        oss << "sigfillset returned " << sigfillsetResult
            << " (errno=" << savedErrorNo << ", "
            << strerror( savedErrorNo ) << ")";
    
        throw CARMA_ERROR( oss.str() );
    }
}


void
carma::util::addSignalToSignalSet( sigset_t & signalSet,
                                   const int  signalNumber,
                                   const bool throwIfAlreadyInSet )
{
    if ( isSignalInSignalSet( signalSet, signalNumber ) ) {
        if ( throwIfAlreadyInSet ) {
            ostringstream oss;
     
            oss << "Signal number " << signalNumber
                << " (" << getTextForSignalNumber( signalNumber ) << ")"
                << " is already in the signal set";
                
            throw CARMA_ERROR( oss.str() );
        }
        
        return;
    }
        
    const int sigaddsetResult = ::sigaddset( &signalSet, signalNumber );
    
    if ( sigaddsetResult != 0 ) {
        const int savedErrorNo = errno;
        
        ostringstream oss;
 
        oss << "sigaddset for signal number " << signalNumber
            << " (" << getTextForSignalNumber( signalNumber ) << ")"
            << " returned " << sigaddsetResult
            << " (errno=" << savedErrorNo << ", "
            << strerror( savedErrorNo ) << ")";

        throw CARMA_ERROR( oss.str() );
    }
}


void
carma::util::removeSignalFromSignalSet( sigset_t & signalSet,
                                        const int  signalNumber,
                                        const bool throwIfNotInSet )
{
    if ( isSignalInSignalSet( signalSet, signalNumber ) == false ) {
        if ( throwIfNotInSet ) {
            ostringstream oss;
     
            oss << "Signal number " << signalNumber
                << " (" << getTextForSignalNumber( signalNumber ) << ")"
                << " is not in the signal set";
                
            throw CARMA_ERROR( oss.str() );
        }
        
        return;
    }
        
    const int sigdelsetResult = ::sigdelset( &signalSet, signalNumber );
    
    if ( sigdelsetResult != 0 ) {
        const int savedErrorNo = errno;
        
        ostringstream oss;
 
        oss << "sigdelset for signal number " << signalNumber
            << " (" << getTextForSignalNumber( signalNumber ) << ")"
            << " returned " << sigdelsetResult
            << " (errno=" << savedErrorNo << ", "
            << strerror( savedErrorNo ) << ")";

        throw CARMA_ERROR( oss.str() );
    }
}


int
carma::util::waitForSignalInSignalSet( const sigset_t & signalSet ) {
    int signalNumber;
    int sigwaitResult = 0;
    
    do {
        sigwaitResult = ::sigwait( &signalSet, &signalNumber );
    } while ( sigwaitResult == EINTR );

    if ( sigwaitResult != 0 ) {
        const int savedErrorNo = errno;
        
        ostringstream oss;
 
        oss << "sigwait returned "
            << sigwaitResult << " (" << strerror( sigwaitResult )
            << ", errno=" << savedErrorNo << ", "
            << strerror( savedErrorNo ) << ")";

        throw CARMA_ERROR( oss.str() );
    }
    
    if ( isSignalInSignalSet( signalSet, signalNumber ) == false ) {
        ostringstream oss;
 
        oss << "Received signal number " << signalNumber
            << " (" << getTextForSignalNumber( signalNumber ) << ") "
            << "which is not specified in the signal set";
        
        throw CARMA_ERROR( oss.str() );
    }
    
    return signalNumber;
}

