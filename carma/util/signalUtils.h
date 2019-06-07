#ifndef CARMA_UTIL_SIGNALUTILS_H
#define CARMA_UTIL_SIGNALUTILS_H

#include <string>
#include <signal.h>


namespace carma {
namespace util {


const char * getTextForSignalNumber( int signalNumber );

const char * getTextForSiginfoSigno( const ::siginfo_t & siginfo );
const char * getTextForSiginfoCode( const ::siginfo_t & siginfo );

::std::string getStringForSiginfo( const ::siginfo_t & siginfo );

//! @brief C++ wrapper for sigismember
bool isSignalInSignalSet( const ::sigset_t & signalSet,
                          int                signalNumber );

//! @brief C++ wrapper for sigemptyset
void setSignalSetToEmpty( ::sigset_t & signalSet );

//! @brief C++ wrapper for sigfillset
void setSignalSetToFull( ::sigset_t & signalSet );

//! @brief C++ wrapper for sigaddset
void addSignalToSignalSet( ::sigset_t & signalSet,
                           int          signalNumber,
                           bool         throwIfAlreadyInSet );

//! @brief C++ wrapper for sigaddset
void removeSignalFromSignalSet( ::sigset_t & signalSet,
                                int          signalNumber,
                                bool         throwIfNotInSet );

//! @brief C++ wrapper for sigwait
//! @return signal number that was received
int waitForSignalInSignalSet( const ::sigset_t & signalSet );

}  // namespace carma::util
}  // namespace carma


#endif
