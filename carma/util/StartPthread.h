#ifndef CARMA_UTIL_START_PTHREAD_H
#define CARMA_UTIL_START_PTHREAD_H

#include <stdexcept>
#include <string>

#include <pthread.h>

#include <log4cpp/NDC.hh>

#include "carma/util/PthreadAttr.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedPthreadCancelDisable.h"
#include "carma/util/ThreadQuit.h"

//! @file
//! Interface file for the carma::util::StartPthreadWithCopy routines

namespace carma {
namespace util {

//
// Basic idea here:
// IF:
//   1. You have a function F returning void and taking an argument of type
//      reference (or const reference) to T.
//
//   AND
//
//   2. You have an object X (or a reference to X or a const reference to X)
//      of type T
//
// THEN:
//   3. StartPthreadWithCopy( F, X ) will start a new pthread and call F with
//      a reference to a copy of your original X.
//
//   AND/OR
//
//   4. StartPthreadWithRef( F, X ) will start a new pthread and call F with
//      a reference to your original X.
//


//! @brief Starts up a new pthread
//! @param ep
//!        Thread entry point function pointer
//! @param arg
//!        Argument to copy and then use to invoke ep
//! @param initialLogNDC
//!        Initial logging NDC to start the new thread with
//! @param attr
//!        If given then it's pthread attributes to use when creating the
//!        new thread
//! @param startCancelable
//!        If @c false then the new thread will be created with pthread
//!        cancellation disabled
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( ArgType & a ),
    const ArgType &          arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc carma::util::StartPthreadWithCopy
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );

// @copydoc carma::util::StartPthreadWithCopy
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( const ArgType & a ),
    const ArgType &          arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc carma::util::StartPthreadWithCopy
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( const ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );

// @copydoc carma::util::StartPthreadWithCopy
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( const ArgType * & a ),
    ArgType * const &        arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc carma::util::StartPthreadWithCopy
template < typename ArgType >
::pthread_t StartPthreadWithCopy(
    void (*ep)( const ArgType * & a ),
    ArgType * const &     arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );


//! @brief Starts up a new pthread
//! @param ep
//!        Thread entry point function pointer
//! @param arg
//!        Argument to use directly (i.e. no copying) to invoke ep
//! @param initialLogNDC
//!        Initial logging NDC to start the new thread with
//! @param attr
//!        If non-NULL then it's pthread attributes to use when creating the
//!        new thread
//! @param startCancelable
//!        If @c false then the new thread will be created with pthread
//!        cancellation disabled
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( ArgType & a ),
    ArgType &                arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc StartPthreadWithRef
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( ArgType & a ),
    ArgType &             arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );

// @copydoc StartPthreadWithRef
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    const ArgType &          arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc StartPthreadWithRef
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );

// @copydoc StartPthreadWithRef
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    ArgType &                arg,
    const ::std::string &    initialLogNDC = ::std::string(),
    const ::pthread_attr_t * attr = 0,
    bool                     startCancelable = false );

// @copydoc StartPthreadWithRef
template < typename ArgType >
::pthread_t StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    ArgType &             arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    bool                  startCancelable = false );


// NOTE: Everything below is just implementation detail. Fugetaboutit.


//! @internal
namespace detail {


//! @internal
template < typename A, typename AT, typename AC >
class BoilerPlateInfo;


//! @internal
template < typename I >
void * BoilerPlate( void * bpInfoArg );


//! @internal
template < typename A, typename AT, typename AC >
::pthread_t StartPthreadCommon( void (*ep)( A ),
                                AT                       argTransfer,
                                const ::std::string &    initialLogNDC,
                                const ::pthread_attr_t * attr,
                                bool                     startCancelable );


template < typename A, typename AT, typename AC >
class BoilerPlateInfo {
    private:
        typedef void (*EpType)( A );

        typedef AC ArgCopyType;

        friend void * BoilerPlate< BoilerPlateInfo >( void * bpInfoArg );

        friend ::pthread_t StartPthreadCommon< A, AT, AC >(
            EpType                   ep,
            AT                       argTransfer,
            const ::std::string &    initialLogNDC,
            const ::pthread_attr_t * attr,
            bool                     startCancelable );

        BoilerPlateInfo( EpType                ep,
                         AT                    argTransfer,
                         const ::std::string & initialLogNDC,
                         bool                  startCancelable );

        PthreadMutex          clientCanContinueGuard_;
        PthreadCond           clientCanContinueCond_;
        bool                  clientCanContinue_;

        bool                  epReached_;

        const EpType          ep_;
        AT                    argTransfer_;

        const ::std::string & initialLogNDC_;
        const bool            startCancelable_;
};


}  // namespace carma::util::detail
}  // namespace carma::util
}  // namespace carma


template < typename A, typename AT, typename AC >
inline
carma::util::detail::BoilerPlateInfo< A, AT, AC >::BoilerPlateInfo(
    EpType                ep,
    AT                    argTransfer,
    const ::std::string & initialLogNDC,
    const bool            startCancelable ) :
clientCanContinue_( false ),
epReached_( false ),
ep_( ep ),
argTransfer_( argTransfer ),
initialLogNDC_( initialLogNDC ),
startCancelable_( startCancelable )
{
}


template < typename I >
void *
carma::util::detail::BoilerPlate( void * const bpInfoArg )
{
    bool clientCanContinueSignalled = false;

    I * info = static_cast< I * >( bpInfoArg );

    try {
        if ( info->initialLogNDC_.empty() == false )
            log4cpp::NDC::push( info->initialLogNDC_ );
        
        if ( info->startCancelable_ == false ) {
            int oldState = PTHREAD_CANCEL_DISABLE;

            if ( ::pthread_setcancelstate( PTHREAD_CANCEL_DISABLE,
                                           &oldState ) != 0 )
                throw std::runtime_error( "::pthread_setcancelstate failed" );
        }

        const ScopedThreadQuitRegisterSelf quitReg;

        typename I::EpType ep = info->ep_;
        typename I::ArgCopyType argCopy = info->argTransfer_;

        {
            const ScopedLock< PthreadMutex >
                lock( info->clientCanContinueGuard_ );

            info->clientCanContinue_ = true;
            info->epReached_ = true;

            // Notice that I signal the cond while still holding the mutex
            // My hope is that this will keep us from having the info block
            // get destructed out from under us while we are still in the
            // middle of either the pthread_cond_signal call or the
            // pthread_mutex_unlock call.
            info->clientCanContinueCond_.Signal();
        }

        clientCanContinueSignalled = true;

        info = 0;

        ep( argCopy );
    } catch ( ... ) {
        if ( clientCanContinueSignalled == false ) {
            try {
                const ScopedLock< PthreadMutex >
                    lock( info->clientCanContinueGuard_ );

                info->clientCanContinue_ = true;
                info->epReached_ = false;

                // Notice that I signal the cond while still holding the mutex
                // My hope is that this will keep us from having the info block
                // get destructed out from under us while we are still in the
                // middle of either the pthread_cond_signal call or the
                // pthread_mutex_unlock call.
                info->clientCanContinueCond_.Signal();
            } catch ( ... ) {
                // Just stifle the exception
            }
        }

        MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError();
    }

    return 0;
}


template < typename A, typename AT, typename AC >
::pthread_t
carma::util::detail::StartPthreadCommon(
    void (*ep)( A ),
    AT                             argTransfer,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    const ScopedPthreadCancelDisable cancelDisable;
    const ScopedThreadQuitDeferSelf quitDefer;

    typedef BoilerPlateInfo< A, AT, AC > InfoType;

    InfoType info( ep, argTransfer, initialLogNDC, startCancelable );

    ::pthread_t thread;
    
    if ( ::pthread_create( &thread,
                           attr,
                           BoilerPlate< InfoType >,
                           &info ) != 0 )
        throw std::runtime_error( "::pthread_create failed" );

    {
        const ScopedLock< PthreadMutex > lock( info.clientCanContinueGuard_ );

        while ( info.clientCanContinue_ == false )
            info.clientCanContinueCond_.Wait( info.clientCanContinueGuard_ );

        if ( info.epReached_ == false )
            throw std::runtime_error( "thread died before entry point call" );
    }

    return thread;
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( ArgType & a ),
    const ArgType &                arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    return
        detail::StartPthreadCommon
            < ArgType &,
              const ArgType &,
              ArgType >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    return
        detail::StartPthreadCommon
            < ArgType &,
              const ArgType &,
              ArgType >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( const ArgType & a ),
    const ArgType &                arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( const ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( const ArgType * & a ),
    ArgType * const &              arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    // This is a particularly gruesome case of mixing
    // const-ness of pointer and references.

    return
        detail::StartPthreadCommon
            < const ArgType * &,
              ArgType * const &,
              const ArgType * >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithCopy(
    void (*ep)( const ArgType * & a ),
    ArgType * const &     arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    // This is a particularly gruesome case of mixing
    // const-ness of pointer and references.

    return
        detail::StartPthreadCommon
            < const ArgType * &,
              ArgType * const &,
              const ArgType * >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( ArgType & a ),
    ArgType &                      arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    return
        detail::StartPthreadCommon
            < ArgType &,
              ArgType &,
              ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( ArgType & a ),
    ArgType &             arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    return
        detail::StartPthreadCommon
            < ArgType &,
              ArgType &,
              ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    const ArgType &                arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    const ArgType &       arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    ArgType &                      arg,
    const ::std::string &          initialLogNDC,
    const ::pthread_attr_t * const attr,
    const bool                     startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              attr,
              startCancelable );
}


template < typename ArgType >
::pthread_t
carma::util::StartPthreadWithRef(
    void (*ep)( const ArgType & a ),
    ArgType &             arg,
    const ::std::string & initialLogNDC,
    const PthreadAttr &   attr,
    const bool            startCancelable )
{
    return
        detail::StartPthreadCommon
            < const ArgType &,
              const ArgType &,
              const ArgType & >
            ( ep,
              arg,
              initialLogNDC,
              &(attr.InternalPthreadAttr()),
              startCancelable );
}


#endif
