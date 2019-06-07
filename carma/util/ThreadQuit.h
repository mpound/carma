#ifndef CARMA_UTIL_THREAD_QUIT_H
#define CARMA_UTIL_THREAD_QUIT_H

#include <pthread.h>

namespace carma {
namespace util {


void ThreadQuitTestSelf( );

void RequestThreadQuit( ::pthread_t thread );

bool CaughtExceptionIsThreadQuitRequestedError( );

void RethrowCaughtExceptionIfThreadQuitRequestedError( );

bool MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError( );


class ScopedThreadQuitDeferSelf {
    public:
        explicit ScopedThreadQuitDeferSelf( );

        /* virtual */ ~ScopedThreadQuitDeferSelf( );

    private:
        // No copying
        ScopedThreadQuitDeferSelf( const ScopedThreadQuitDeferSelf & );
        ScopedThreadQuitDeferSelf & operator=( const ScopedThreadQuitDeferSelf & );
};


class ScopedThreadQuitRegisterSelf {
    public:
        explicit ScopedThreadQuitRegisterSelf( );

        /* virtual */ ~ScopedThreadQuitRegisterSelf( );

    private:
        // No copying
        ScopedThreadQuitRegisterSelf( const ScopedThreadQuitRegisterSelf & );
        ScopedThreadQuitRegisterSelf & operator=( const ScopedThreadQuitRegisterSelf & );
};


class ThreadQuitRequestHandler {
    public:
        virtual ~ThreadQuitRequestHandler( ) { }
        
        virtual void HandleQuitRequest( ::pthread_t thread ) = 0;
};


class ScopedThreadQuitRequestHandlerSelf {
    public:
        explicit ScopedThreadQuitRequestHandlerSelf( ThreadQuitRequestHandler & handler );

        /* virtual */ ~ScopedThreadQuitRequestHandlerSelf( );

    private:
        // No copying
        ScopedThreadQuitRequestHandlerSelf( const ScopedThreadQuitRequestHandlerSelf & );
        ScopedThreadQuitRequestHandlerSelf & operator=( const ScopedThreadQuitRequestHandlerSelf & );

        ::size_t handlerCookie_;
};


}  // namespace carma::util
}  // namespace carma

#endif
