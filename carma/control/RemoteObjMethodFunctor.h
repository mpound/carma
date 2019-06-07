#ifndef CARMA_CONTROL_REMOTE_OBJ_METHOD_FUNCTOR_H
#define CARMA_CONTROL_REMOTE_OBJ_METHOD_FUNCTOR_H


#include <string>

#include "carma/control/RemoteObjHandleT.h"
#include "carma/util/Time.h"


namespace carma {
namespace control {


template < typename T >
class RemoteObjMethodFunctorBase {
    public:
        RemoteObjMethodFunctorBase(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString );
        
        virtual ~RemoteObjMethodFunctorBase( ) {  }
        
        void operator()( );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj ) = 0;
        
        RemoteObjHandleT< T > * const remoteObjHandle_;
        const ::std::string           remoteCallString_;
};


template < typename T >
class RemoteObjMethodFunctor0 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor0(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( ) );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( );
};


template < typename T,
           typename A0 >
class RemoteObjMethodFunctor1 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor1(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0 ),
            const A0 &              arg0 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0 );
        const A0  arg0_;
};


template < typename T,
           typename A0,
           typename A1 >
class RemoteObjMethodFunctor2 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor2(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0, A1 ),
            const A0 &              arg0,
            const A1 &              arg1 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0, A1 );
        const A0  arg0_;
        const A1  arg1_;
};


template < typename T,
           typename A0,
           typename A1,
           typename A2 >
class RemoteObjMethodFunctor3 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor3(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0, A1, A2 ),
            const A0 &              arg0,
            const A1 &              arg1,
            const A2 &              arg2 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0, A1, A2 );
        const A0  arg0_;
        const A1  arg1_;
        const A2  arg2_;
};


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3 >
class RemoteObjMethodFunctor4 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor4(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0, A1, A2, A3 ),
            const A0 &              arg0,
            const A1 &              arg1,
            const A2 &              arg2,
            const A3 &              arg3 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0, A1, A2, A3 );
        const A0  arg0_;
        const A1  arg1_;
        const A2  arg2_;
        const A3  arg3_;
};


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
class RemoteObjMethodFunctor5 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor5(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0, A1, A2, A3, A4 ),
            const A0 &              arg0,
            const A1 &              arg1,
            const A2 &              arg2,
            const A3 &              arg3,
            const A4 &              arg4 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0, A1, A2, A3, A4 );
        const A0  arg0_;
        const A1  arg1_;
        const A2  arg2_;
        const A3  arg3_;
        const A4  arg4_;
};


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
class RemoteObjMethodFunctor9 : public RemoteObjMethodFunctorBase< T > {
    public:
        RemoteObjMethodFunctor9(
            RemoteObjHandleT< T > * remoteObjHandle,
            const ::std::string &   remoteCallString,
            void               (T::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8 ),
            const A0 &              arg0,
            const A1 &              arg1,
            const A2 &              arg2,
            const A3 &              arg3,
            const A4 &              arg4,
            const A5 &              arg5,
            const A6 &              arg6,
            const A7 &              arg7,
            const A8 &              arg8 );
        
    private:
        virtual void makeRemoteCall( typename T::_ptr_type remoteObj );

        void (T::*methodPtr_)( A0, A1, A2, A3, A4, A5, A6, A7, A8 );
        const A0  arg0_;
        const A1  arg1_;
        const A2  arg2_;
        const A3  arg3_;
        const A4  arg4_;
        const A5  arg5_;
        const A6  arg6_;
        const A7  arg7_;
        const A8  arg8_;
};
// NOTE: Below here is simply implementation


template < typename T >
RemoteObjMethodFunctorBase< T >::RemoteObjMethodFunctorBase(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString ) :
remoteObjHandle_( remoteObjHandle ),
remoteCallString_( remoteCallString ) {
}


template < typename T >
void
RemoteObjMethodFunctorBase< T >::operator()( ) {
    if ( (remoteObjHandle_ != 0) && remoteObjHandle_->isObjReachable( ) ) {
        try {
            typename T::_var_type remoteObject = remoteObjHandle_->remoteObj( );

            if ( CORBA::is_nil( remoteObject ) == false ) {
                const double sendTime = util::Time::MJD( );
                
                makeRemoteCall( remoteObject );
                
                remoteObjHandle_->logSentCommandIfNeeded( remoteCallString_,
                                                          sendTime );
            }
        } catch ( const CORBA::Exception & ex ) {
            remoteObjHandle_->processException( remoteCallString_, ex );
        }
    }
}


template < typename T >
RemoteObjMethodFunctor0< T >::RemoteObjMethodFunctor0(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( ) ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ) {
}


template < typename T,
           typename A0 >
RemoteObjMethodFunctor1< T, A0 >::RemoteObjMethodFunctor1(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0 ),
    const A0 &              arg0 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ) {
}


template < typename T,
           typename A0,
           typename A1 >
RemoteObjMethodFunctor2< T, A0, A1 >::RemoteObjMethodFunctor2(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0, A1 ),
    const A0 &              arg0,
    const A1 &              arg1 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ),
arg1_( arg1 ) {
}


template < typename T,
           typename A0,
           typename A1,
           typename A2 >
RemoteObjMethodFunctor3< T, A0, A1, A2 >::RemoteObjMethodFunctor3(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0, A1, A2 ),
    const A0 &              arg0,
    const A1 &              arg1,
    const A2 &              arg2 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ),
arg1_( arg1 ),
arg2_( arg2 ) {
}


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3 >
RemoteObjMethodFunctor4< T, A0, A1, A2, A3 >::RemoteObjMethodFunctor4(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0, A1, A2, A3 ),
    const A0 &              arg0,
    const A1 &              arg1,
    const A2 &              arg2,
    const A3 &              arg3 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ),
arg1_( arg1 ),
arg2_( arg2 ),
arg3_( arg3 ) {
}


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 >::RemoteObjMethodFunctor5(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0, A1, A2, A3, A4 ),
    const A0 &              arg0,
    const A1 &              arg1,
    const A2 &              arg2,
    const A3 &              arg3,
    const A4 &              arg4 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ),
arg1_( arg1 ),
arg2_( arg2 ),
arg3_( arg3 ),
arg4_( arg4 ) {
}


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 >::RemoteObjMethodFunctor9(
    RemoteObjHandleT< T > * remoteObjHandle,
    const ::std::string &   remoteCallString,
    void               (T::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8 ),
    const A0 &              arg0,
    const A1 &              arg1,
    const A2 &              arg2,
    const A3 &              arg3,
    const A4 &              arg4,
    const A5 &              arg5,
    const A6 &              arg6,
    const A7 &              arg7,
    const A8 &              arg8 ) :
RemoteObjMethodFunctorBase< T >( remoteObjHandle, remoteCallString ),
methodPtr_( methodPtr ),
arg0_( arg0 ),
arg1_( arg1 ),
arg2_( arg2 ),
arg3_( arg3 ),
arg4_( arg4 ),
arg5_( arg5 ),
arg6_( arg6 ),
arg7_( arg7 ),
arg8_( arg8 ) {
}

template < typename T >
void
RemoteObjMethodFunctor0< T >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( );
}


template < typename T,
           typename A0 >
void
RemoteObjMethodFunctor1< T, A0 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_ );
}


template < typename T,
           typename A0,
           typename A1 >
void
RemoteObjMethodFunctor2< T, A0, A1 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_, arg1_ );
}


template < typename T,
           typename A0,
           typename A1,
           typename A2 >
void
RemoteObjMethodFunctor3< T, A0, A1, A2 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_, arg1_, arg2_ );
}


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3 >
void
RemoteObjMethodFunctor4< T, A0, A1, A2, A3 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_, arg1_, arg2_, arg3_ );
}


template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
void
RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_, arg1_, arg2_, arg3_, arg4_ );
}

template < typename T,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
void
RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 >::makeRemoteCall(
    typename T::_ptr_type remoteObj ) {
    (remoteObj->*methodPtr_)( arg0_, arg1_, arg2_, arg3_, 
                              arg4_, arg5_, arg6_, arg7_, arg8_ );
}


}  // namespace carma::control
}  // namespace carma


#endif
