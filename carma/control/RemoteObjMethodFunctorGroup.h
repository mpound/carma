#ifndef CARMA_CONTROL_REMOTE_OBJ_METHOD_FUNCTOR_GROUP_H
#define CARMA_CONTROL_REMOTE_OBJ_METHOD_FUNCTOR_GROUP_H


#include <map>
#include <set>
#include <string>

#include "carma/control/RemoteObjMethodFunctor.h"


namespace carma {
namespace control {


::std::string
makeRemoteObjCallString( const ::std::string & methodName,
                         const ::std::string & paramString );
                      

template < typename T, typename H >
::std::map< ::std::string, RemoteObjMethodFunctor0< T > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( ) );


template < typename T, typename H, typename A0 >
::std::map< ::std::string, RemoteObjMethodFunctor1< T, A0 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0 ),
    const A0 &                arg0 );


template < typename T, typename H, typename A0, typename A1 >
::std::map< ::std::string, RemoteObjMethodFunctor2< T, A0, A1 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1 ),
    const A0 &                arg0,
    const A1 &                arg1 );


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2 >
::std::map< ::std::string, RemoteObjMethodFunctor3< T, A0, A1, A2 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2 );


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3 >
::std::map< ::std::string, RemoteObjMethodFunctor4< T, A0, A1, A2, A3 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3 );


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
::std::map< ::std::string, RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3, A4 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4 );

template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4, 
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
::std::map< ::std::string, RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7, 
    const A8 &                arg8 );



// NOTE: Below here is simply implementation


template < typename T, typename H >
::std::map< ::std::string, RemoteObjMethodFunctor0< T > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( ) ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, RemoteObjMethodFunctor0< T > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor0< T >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr ) ) );
    }

    return result;
}


template < typename T, typename H, typename A0 >
::std::map< ::std::string, RemoteObjMethodFunctor1< T, A0 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0 ),
    const A0 &                arg0 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, RemoteObjMethodFunctor1< T, A0 > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor1< T, A0 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0 ) ) );
    }

    return result;
}


template < typename T, typename H, typename A0, typename A1 >
::std::map< ::std::string, RemoteObjMethodFunctor2< T, A0, A1 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1 ),
    const A0 &                arg0,
    const A1 &                arg1 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, RemoteObjMethodFunctor2< T, A0, A1 > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor2< T, A0, A1 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0,
                    arg1 ) ) );
    }

    return result;
}


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2 >
::std::map< ::std::string, RemoteObjMethodFunctor3< T, A0, A1, A2 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string,
                        RemoteObjMethodFunctor3< T, A0, A1, A2 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor3< T, A0, A1, A2 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0,
                    arg1,
                    arg2 ) ) );
    }

    return result;
}


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3 >
::std::map< ::std::string, RemoteObjMethodFunctor4< T, A0, A1, A2, A3 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string,
                        RemoteObjMethodFunctor4< T, A0, A1, A2, A3 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor4< T, A0, A1, A2, A3 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3 ) ) );
    }

    return result;
}


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
::std::map< ::std::string, RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3, A4 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string,
                        RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor5< T, A0, A1, A2, A3, A4 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4 ) ) );
    }

    return result;
}


template < typename T, typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
::std::map< ::std::string, 
            RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
makeRemoteObjMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    const ::std::string &     methodName,
    const ::std::string &     paramString,
    void                 (T::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string,
                        RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        RemoteObjHandleT< T > * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                RemoteObjMethodFunctor9< T, A0, A1, A2, A3, A4, A5, A6, A7, A8 >(
                    h,
                    makeRemoteObjCallString( methodName, paramString ),
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5,
                    arg6,
                    arg7,
                    arg8 ) ) );
    }

    return result;
}

}  // namespace carma::control
}  // namespace carma


#endif
