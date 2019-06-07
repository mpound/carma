#ifndef CARMA_CONTROL_HANDLE_METHOD_FUNCTOR_GROUP_H
#define CARMA_CONTROL_HANDLE_METHOD_FUNCTOR_GROUP_H


#include <map>
#include <set>
#include <string>

#include "carma/control/MethodFunctors.h"

// Definitions for up to 5 arguments to method pointer.
// If invoked method needs more,  add to this file
// or consider consolidating your method arguments.
//
/**********************************************************************/
// NOTE: If you get compile time errors on makeHandleMethodFunctorGroup
// calls, check the number of parameters in your invocation.
// HandleMethodFunctorGroup.h only defines templates for
// up to 9 parameters and you'll get a error message:
//
// "no matching function call to to makeHandleMethodFunctorGroup(...) ...".
//
// ##### Note also you can get a similar error if you use references
// ##### anywhere in the method signature which you are dispatching.
// ##### This is a common error! (mwp)
/**********************************************************************/

namespace carma {
namespace control {


template < typename H >
::std::map< ::std::string, MethodFunctor0< H > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( ) );


template < typename H, typename A0 >
::std::map< ::std::string, MethodFunctor1< H, A0 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0 ),
    const A0 &                arg0 );


template < typename H, typename A0, typename A1 >
::std::map< ::std::string, MethodFunctor2< H, A0, A1 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1 ),
    const A0 &                arg0,
    const A1 &                arg1 );


template < typename H, typename A0, typename A1, typename A2 >
::std::map< ::std::string, MethodFunctor3< H, A0, A1, A2 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2 );


template < typename H, typename A0, typename A1, typename A2, typename A3 >
::std::map< ::std::string, MethodFunctor4< H, A0, A1, A2, A3 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3 );


template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
::std::map< ::std::string, MethodFunctor5< H, A0, A1, A2, A3, A4 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4 );


template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5 >
::std::map< ::std::string, MethodFunctor6< H, A0, A1, A2, A3, A4, A5 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4, 
    const A5 &                arg5 );


template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6 >
::std::map< ::std::string, MethodFunctor7< H, A0, A1, A2, A3, A4, A5, A6 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5, A6 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4, 
    const A5 &                arg5,
    const A6 &                arg6 );

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7>
::std::map< ::std::string, MethodFunctor8< H, A0, A1, A2, A3, A4, A5, A6, A7 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4, A5, A6, A7),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4, 
    const A5 &                arg5,
    const A6&                 arg6, 
    const A7&                 arg7);

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
::std::map< ::std::string, MethodFunctor9< H, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4, A5, A6, A7, A8),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4, 
    const A5 &                arg5,
    const A6&                 arg6, 
    const A7&                 arg7, 
    const A8&                 arg8);

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9 >
::std::map< ::std::string, MethodFunctor10< H, A0, A1, A2, A3, A4, 
                                               A5, A6, A7, A8, A9 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, 
                                          A5, A6, A7, A8, A9 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9 );

// 11 args                                           
template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9,
           typename A10 >
::std::map< ::std::string, MethodFunctor11< H, A0, A1, A2, A3, A4, 
                                           A5, A6, A7, A8, A9, A10 > >                                          
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, 
                                          A5, A6, A7, A8, A9, A10 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10 &               arg10 );

// 12 args                                           
template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9,
           typename A10,
           typename A11 >
::std::map< ::std::string, MethodFunctor12< H, A0, A1, A2, A3, A4, 
                                           A5, A6, A7, A8, A9, A10, A11 > >                                          
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4, 
                                         A5, A6, A7, A8, A9, A10, A11),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10 &               arg10,
    const A11&                arg11);

// 19 args
template < typename H,
   typename A0,
   typename A1,
   typename A2,
   typename A3,
   typename A4,
   typename A5,
   typename A6,
   typename A7,
   typename A8,
   typename A9,
   typename A10,
   typename A11,
   typename A12,
   typename A13,
   typename A14,
   typename A15,
   typename A16,
   typename A17,
   typename A18,
   typename A19 >
::std::map< ::std::string, MethodFunctor19< H, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, 
					  A11, A12, A13, A14, A15, A16, A17, A18 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10 &               arg10,
    const A11 &               arg11,
    const A12 &               arg12,
    const A13 &               arg13,
    const A14 &               arg14,
    const A15 &               arg15,
    const A16 &               arg16,
    const A17 &               arg17,
    const A18 &               arg18
    );

// NOTE: Below here is simply implementation


template < typename H >
::std::map< ::std::string, MethodFunctor0< H > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( ) ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor0< H > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor0< H >(
                    h,
                    methodPtr ) ) );
    }

    return result;
}


template < typename H, typename A0 >
::std::map< ::std::string, MethodFunctor1< H, A0 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0 ),
    const A0 &                arg0 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor1< H, A0 > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor1< H, A0 >(
                    h,
                    methodPtr,
                    arg0 ) ) );
    }

    return result;
}


template < typename H, typename A0, typename A1 >
::std::map< ::std::string, MethodFunctor2< H, A0, A1 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1 ),
    const A0 &                arg0,
    const A1 &                arg1 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor2< H, A0, A1 > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor2< H, A0, A1 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1 ) ) );
    }

    return result;
}


template < typename H, typename A0, typename A1, typename A2 >
::std::map< ::std::string, MethodFunctor3< H, A0, A1, A2 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor3< H, A0, A1, A2 > > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor3< H, A0, A1, A2 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2 ) ) );
    }

    return result;
}


template < typename H, typename A0, typename A1, typename A2, typename A3 >
::std::map< ::std::string, MethodFunctor4< H, A0, A1, A2, A3 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor4< H, A0, A1, A2, A3> > FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor4< H, A0, A1, A2, A3 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3 ) ) );
    }

    return result;
}


template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4 >
::std::map< ::std::string, MethodFunctor5< H, A0, A1, A2, A3, A4 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor5< H, A0, A1, A2, A3, A4 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor5< H, A0, A1, A2, A3, A4 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4 ) ) );
    }

    return result;
}


template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5 >
::std::map< ::std::string, MethodFunctor6< H, A0, A1, A2, A3, A4, A5 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, 
                        MethodFunctor6< H, A0, A1, A2, A3, A4, A5 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor6< H, A0, A1, A2, A3, A4, A5 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5 ) ) );
    }

    return result;
}

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6 >
::std::map< ::std::string, MethodFunctor7< H, A0, A1, A2, A3, A4, A5, A6 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5, A6 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, 
                        MethodFunctor7< H, A0, A1, A2, A3, A4, A5, A6 > >
            FunctorGroup;

    FunctorGroup result;
    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;
        if ( h == 0 ) continue;            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor7< H, A0, A1, A2, A3, A4, A5, A6 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5,
                    arg6 ) ) );
    }
    return result;
}

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7 >
::std::map< ::std::string, MethodFunctor8< H, A0, A1, A2, A3, A4, A5, A6, A7 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4, A5, A6, A7 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6, 
    const A7 &                arg7) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, 
                        MethodFunctor8< H, A0, A1, A2, A3, A4, A5, A6, A7 > >
            FunctorGroup;

    FunctorGroup result;
    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;
        if ( h == 0 ) continue;            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor8< H, A0, A1, A2, A3, A4, A5, A6, A7 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5,
                    arg6,
                    arg7 ) ) );
    }
    return result;
}

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8 >
::std::map< ::std::string, MethodFunctor9< H, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4, A5, A6, A7, A8),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6, 
    const A7 &                arg7,
    const A8&                 arg8) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, 
                        MethodFunctor9< H, A0, A1, A2, A3, A4, A5, A6, A7, A8 > >
            FunctorGroup;

    FunctorGroup result;
    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;
        if ( h == 0 ) continue;            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor9< H, A0, A1, A2, A3, A4, A5, A6, A7, A8 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5,
                    arg6,
                    arg7,
                    arg8) ) );
    }
    return result;
}

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9 >
::std::map< ::std::string, MethodFunctor10< H, A0, A1, A2, A3, A4,
                                           A5, A6, A7, A8, A9 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4,
                                          A5, A6, A7, A8, A9 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor10< H, A0, A1, A2, A3, A4,
                                                       A5, A6, A7, A8, A9 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor10< H, A0, A1, A2, A3, A4,
                                 A5, A6, A7, A8, A9 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5, 
                    arg6,
                    arg7,
                    arg8, 
                    arg9 ) ) );
    }

    return result;
 }

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9,
           typename A10 >
::std::map< ::std::string, MethodFunctor11< H, A0, A1, A2, A3, A4,
                                           A5, A6, A7, A8, A9, A10 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4,
                                          A5, A6, A7, A8, A9, A10 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10 &               arg10 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor11< H, A0, A1, A2, A3, A4,
                                                       A5, A6, A7, A8, A9, A10 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );
    for ( ; i != iEnd; ++i ) {
        H * const h = *i;
        if ( h == 0 ) continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor11< H, A0, A1, A2, A3, A4,
                                 A5, A6, A7, A8, A9, A10 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5, 
                    arg6,
                    arg7,
                    arg8, 
                    arg9,
                    arg10 ) ) );
    }

    return result;
 }

template < typename H,
           typename A0,
           typename A1,
           typename A2,
           typename A3,
           typename A4,
           typename A5,
           typename A6,
           typename A7,
           typename A8,
           typename A9,
           typename A10,
           typename A11 >
::std::map< ::std::string, MethodFunctor12< H, A0, A1, A2, A3, A4,
                                           A5, A6, A7, A8, A9, A10, A11 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)(A0, A1, A2, A3, A4,
                                         A5, A6, A7, A8, A9, A10, A11),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10&                arg10,
    const A11&                arg11) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor12< H, A0, A1, A2, A3, A4,
                                          A5, A6, A7, A8, A9, A10, A11 > >
            FunctorGroup;
    FunctorGroup result;
    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );
    for ( ; i != iEnd; ++i ) {
        H * const h = *i;
        if ( h == 0 ) continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor12< H, A0, A1, A2, A3, A4,
                                 A5, A6, A7, A8, A9, A10, A11 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5, 
                    arg6,
                    arg7,
                    arg8, 
                    arg9,
                    arg10,
                    arg11) ) );
    }
    return result;
 }

template < typename H,
  typename A0,
  typename A1,
  typename A2,
  typename A3,
  typename A4,
  typename A5,
  typename A6,
  typename A7,
  typename A8,
  typename A9,
  typename A10,
  typename A11,
  typename A12,
  typename A13,
  typename A14,
  typename A15,
  typename A16,
  typename A17,
  typename A18 >
  ::std::map< ::std::string, MethodFunctor19< H, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18 > >
makeHandleMethodFunctorGroup(
    const ::std::set< H * > & hGroup,
    void                 (H::*methodPtr)( A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18 ),
    const A0 &                arg0,
    const A1 &                arg1,
    const A2 &                arg2,
    const A3 &                arg3,
    const A4 &                arg4,
    const A5 &                arg5,
    const A6 &                arg6,
    const A7 &                arg7,
    const A8 &                arg8,
    const A9 &                arg9,
    const A10 &               arg10,
    const A11 &               arg11,
    const A12 &               arg12,
    const A13 &               arg13,
    const A14 &               arg14,
    const A15 &               arg15,
    const A16 &               arg16,
    const A17 &               arg17,
    const A18 &               arg18 ) {
    typedef typename ::std::set< H * >::const_iterator HGroupConstIterator;
    typedef ::std::map< ::std::string, MethodFunctor19< H, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18 > >
            FunctorGroup;

    FunctorGroup result;

    HGroupConstIterator i = hGroup.begin( );
    const HGroupConstIterator iEnd = hGroup.end( );

    for ( ; i != iEnd; ++i ) {
        H * const h = *i;

        if ( h == 0 )
            continue;
            
        result.insert(
            ::std::make_pair(
                h->doName( ),
                MethodFunctor19< H, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18 >(
                    h,
                    methodPtr,
                    arg0,
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5, 
                    arg6,
                    arg7,
                    arg8, 
                    arg9, 
                    arg10,
                    arg11,
                    arg12,
                    arg13,
                    arg14,
                    arg15, 
                    arg16,
                    arg17,
                    arg18 ) ) );
    }

    return result;
}


}  // namespace carma::control
}  // namespace carma


#endif
