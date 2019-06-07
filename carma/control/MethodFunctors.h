#ifndef CARMA_CONTROL_METHOD_FUNCTORS_H
#define CARMA_CONTROL_METHOD_FUNCTORS_H


namespace carma {
namespace control {


template < typename T >
class MethodFunctor0 {
    public:
        MethodFunctor0( T *       t,
                        void (T::*methodPtr)( ) );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( );
};


template < typename T,
           typename S0 >
class MethodFunctor1 {
    public:
        MethodFunctor1( T *        t,
                        void  (T::*methodPtr)( S0 ),
                        const S0 & s0 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0 );
        const S0  s0_;
};


template < typename T,
           typename S0,
           typename S1 >
class MethodFunctor2 {
    public:
        MethodFunctor2( T *        t,
                        void  (T::*methodPtr)( S0, S1 ),
                        const S0 & s0,
                        const S1 & s1 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1 );
        const S0  s0_;
        const S1  s1_;
};


template < typename T,
           typename S0,
           typename S1,
           typename S2  >
class MethodFunctor3 {
    public:
        MethodFunctor3( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2 );
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
};


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3  >
class MethodFunctor4 {
    public:
        MethodFunctor4( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2, S3 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2, S3 );
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
};


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4  >
class MethodFunctor5 {
    public:
        MethodFunctor5( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2, S3, S4 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2, S3, S4 );
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5 >
class MethodFunctor6 {
    public:
        MethodFunctor6( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2, S3, S4, S5 );
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6 >
class MethodFunctor7 {
    public:
        MethodFunctor7( T *        t,
                        void  (T::*methodPtr)(S0, S1, S2, S3, S4, S5, S6),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6 );        
        void operator()( );        
    private:
        T * const t_;
        void (T::*methodPtr_)(S0, S1, S2, S3, S4, S5, S6);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7 >
class MethodFunctor8 {
    public:
        MethodFunctor8(
            T* t,
            void  (T::*methodPtr)(S0, S1, S2, S3, S4, S5, S6, S7),
            const S0 & s0,
            const S1 & s1,
            const S2 & s2,
            const S3 & s3,
            const S4 & s4,
            const S5 & s5,
            const S6 & s6,
            const S7 & s7);
        void operator()( );        
    private:
        T * const t_;
        void (T::*methodPtr_)(S0, S1, S2, S3, S4, S5, S6, S7);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8 >
class MethodFunctor9 {
    public:
        MethodFunctor9( T *        t,
                        void  (T::*methodPtr)(S0, S1, S2, S3, S4, 
                                              S5, S6, S7, S8),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6,
                        const S7 & s7,
                        const S8 & s8);        
        void operator()( );       
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2, S3, S4, S5, S6, S7, S8);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
        const S8  s8_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9 >
class MethodFunctor10 {
    public:
        MethodFunctor10( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2, S3, S4, 
                                               S5, S6, S7, S8, S9 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6,
                        const S7 & s7,
                        const S8 & s8,
                        const S9 & s9 );        
        void operator()( );        
    private:
        T * const t_;
        void (T::*methodPtr_)(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
        const S8  s8_;
        const S9  s9_;
};

// MethodFunctor11
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10 >
class MethodFunctor11 {
    public:
        MethodFunctor11(T *        t,
                        void  (T::*methodPtr)(S0, S1, S2, S3, S4, 
                                              S5, S6, S7, S8, S9, S10),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6,
                        const S7 & s7,
                        const S8 & s8,
                        const S9 & s9,
                        const S10& s10);        
        void operator()( );        
    private:
        T * const t_;
        void (T::*methodPtr_)(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
        const S8  s8_;
        const S9  s9_;
        const S10 s10_;
};

// MethodFunctor12
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10,
           typename S11 >
class MethodFunctor12 {
    public:
        MethodFunctor12(T*        t,
                        void  (T::*methodPtr)(S0, S1, S2, S3, S4, 
                                              S5, S6, S7, S8, S9, S10, S11),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6,
                        const S7 & s7,
                        const S8 & s8,
                        const S9 & s9,
                        const S10& s10,
                        const S11& s11);        
        void operator()( );        
    private:
        T * const t_;
        void (T::*methodPtr_)(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
        const S8  s8_;
        const S9  s9_;
        const S10 s10_;
        const S11 s11_;
};

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10,
           typename S11,
           typename S12,
           typename S13,
           typename S14,
           typename S15,
           typename S16,
           typename S17,
           typename S18 >
class MethodFunctor19 {
    public:
        MethodFunctor19( T *        t,
                        void  (T::*methodPtr)( S0, S1, S2, S3, S4, 
                                               S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18 ),
                        const S0 & s0,
                        const S1 & s1,
                        const S2 & s2,
                        const S3 & s3,
                        const S4 & s4,
                        const S5 & s5,
                        const S6 & s6,
                        const S7 & s7,
                        const S8 & s8,
                        const S9 & s9,
                        const S10 & s10,
                        const S11 & s11,
                        const S12 & s12,
                        const S13 & s13,
                        const S14 & s14,
                        const S15 & s15,
                        const S16 & s16,
                        const S17 & s17,
                        const S18 & s18 );
        
        void operator()( );
        
    private:
        T * const t_;
        void (T::*methodPtr_)( S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18);
        const S0  s0_;
        const S1  s1_;
        const S2  s2_;
        const S3  s3_;
        const S4  s4_;
        const S5  s5_;
        const S6  s6_;
        const S7  s7_;
        const S8  s8_;
        const S9  s9_;
        const S10  s10_;
        const S11  s11_;
        const S12  s12_;
        const S13  s13_;
        const S14  s14_;
        const S15  s15_;
        const S16  s16_;
        const S17  s17_;
        const S18  s18_;
};

// NOTE: Below here is simply implementation


template < typename T >
MethodFunctor0< T >::MethodFunctor0(
    T * const t,
    void (T::*methodPtr)( ) ) :
t_( t ),
methodPtr_( methodPtr ) {
}


template < typename T >
void
MethodFunctor0< T >::operator()( ) {
    (t_->*methodPtr_)( );
}


template < typename T, typename S0 >
MethodFunctor1< T, S0 >::MethodFunctor1(
    T * const  t,
    void  (T::*methodPtr)( S0 ),
    const S0 & s0 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ) {
}


template < typename T, typename S0 >
void
MethodFunctor1< T, S0 >::operator()( ) {
    (t_->*methodPtr_)( s0_ );
}


template < typename T, typename S0, typename S1 >
MethodFunctor2< T, S0, S1 >::MethodFunctor2(
    T * const  t,
    void  (T::*methodPtr)( S0, S1 ),
    const S0 & s0,
    const S1 & s1 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ) {
}


template < typename T,
           typename S0,
           typename S1 >
void
MethodFunctor2< T, S0, S1 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_ );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2 >
MethodFunctor3< T, S0, S1, S2 >::MethodFunctor3(
    T * const  t,
    void  (T::*methodPtr)( S0, S1, S2 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ) {
}


template < typename T,
           typename S0,
           typename S1,
           typename S2 >
void
MethodFunctor3< T, S0, S1, S2 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_, s2_ );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3 >
MethodFunctor4< T, S0, S1, S2, S3 >::MethodFunctor4(
    T * const  t,
    void  (T::*methodPtr)( S0, S1, S2, S3 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ) {
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3 >
void
MethodFunctor4< T, S0, S1, S2, S3 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_, s2_, s3_ );
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4  >
MethodFunctor5< T, S0, S1, S2, S3, S4 >::MethodFunctor5(
    T * const  t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ) {
}


template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4  >
void
MethodFunctor5< T, S0, S1, S2, S3, S4 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_, s2_, s3_, s4_ );
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5 >
MethodFunctor6< T, S0, S1, S2, S3, S4, S5 >::MethodFunctor6(
    T * const  t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ) {
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5 >
void
MethodFunctor6< T, S0, S1, S2, S3, S4, S5 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_, s2_, s3_, s4_, s5_ );
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6 >
MethodFunctor7< T, S0, S1, S2, S3, S4, S5, S6 >::MethodFunctor7(
    T * const  t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5, S6 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ) {
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6 >
void
MethodFunctor7< T, S0, S1, S2, S3, S4, S5, S6 >::operator()( ) {
    (t_->*methodPtr_)( s0_, s1_, s2_, s3_, s4_, s5_, s6_ );
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7 >
MethodFunctor8< T, S0, S1, S2, S3, S4, S5, S6, S7 >::MethodFunctor8( 
    T *        t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5, S6, S7),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6,
    const S7 & s7) :
t_( t ),
methodPtr_(methodPtr),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ),
s7_( s7 ) { 
}

template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8 >
MethodFunctor9< T, S0, S1, S2, S3, S4, S5, S6, S7, S8 >::MethodFunctor9( 
    T *        t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5, S6, S7, S8),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6,
    const S7 & s7,
    const S8 & s8) :
t_( t ),
methodPtr_(methodPtr),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ),
s7_( s7 ),
s8_(s8) { 
}
        
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8 >
void
MethodFunctor9< T, S0, S1, S2, S3, S4, S5, S6, S7, S8 >::operator()( )
{
    (t_->*methodPtr_)(s0_, s1_, s2_, s3_, s4_, s5_, s6_, s7_, s8_);
}

//  MethodFunctor10       
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9 >
MethodFunctor10< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9 >::MethodFunctor10( 
    T *        t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5, S6, S7, S8, S9 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6,
    const S7 & s7,
    const S8 & s8,
    const S9 & s9 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ),
s7_( s7 ),
s8_( s8 ),
s9_( s9 ) { 
}
        
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9>
void
MethodFunctor10< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9 >::operator()( )
{
    (t_->*methodPtr_)( s0_, s1_, s2_, s3_, s4_, s5_, s6_, s7_, s8_, s9_);
}
        
//  MethodFunctor11       
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10 >
MethodFunctor11< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 >::MethodFunctor11( 
    T *        t,
    void  (T::*methodPtr)(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6,
    const S7 & s7,
    const S8 & s8,
    const S9 & s9,
    const S10& s10 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ),
s7_( s7 ),
s8_( s8 ),
s9_( s9 ),
s10_(s10) { 
}
        
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10 >
void
MethodFunctor11< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10 >::operator()( )
{
    (t_->*methodPtr_)(s0_, s1_, s2_, s3_, s4_, s5_, s6_, s7_, s8_, s9_, s10_);
}

// MethodFunctor19
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10,
           typename S11,
           typename S12,
           typename S13,
           typename S14,
           typename S15,
           typename S16,
           typename S17,
           typename S18>
  MethodFunctor19< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18 >::MethodFunctor19( 
    T *        t,
    void  (T::*methodPtr)( S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18 ),
    const S0 & s0,
    const S1 & s1,
    const S2 & s2,
    const S3 & s3,
    const S4 & s4,
    const S5 & s5,
    const S6 & s6,
    const S7 & s7,
    const S8 & s8,
    const S9 & s9,
    const S10 & s10,
    const S11 & s11,
    const S12 & s12,
    const S13 & s13,
    const S14 & s14,
    const S15 & s15,
    const S16 & s16,
    const S17 & s17,
    const S18 & s18 ) :
t_( t ),
methodPtr_( methodPtr ),
s0_( s0 ),
s1_( s1 ),
s2_( s2 ),
s3_( s3 ),
s4_( s4 ),
s5_( s5 ),
s6_( s6 ),
s7_( s7 ),
s8_( s8 ),
s9_( s9 ),
s10_( s10 ),
s11_( s11 ),
s12_( s12 ),
s13_( s13 ),
s14_( s14 ),
s15_( s15 ),
s16_( s16 ),
s17_( s17 ),
s18_( s18 ) { 
}
        
template < typename T,
           typename S0,
           typename S1,
           typename S2,
           typename S3,
           typename S4,
           typename S5,
           typename S6,
           typename S7,
           typename S8,
           typename S9,
           typename S10,
           typename S11,
           typename S12,
           typename S13,
           typename S14,
           typename S15,
           typename S16,
           typename S17,
           typename S18 >
        
void
  MethodFunctor19< T, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18 >::operator()( )
{
  (t_->*methodPtr_)( s0_, s1_, s2_, s3_, s4_, s5_, s6_, s7_, s8_, s9_, s10_, s11_, s12_, s13_, s14_, s15_, s16_, s17_, s18_ );
}

}  // namespace carma::control
}  // namespace carma


#endif
