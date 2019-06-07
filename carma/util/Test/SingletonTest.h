/** @file
 * CppUnit test fixture for carma::util::Singleton templatized class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2005/03/01 00:03:25 $
 * $Id: SingletonTest.h,v 1.5 2005/03/01 00:03:25 abeard Exp $
 */
#ifndef CARMA_UTIL_TEST_SINGLETONTEST_H
#define CARMA_UTIL_TEST_SINGLETONTEST_H

#include "carma/util/demangle.h"
#include "carma/util/Singleton.h"

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <typeinfo>
#include <iostream>

namespace carma {
namespace util {
namespace test {

static const bool verbose = false;

// -----------------------------------------------------------------------------
/**
 * Singleton test class to test subclassed singleton usage.
 * This class is templatized in order to allow easy testing of the 
 * various CreationPolicies.
 */
template<template<class> class CreationPolicy>
class Loner : public Singleton<Loner<CreationPolicy>, CreationPolicy > {
public:

    static unsigned int getInstanceCount() 
    {
        return nInstances_;
    };
   
protected:
    
   // No protected methods or data. 

private:

    // Required so that Singleton can have access to c-tor and d-tor.
    friend class CreationPolicy<Loner>;

    static unsigned int nInstances_;

    Loner() 
    {
        if (verbose) std::cout << "Constructing " 
            << carma::util::demangleTypeName(typeid(*this)) << std::endl;
        nInstances_++;
    }; 

    ~Loner() 
    {
        if (verbose) std::cout << "Destructing " 
            << carma::util::demangleTypeName(typeid(*this)) << std::endl;
        nInstances_--;
    };
    
};

template<template<class> class TEMPLATE>
unsigned int Loner<TEMPLATE>::nInstances_;

// -----------------------------------------------------------------------------
/**
 * Helper class to test Singleton usage as an adaptor.
 * Note this is templatized merely for testing in order to provide
 * unitialized test classes for each type of singleton tested.
 */
template<template<class> class TEMPLATE>
class Adapted {
public:

    Adapted();
    ~Adapted();

    static unsigned int getInstanceCount();

private:

    static unsigned int nInstances_;

};


template<template<class> class TEMPLATE>
Adapted<TEMPLATE>::Adapted()
{
    if (verbose) ::std::cout << "Constructing " 
        << carma::util::demangleTypeName(typeid(*this)) << std::endl;
    nInstances_++;
}

template<template<class> class TEMPLATE>
Adapted<TEMPLATE>::~Adapted()
{
    if (verbose) ::std::cout << "Destructing " 
        << carma::util::demangleTypeName(typeid(*this)) << std::endl;
    nInstances_--;
}

template<template<class> class TEMPLATE>
unsigned int Adapted<TEMPLATE>::getInstanceCount()
{
    return nInstances_;
}

template<template<class> class TEMPLATE>
unsigned int Adapted<TEMPLATE>::nInstances_ = 0;

// -----------------------------------------------------------------------------
/**
 * Main example cases listed in documentation.
 * It's good to make sure these work exactly as advertised.
 */
class Lonely : public Singleton<Lonely, CreateWithNewPolicy> {
public:    
    
    static unsigned int getInstanceCount() { return nInstances_; };

private:

    // Must declare CreationPolicy<TYPE> a friend to allow
    // internal access to your c'tor and d'tor.
    friend class CreateWithNewPolicy<Lonely>;

    // Tribute to the Beatles dark side...
    Lonely() { 
        nInstances_++;
        if (verbose) ::std::cout 
            << "Yes I'm Lonely, want to die..." << ::std::endl;
    };

    ~Lonely() { 
        nInstances_--;
        if (verbose) ::std::cout 
            << "If I ain't dead already "
            << "Ooh girl you know the reason why!" << ::std::endl;
    };

    static unsigned int nInstances_;
};

unsigned int Lonely::nInstances_;


// -----------------------------------------------------------------------------
/**
 * Templatized CppUnit test class for testing carma::util Singleton
 * class variants created using subclassing.
 */
template <class TYPE>
class SubclassedSingletonTest : public CppUnit::TestFixture {
public:

    void setUp();

    void tearDown();

    CPPUNIT_TEST_SUITE( SubclassedSingletonTest );

    CPPUNIT_TEST( verifyLazyOrStaticInitialization );
    CPPUNIT_TEST( verifyInstanceExists );
    CPPUNIT_TEST( verifyInstanceEquality );
    CPPUNIT_TEST( verifySingleInstanceOnly );

    CPPUNIT_TEST_SUITE_END();
    
    void verifyLazyOrStaticInitialization();
    void verifyInstanceExists();
    void verifyInstanceEquality();
    void verifySingleInstanceOnly();

private:

};

template <class TYPE>
void SubclassedSingletonTest<TYPE>::setUp()
{
    // No assembly required
}

template <class TYPE>
void SubclassedSingletonTest<TYPE>::tearDown()
{
    // No destruction required
}

template <class TYPE>
void SubclassedSingletonTest<TYPE>::verifyLazyOrStaticInitialization()
{
        CPPUNIT_ASSERT(TYPE::getInstanceCount() == 0);
        CPPUNIT_ASSERT(TYPE::instance().getInstanceCount() == 1);
}

template <class TYPE>
void SubclassedSingletonTest<TYPE>::verifyInstanceExists()
{
    TYPE& i1 = TYPE::instance();
    CPPUNIT_ASSERT(&i1);
}

template <class TYPE>
void SubclassedSingletonTest<TYPE>::verifyInstanceEquality()
{
    TYPE& i1 = TYPE::instance();
    TYPE& i2 = TYPE::instance();
    CPPUNIT_ASSERT(&i1 == &i2);
}

template <class TYPE>
void SubclassedSingletonTest<TYPE>::verifySingleInstanceOnly()
{
    CPPUNIT_ASSERT(TYPE::instance().getInstanceCount() == 1);
}



// -----------------------------------------------------------------------------
/**
 * Templatized CppUnit test class for testing carma::util Singleton
 * class variants created using subclassing.
 */
template <class TYPE, template <class> class CreationPolicy>
class AdaptedSingletonTest : public CppUnit::TestFixture {
public:

    void setUp();

    void tearDown();

    CPPUNIT_TEST_SUITE( AdaptedSingletonTest );

    CPPUNIT_TEST( verifyLazyOrStaticInitialization );
    CPPUNIT_TEST( verifyInstanceExists );
    CPPUNIT_TEST( verifyInstanceEquality );
    CPPUNIT_TEST( verifySingleInstanceOnly );

    CPPUNIT_TEST_SUITE_END();
    
    void verifyLazyOrStaticInitialization();
    void verifyInstanceExists();
    void verifyInstanceEquality();
    void verifySingleInstanceOnly();

private:

};

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::setUp()
{
    // No assembly required
}

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::tearDown()
{
    // No destruction required
}

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::verifyLazyOrStaticInitialization()
{
    CPPUNIT_ASSERT(TYPE::getInstanceCount() == 0);
    unsigned int nIns = 
        Singleton<TYPE, CreationPolicy >::instance().
        getInstanceCount();
    CPPUNIT_ASSERT(nIns == 1);
}

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::verifyInstanceExists()
{
    TYPE& i1 = Singleton<TYPE, CreationPolicy >::instance();
    CPPUNIT_ASSERT(&i1);
}

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::verifyInstanceEquality()
{
    TYPE& i1 = Singleton<TYPE, CreationPolicy >::instance();
    TYPE& i2 = Singleton<TYPE, CreationPolicy >::instance();
    CPPUNIT_ASSERT(&i1 == &i2);
}

template <class TYPE, template <class> class CreationPolicy>
void AdaptedSingletonTest<TYPE, CreationPolicy>::verifySingleInstanceOnly()
{
    unsigned int nIns = 
        Singleton<TYPE, CreationPolicy >::instance().
        getInstanceCount();
    CPPUNIT_ASSERT(nIns == 1);
}

// -----------------------------------------------------------------------------
/**
 * Simple test of CreateWithNewUnmanagedPolicy which attempts to verify the
 * main use case.
 */
class Automanaged : 
    public Singleton<Automanaged, CreateWithNewUnmanagedPolicy> {
public:

    static unsigned int getInstanceCount() 
    {
        return nInstances_;
    };

private:
    
    friend class CreateWithNewUnmanagedPolicy<Automanaged>;
    friend class std::auto_ptr<Automanaged>;

    Automanaged() {nInstances_++;};
    ~Automanaged() {
        Singleton<Automanaged, CreateWithNewUnmanagedPolicy>::destroyInstance();
        nInstances_--;
    };

    static unsigned int nInstances_;
};

unsigned int Automanaged::nInstances_ = 0;


class AutomanagedTest : public CppUnit::TestFixture {
public:
    
    void setUp();

    void tearDown();

    CPPUNIT_TEST_SUITE( AutomanagedTest );

    CPPUNIT_TEST( verifyAutoDestruction );
    CPPUNIT_TEST( verifyLogicException );

    CPPUNIT_TEST_SUITE_END();

    void verifyAutoDestruction();
    void verifyLogicException();
    
private:

};

void AutomanagedTest::setUp() {}

void AutomanagedTest::tearDown() {}

void AutomanagedTest::verifyAutoDestruction()
{
    {
        std::auto_ptr<Automanaged> automanaged(&(Automanaged::instance()));
    }
    
    CPPUNIT_ASSERT(Automanaged::getInstanceCount() == 0);
}

void AutomanagedTest::verifyLogicException()
{
    try {
        Automanaged& instance = Automanaged::instance();
        if (verbose) std::cout 
            << "AutomanagedTest::verifyLogicException: " 
            << instance.getInstanceCount() << " instances." << std::endl;
        throw 1;
    } catch (const std::logic_error& le) {
        if (verbose) std::cout 
            << "AutomanagedTest::verifyLogicException" << std::endl;
        CPPUNIT_ASSERT(1);
    } catch (...) {
        CPPUNIT_ASSERT(0);
    }
}

}}} // End namespace carma::util::test
#endif
