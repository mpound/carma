#include "carma/corba/Test/ObjectManagerImpl.h"

#include "carma/corba/Server.h"
#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/Hello_skel.h"
#include "carma/corba/Test/Hello_skel_tie.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/corba/Test/Terminator_skel.h"
#include "carma/corba/Test/Terminator_skel_tie.h"
#include "carma/corba/Test/Wait.h"
#include "carma/corba/Test/Wait_skel.h"
#include "carma/corba/Test/Wait_skel_tie.h"

#include <iostream>

using namespace carma::corba;
using namespace carma::corba::test;
using namespace std;

ObjectManagerImpl::ObjectManagerImpl( Server & server ) :
    helloImpl_( ),
    terminatorImpl_( server ),
    waitImpl_( )
{
    namespace POA_cct = POA_carma::corba::test;

    cout << "SERVER: Creating ObjectManagerImpl." << endl;

    server.addServant< POA_cct::Hello_tie >( helloImpl_, helloPtr_ );
    server.addServant< POA_cct::Terminator_tie >( terminatorImpl_, 
                                                  terminatorPtr_ );
    server.addServant< POA_cct::Wait_tie >( waitImpl_, waitPtr_ ); 
}

ObjectManagerImpl::~ObjectManagerImpl( ) 
{
    cout << "SERVER: Destroying ObjectManagerImpl." << endl;
}

Hello_ptr
ObjectManagerImpl::Hello( ) 
{
    cout << "SERVER: Retrieving Hello DO from ObjectManager." << endl;
    return Hello::_duplicate( helloPtr_ );
}

Terminator_ptr
ObjectManagerImpl::Terminator( )
{
    cout << "SERVER: Retrieving Terminator DO from ObjectManager." << endl;
    return Terminator::_duplicate( terminatorPtr_ );
}

Wait_ptr
ObjectManagerImpl::Wait( )
{
    cout << "SERVER: Retrieving Wait DO from ObjectManager." << endl;
    return Wait::_duplicate( waitPtr_ );
}
