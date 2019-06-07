#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/util/Program.h"
#include "carma/util/ExceptionUtils.h"

#include <iostream>
#include <orbsvcs/CosNotifyChannelAdminC.h>

using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace {

void
dumpChannelContents( CosNotifyChannelAdmin::EventChannel_ptr ec ) 
{
    cout << "   Event channel contains " << flush;
    CosNotifyChannelAdmin::AdminIDSeq_var admins = 
        ec->get_all_consumeradmins();
    cout << admins->length() << " consumer admins:" << endl;

    CosNotifyChannelAdmin::ConsumerAdmin_var defaultConsumerAdmin = 
        ec->default_consumer_admin( );
    cout << "       Default admin (id=" << defaultConsumerAdmin->MyID()
        << ") contains " 
        << defaultConsumerAdmin->push_suppliers()->length() 
        << " push suppliers." << endl;

    for ( CORBA::ULong j = 0; j < admins->length(); ++j ) {

        CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin = 
            ec->get_consumeradmin( admins[j] );

        CosNotifyChannelAdmin::ProxyIDSeq_var pushSuppliers = 
            consumerAdmin->push_suppliers();

        const CORBA::ULong numSuppliers = pushSuppliers->length();
        cout << "       Consumer admin (id=" << consumerAdmin->MyID()
            << ") contains " << numSuppliers << " push suppliers." << endl;

        for ( CORBA::ULong k = 0; k < numSuppliers; ++k ) {

            CosNotifyChannelAdmin::ProxySupplier_var supplier = 
                consumerAdmin->get_proxy_supplier( pushSuppliers[k] );
            CosNotifyChannelAdmin::StructuredProxyPushSupplier_var 
                pushSupplier = CosNotifyChannelAdmin::
                StructuredProxyPushSupplier::_narrow( supplier );

        }

    }
} // dumpChannelContents

} // namespace < unnamed >


// @version $Revision: 1.3 $
//
// @usag tNotify imr=< imrhostname<:port> >
//
// @logger TEST_FACILITY  carma.test.corba.tNotify
//
// @key channel @noDefault string EventChannel name to resolve from nameservice.
//
// @description Test program to query notification server.
int Program::main( )
try {
    carma::corba::Server & server = getCorbaServer();
    carma::corba::Client & client = getCorbaClient();

    if ( parameterWasSpecified( "channel" ) ) {
        const string channelName = getStringParameter( "channel" );
        cout << "Retrieving " << channelName << " from name service." << endl;
        
        CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin = 
            client.resolveName< CosNotifyChannelAdmin::ConsumerAdmin >( 
                channelName );
        dumpChannelContents( consumerAdmin->MyChannel() );
        return 0;
    }

    cout << "Retrieving NotifyEventChannelFactory." << endl; 
    CosNotifyChannelAdmin::EventChannelFactory_var notifyChannelFactory;

    notifyChannelFactory = 
        client.resolveName< CosNotifyChannelAdmin::EventChannelFactory >(
            "NotifyEventChannelFactory" );

    cout << "Retrieving all channel ids... " << flush;

    CosNotifyChannelAdmin::ChannelIDSeq_var cids =
        notifyChannelFactory->get_all_channels( );

    cout << cids->length() << " retrieved." << endl;

    for ( CORBA::ULong i = 0; i < cids->length(); ++i ) {

        cout << "Retrieving EventChannel (id=" << cids[i] << ")..." << flush;
        CosNotifyChannelAdmin::EventChannel_var ec = 
            notifyChannelFactory->get_event_channel( cids[i] );
        cout << " done." << endl;

        dumpChannelContents( ec );
    }

    return 0;

} catch (...) {
    cerr << getStringForCaught() << endl;
    return 1;
}
