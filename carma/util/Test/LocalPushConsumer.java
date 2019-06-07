package carma.util.Test;

import carma.util.NotificationConsumer;

public class LocalPushConsumer extends carma.util.NotificationConsumer {
    
    public LocalPushConsumer(org.omg.CORBA.ORB orb, String channelName) 
    throws org.omg.PortableServer.POAPackage.AdapterAlreadyExists,
           org.omg.PortableServer.POAPackage.InvalidPolicy,
           org.omg.CORBA.ORBPackage.InvalidName {
        super(orb, channelName);
    }
    
    public void push_structured_event(org.omg.CosNotification.StructuredEvent event) 
    throws org.omg.CosEventComm.Disconnected, org.omg.CORBA.SystemException {

        Integer value, supplierId;
        
        supplierId = event.filterable_data[0].value.extract_ulong();

        System.out.println("Consumed event from supplier "+supplierId);
        value = event.filterable_data[1].value.extract_ulong();
        
        System.out.println("nElements in event: "+event.filterable_data.length);

        try {
            org.omg.CosNotification.Property data = event.filterable_data[2];
            System.out.println("Type: "+data.value.type().id());
            float[] fData =  org.omg.CORBA.FloatSeqHelper.extract(data.value);
            System.out.println("First value is "+fData[0]);
        } catch (org.omg.CORBA.TypeCodePackage.BadKind ex) {
            ex.printStackTrace();
        } catch (org.omg.CORBA.SystemException ex) {
            ex.printStackTrace();
        }
        //  float dValue = event.filterable_data[2].value.extract_float();
    }
}

// vim: set ts=4 sts=4 sw=4 et:
