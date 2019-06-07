
/**
 *
 * Unit test for the ovro antenna monitor hierarchy.
 *
 * @author: Steve Scott
 *
 * $Id: ovroTest.cc,v 1.21 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.21 $ $Date: 2012/01/18 18:48:43 $
// @usage       ovroTest [imr=imrhost] [ant=#]
// @description
//      Test program for the ovro antenna monitoring hierarchy
// @key ant 1 integer antenna number
//
// @logger TEST_FACILITY carma.test.monitor.ovroTest
//

#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/MonitorSubsystemMaster.h"
#include "carma/monitor/MonitorPointIterator.h"

#include "carma/monitor/Test/utils.cc"


using namespace std;
using namespace carma::util;
using namespace carma::monitor;


int Program::main() 
{
    int antNo = getIntParameter("ant");
    bool canonical        = true;
    bool verbose          = false;
    bool value            = false;
    carma::util::Time time;
    cout.setf(ios::fixed);
    
    // Define subsystems
    OvroSubsystem&      a = *new OvroSubsystem(antNo);
    //OvroSubsystem      a(antNo);
    //OvroSubsystem&      b= *new OvroSubsystem(2);

    PhysicalDevice dev;
    //cout<<dev.toStringAll()<<endl;	
	
    MonitorComponent* mc;
    //MonitorContainer* con;
    //MonitorPoint*     mp;
    MonitorPointFloat*  s;
    //SensePointFloat*  t;
    //SensePointShort*  ss;
    //SerialNo*         sn;
    
    cout<<"-------------- Quick def of subsystems------------------"<<endl;
    cout<<"Subsystem:"<<a.toString(canonical, verbose, value)<<endl;
   
    cout<<"--------------Initial hierarchies to string (verbose)------------------"<<endl;
    cout<<a.hierarchyToString(true, true, true);
    //cout<<"  --------------now second ant..."<<endl;
    //cout<<b.hierarchyToString(true, true, true);
	
    //cout<<"--------------Initial hierarchies to string------------------"<<endl;
    cout<<a.hierarchyToString(canonical, verbose, value);
    //cout<<"  --------------now second ant..."<<endl;
    //cout<<b.hierarchyToString(canonical, verbose, value);
	
    cout<<"--------------SetNoData and then set ambTemp---------------"<<endl;
    a.setNoData();
    s = &a.cryo().compressor().ambientTemp();
    //t = &b.cryo().compressor().ambientTemp();
    s->setValue(1.2);
    cout<<"setval a"<<endl;
    cout<<s->toString(true, false, true)<<endl;
    //cout<<t->toString(true, false, true)<<endl;
    cout<<"setval b"<<endl;
    //t->setValue(6.7);
    cout<<s->toString(true, false, true)<<endl;
    //cout<<t->toString(true, false, true)<<endl;

    cout<<"-------------- Set timestamp------------------"<<endl;
    a.timestamp().setValue(time.MJD());
    cout<<"Timestamp set" <<endl;
    cout<<a.timestamp().toString(true, true)<<endl;

    cout<<"------------------Lookup by name--------------------"<<endl;
    string compName;
    compName ="Cryo.Compressor";
    mc = a.getComponentPtr(compName, true);
    cout<<"  ---lookup of ->"<<compName<<endl;
    if (mc == 0) cout<<"*****no component****"<<endl;
    else cout<<mc->hierarchyToString(false,false, true);
    compName ="Cryo.Compressor.ambientTemp";
    mc = a.getComponentPtr(compName, true);
    cout<<"  ---lookup of ->"<<compName<<endl;
    if (mc == 0) cout<<"*****no component****"<<endl;
    else cout<<mc->hierarchyToString(false,false, true);
    compName ="FrontEnd";
    mc = a.getComponentPtr(compName, true);
    cout<<"  ---lookup of ->"<<compName<<endl;
    if (mc == 0) cout<<"*****no component****"<<endl;
    else cout<<mc->hierarchyToString(false,false, true);
    compName ="Frontend";
    mc = a.getComponentPtr(compName, true);
    cout<<"  ---lookup of ->"<<compName<<endl;
    if (mc == 0) cout<<"*****no component****"<<endl;
    else cout<<mc->hierarchyToString(false,false, true);;

 
    cout<<"----------------------leafNodes----------------------------"<<endl;
    cout<<a.leafToString(false, true);

    cout<<"------------------------Tags---------------------------"<<endl;
    cout<<a.monitorPointTags()<<endl;

    cout<<"--------------------Missing Tags-----------------------"<<endl;
    cout<<a.monitorPointTags(true)<<endl;

    //cout<<"--------------------------carma--------------------------"<<endl;
    //MonitorContainer c("carma");
    //c.add(a); c.add(b);    
    //cout<<c.hierarchyToString(canonical, verbose, value);
    
    //cout<<"-----------------name lookup on carma-------------------"<<endl;
    //compName = "Ovro2.Cryo.Dewar.firstStageTemp";
    //cout<<"  ---lookup of ->"<<compName<<endl;
    //mc = c.getComponentPtr(compName, true);
    //cout<<mc->toString(canonical, true, value)<<endl;

    //cout<<"----------------------leafNodes--------------------------"<<endl;
    //cout<<c.leafToString(false, true);

 

    cout<<"---------------------resetValidity-----------------------"<<endl;
    a.resetValidity();
    cout << "subsystem.resetValidity() completed successfully" << endl;

    cout<<"-----------------monitorPointIterator-------------------"<<endl;
    MonitorPointIterator* itr;
    itr = new MonitorPointIterator(a);
    bool before = itr->hasMonitorPoint();
    bool after = before;
    while(after=(*itr)++) {
        after = itr->hasMonitorPoint();
        cout<<boolalpha<<before<<"/"<<after<<"  "
            <<itr->getMonitorPoint().getCanonicalName() << endl;
        before = after;
    }
    cout<<"Done: "<<boolalpha<<before<<"/"<<after<<"  "
        <<itr->getMonitorPoint().getCanonicalName() <<"\n"<< endl;
    
    itr = new MonitorPointIterator(a.drive());
    while(after=(*itr)++) {
        after = itr->hasMonitorPoint();
        cout<<boolalpha<<before<<"/"<<after<<"  "
            <<itr->getMonitorPoint().getCanonicalName() << endl;
        before = after;
    }
    cout<<"Done: "<<boolalpha<<before<<"/"<<after<<"  "
        <<itr->getMonitorPoint().getCanonicalName() <<"\n"<< endl;

    MonitorPointIterator it(a);
    before = it.hasMonitorPoint();
    after = before;
    while(after=it++) {
        after = it.hasMonitorPoint();
        cout<<boolalpha<<before<<"/"<<after<<"  "
            <<it.getMonitorPoint().getCanonicalName() << endl;
        before = after;
    }
    cout<<"Done: "<<boolalpha<<before<<"/"<<after<<"  "
        <<it.getMonitorPoint().getCanonicalName() <<"\n"<< endl;
    

    cout<<"-----------------setPersistent-------------------"<<endl;
    //a.timestamp().setPersistent(true);
    itr = new MonitorPointIterator(a.drive());
    while((*itr)++) {
        itr->getMonitorPoint().setPersistent(true);
    }
    cout<<a.hierarchyToString(true, true, true);
    cout<<a.timestamp().toString(true, true)<<endl;

    cout<<"-----------------averagingDataSet-------------------"<<endl;
    MonitorPointIterator itr1(a);
    while(itr1++) {
        //itr1.getMonitorPoint().setNumSamples(5);
    }
    

    cout<<"-------------------------write--------------------------"<<endl;
    try {
        cout << "Ready to write..." << endl;
        a.write();
        cout << "Write complete" << endl;
    } catch(CORBA::Exception& e) {
        cout << e << endl;
    } catch(std::exception& e) {
        cout << e.what() << endl;
        throw(e);
    }
    cout<<a.timestamp().toString(true, true)<<endl;
    
    cout<<"--------------------------modify local-------------------------"<<endl;
    //cout<<"strval:"<<(int)mpstr.getAveValidity()<<endl;

    cout<<"--------------------------read-------------------------"<<endl;
    //OvroSubsystem& b = *new OvroSubsystem(antNo);
    //b.read();
    //cout<<"b:"<<b.test().mpshort().toString(true)<<endl;
    int rep=0;
    a.readNewest();
    a.read();
    cout<<a.timestamp().toString(true, true)<<endl;
    //cout<<"numsamps:"<<mpd.getNumSamples()<<endl;
    if (verbose) {
        MonitorPointIterator itr2(a);
	while(itr2++) {
	    MonitorPoint& mp = itr2.getMonitorPoint();
	    cout<<mp.getName() << ":"<<mp.getNumSamples()<<endl;
	}
    }

    //a.startAutoWriter();
    //Loop...
    sleepUntilNextFrame();
    while(true) {
    cout<<"rep:"<<++rep<<endl;

    itr1.reset();
    while(itr1++) {
        MonitorPoint& m = itr1.getMonitorPoint();
        if (m.getNumSamples()>=3)m.setValidity(MonitorPoint::INVALID_HW_BAD, 2);
    }
    
    a.write();
    
    a.read();
    
    sleepUntilNextFrame();

    }
    
    cout<<"----------------successful completion-------------------"<<endl;
    return EXIT_SUCCESS;
}












