
/**
 *
 * Unit test for monitor hierarchy.
 * This is still in rough shape as it is evolving from a program
 * that just printed out values for visual inspection.
 * Some features are now tested and failures reported.
 *
 * @author: Steve Scott
 *
 * $Id: monitorHierarchyTest.cc,v 1.26 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.26 $ $Date: 2012/01/18 18:48:43 $
// @usage       monitorHierarchyTest 
// @description
//      Test program for monitoring hierarchy, using the test subsystem. 
//      Does not use any transport.
//
// @key quiet true bool set true to see lots of output
//
// @logger TEST_FACILITY carma.test.monitor.monitorHierarchyTest
//

#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"


using namespace std;
using namespace carma::util;
using namespace carma::monitor;


int Program::main() 
{
    //int antNo = 5;
    const bool canonical        = true;
    const bool value            = false;

    const bool quiet = getBoolParameter("quiet");
    const bool loud  = !quiet;

    // Define subsystems
    TestSubsystem & a = *(new TestSubsystem);

    MonitorComponent* mc;
    MonitorPointFloat*  s;
    MonitorPointSerialNo*         sn;
    
    // Test sense point iterator
    MonitorPointIterator spi(a);
    if (loud) cout << "-------------- List sense points --------------" << endl;
    while (spi++) {
        MonitorPoint& mp = spi.getMonitorPoint();
        if (loud) cout << "SensePoint:" << mp.getCanonicalName() << endl;
    }
    
    if (loud) cout << "------------ Quick def of subsystems-----------"<<endl;
    const string sQuick = a.toString(canonical, false, value);
    if (loud) cout<<"Subsystem:"<<sQuick<<endl;
   
    if (loud) cout << "------------ Set timestamps--------------" << endl;
    a.timestamp().setValue(Time::MJD());
	
    if (loud) cout << "-----Initial hierarchies to terse string-----" << endl;
    const string sTerse = a.hierarchyToString(canonical, false, value);
    if (loud) cout << sTerse;
 	
    if (loud) cout << "----Initial hierarchies to verbose string----" << endl;
    const string sVerbose = a.hierarchyToString(canonical, true, value);
    if (loud) cout << sVerbose;
 	
    if (loud) cout << "---------SetNoData and then set mpfloat----------" << endl;
    a.setNoData();
    s = &a.box().canDevice(1).spfloat();
    s->setValue(1.2);
    if (loud) cout << "setval a" << endl;
    if (loud) cout << s->toString(true, false, true) << endl;

    if (loud) cout << "---------------Lookup by name----------------" << endl;
    string compName;
    compName ="Box.CanDevice1";
    mc = a.getComponentPtr(compName, true);
    if (mc == 0) {
        cout << "  ---lookup of ->" <<compName << " failed!!" << endl;
        return EXIT_FAILURE;
    }
    else {
        if (loud) cout << "  ---lookup of ->" << compName << endl;
        if (loud) cout << mc->hierarchyToString(false,false, true);
    }
    
    compName ="Box.CanDevice3.spdouble";
    mc = a.getComponentPtr(compName, true);
    if (mc == 0) {
        cout << "  ---lookup of ->" <<compName << " failed!!" << endl;
        return EXIT_FAILURE;
    }
    else {
        if (loud) cout << "  ---lookup of ->" << compName << endl;
        if (loud) cout << mc->hierarchyToString(false,false, true);
    }
    
    compName ="Fake";
    mc = a.getComponentPtr(compName, true);
    if (mc == 0) {
        cout << "  ---lookup of ->" <<compName << " failed!!" << endl;
        return EXIT_FAILURE;
    }
    else {
        if (loud) cout << "  ---lookup of ->" << compName << endl;
        if (loud) cout << mc->hierarchyToString(false,false, true);
    }

    compName ="Frontend";
    mc = a.getComponentPtr(compName, true);
    if (mc != 0) {
        cout << "  ---lookup of ->" <<compName 
             << " didn't failed when it should have!!" << endl;
        return EXIT_FAILURE;
    }

    cout << "----------------set serial numbers-----------------" << endl;
    sn = &a.box().canDevice(2).spserialno();
    sn->setValue(2681);
    cout << sn->toString(true) << endl;

    cout << "--------------------leafNodes---------------------" << endl;
    cout << a.leafToString(false, true);

    cout << "-----------------------Tags------------------------" << endl;
    cout << a.monitorPointTags() << endl;

    cout << "-------------------Missing Tags--------------------" << endl;
    cout << a.monitorPointTags(true) << endl;


    cout<<"--------------------datatypes----------------------" << endl;
    TestSubsystem::Box& d = a.box();
    int ival = 234;
    MonitorPointByte&  mpb  = d.mpbyte();
    MonitorPointShort& mps  = d.mpshort();
    MonitorPointInt&   mpi  = d.mpint();
    mpb.setValue(ival);
    mps.setValue(ival);
    mpi.setValue(ival);
    cout<<"set:"<<ival<<" "<<mpb.toString()<<endl;
    cout<<"set:"<<ival<<" "<<mps.toString()<<endl;
    cout<<"set:"<<ival<<" "<<mpi.toString()<<endl;

    double fval = 52998.12345678;
    MonitorPointFloat& mpf   = d.mpfloat();
    MonitorPointDouble& mpd  = d.mpdouble();
    MonitorPointAbstime& mpa = d.mpmjd();
    mpf.setValue(fval); mpf.setWidth(12); mpf.setPrecision(4);
    mpd.setValue(fval); mpd.setWidth(12); mpd.setPrecision(4);
    mpa.setValue(fval);
    cout<<"set:"<<fval<<" "<<mpf.toString()<<endl;
    cout<<"set:"<<fval<<" "<<mpd.toString()<<endl;
    cout<<"set:"<<fval<<" "<<mpa.toString()<<endl;
    mpa.setWidth(mpa.getWidth()+4); 
    mpa.setPrecision(3);
    cout<<"set:"<<fval<<" "<<mpa.toString()<<endl;

    MonitorPointString& mpstr = d.mpstring();
    string strval = "zyxwvutsrqponmlkjihgfedcba";
           strval = "zyxwvut";
    mpstr.setValue(strval);
    cout<<"set:"<<strval;
    cout<<"stringValue:"<<mpstr.getValue()<<endl;
    
    MonitorPointBool& mpbool = d.mpbool();
    bool bval = true;
    mpbool.setValue(bval);
    cout << "setBool:" << bval << "  " << mpbool.getValue() << endl;
    
    cout<<"------------------multiple samples--------------------"<<endl;
    mpi.setNumSamples(2);
    bool exceptionThrown = false;
    try {
        mpi.setValue(456, 2);
    }
    catch (exception& e) {
        exceptionThrown = true;
    }
    if (!exceptionThrown) {
        cerr << "Out of bounds sample not thrown" << endl;
        return EXIT_FAILURE;
    }
    mpi.setValue(456, 1);
    cout<<"MPi:"<<mpi.getValue(0)<<"/"<<mpi.getValue(1)<<endl;

    cout<<"------------------time series--------------------"<<endl;
    exceptionThrown = false;
    try {
        mpbool.setTimeSeries(false);
    }
    catch (exception& e) {
        exceptionThrown = true;
    }
    if (!exceptionThrown) {
        cerr << "Set timeSeries exception not thrown" << endl;
        return EXIT_FAILURE;
    }
    try {
        mpi.setTimeSeries(false);
    }
    catch (exception& e) {
        cerr << "MPint.setTimeSeries() throws erroneous exception" << endl;
        return EXIT_FAILURE;
    }
    
    
    
    string str("the quick brown fox jumped over the lazy dog, zyxwvutsrqp");
    mpstr.setValue(str);
    cout<<mpstr.toString(true)<<endl;
    mps.setValue(6661);
    cout<<mps.toString(true)<<endl;
    
    cout<<"----------------successful completion-------------------"<<endl;
    return EXIT_SUCCESS;
}
