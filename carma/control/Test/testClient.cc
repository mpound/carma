/**
 *
 * Interactive test program for subarray control. 
 * Doesn't provide ability to check everything, just some things.
 *
 * @author: Steve Scott
 *
 * $Id: testClient.cc,v 1.25 2012/01/17 22:10:06 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
 
// Keyword setup...
// @version     $Revision: 1.25 $ $Date: 2012/01/17 22:10:06 $
// @usage control
// @key verbose        false                     bool    something or other verbose
// @key component      Carma.Ovro4.Drive.Azimuth string  monitor component to dump
// @key compInt        Carma.Test.Box.mpint      string  monitor integer to dump
// @key compDouble     Carma.Test.Box.mpdouble   string  monitor double to dump
// @key subarrayNumber 1                         int     Should be 1 through 5
// @key ant            4                         int     Carma antenna number
// @key source         3c273                     string  source for track command
// @key offset         0                         double  azimuth offset
//
// @logger TEST_FACILITY carma.test.control.testClient
//

 
#include <iostream>
#include <iomanip>

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl.h"
#include "carma/control/Subarray.h"
#include "carma/corba/Client.h"
#include "carma/util/Program.h"

                                                                                 
using namespace std;
using namespace carma::control;
using namespace carma::util;
using namespace carma;

class ControlTester {
public:
    ControlTester(int saNo) 
    {
        string name = Subarray::makeName(SUBARRAY_CONTROL_NAME, saNo);
        cerr << "testClient: SubarrayControlName = " 
                  << name << endl;
        corba::Client & client = Program::getProgram().getCorbaClient();
        con = client.resolveName<SubarrayControl>(name);
        if (CORBA::is_nil(con)) {
            cerr << "System reference not found for ControlSystem." << endl;
            exit(EXIT_FAILURE);
        }
    }
    
    void query(const string& component) 
    {
        string reply = "none";
        reply = con->query(component.c_str());
        cout << "monitorComponent=" << component 
             << "  reply:\n" << reply << endl;                                                                                                   
    }
    
    
private:
    SubarrayControl_var con;
};


int Program::main()
{
    const string component  = getStringParameter("component");
    const string compInt    = getStringParameter("compInt");
    const string compDouble = getStringParameter("compDouble");
    const int carmAntNo     = getIntParameter("ant");
    const int saNo          = getIntParameter("subarrayNumber");
    const string source     = getStringParameter("source");

    const bool offsetWasSpecified = parameterWasSpecified("offset");
    const double offset =
        (offsetWasSpecified ? getDoubleParameter("offset") : 0.0);
    
    const string saName = Subarray::makeName(SUBARRAY_CONTROL_NAME, saNo);
    
    ControlTester tester(saNo);
    corba::Client & client = Program::getProgram().getCorbaClient();
    SubarrayControl_var con = client.resolveName<SubarrayControl>(saName);
    if (CORBA::is_nil(con)) {
            cerr << "System reference not found for ControlSystem." << endl;
        exit(EXIT_FAILURE);
    }
     
    tester.query( component );

    try {  
        cout << "queryDouble(" << compDouble <<")=" 
             << con->queryDouble(compDouble.c_str()) << endl;
    } catch (ErrorException& e) {
        cerr << "Couldn't get value for component:" << compDouble << endl;
    }
    
    try {    
        cout << "queryInt(" << compInt <<")=" 
             << con->queryInt(compInt.c_str()) << endl;
    } catch (ErrorException& e) {
        cerr << "Couldn't get value for component:" << compInt << endl;
    }
    
    
    SeqShort carmaAntNoSeq;
    
    carmaAntNoSeq.length( 1 );
    carmaAntNoSeq[ 0 ] = 0;
    
    con->track(source.c_str(), carmaAntNoSeq, true, carma::control::ZERO, 20.0, false);
    cout << "track(" << source << ")" << endl;
    if (offsetWasSpecified) {
        con->offsetAz(offset, carmAntNo);
    }

    con->move(56.7, 67.8, carmAntNo);
    con->moveAz(78.9, carmAntNo);
    con->moveEl(90.1, carmAntNo);
    
    con->offset(123.4, 234.5, carmAntNo);
    con->offsetAz(345.6, carmAntNo);
    con->offsetEl(456.7, carmAntNo);

    con->track(source.c_str(), carmaAntNoSeq, true, carma::control::ZERO, 20.0, false);
    if (offsetWasSpecified) {
        con->offsetAz(offset, carmAntNo);
    }

    con->pad(3, 8);
    con->padOffset(3.4, 4.5, 5.6, carmAntNo);
    con->noiseSource(true,false);
    con->delay(901.2, carmAntNo);

    // resetting DO references
    //con->reset();

 
    return EXIT_SUCCESS;
 
}


