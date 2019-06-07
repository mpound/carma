/**
 * $Id: testParser.cc,v 1.12 2010/02/12 01:14:11 abeard Exp $
 *
 * @author Original Chul Gwon
 *
 * @version $Revision: 1.12 $
 * @usage testParser xml=<file.xml>
 * @description
 * testParser is a test program for parsing XML files using saxHandler
 *
 * @key  xml  @mandatory  string  name of xml file 
 *
 */

#include "carma/dbms/DBConnection.h"
#ifdef CARMA_DBMS_MYSQL
  #include "carma/dbms/MySQLDBConnection.h"
#endif
#include "carma/dbms/MPMLException.h"
#include "carma/dbms/SaxHandler.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include <iostream>
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransService.hpp>

using namespace std;
using namespace carma::dbms;

int carma::util::Program::main() {

    carma::util::frameType now = carma::util::Time::computeCurrentFrame();
    try {
        XMLPlatformUtils::Initialize();
    } catch (const XMLException &ex) {
        char *message = XMLString::transcode(ex.getMessage());
        std::cerr << "Error during initialization: \n"
                  << message
                  << std::endl;
        XMLString::release(&message);
        return 1;
    }

    const ::std::string xmlFileName =
        carma::util::Program::getStringParameter("xml");
        
    SAXParser *parser = new SAXParser();
    parser->setValidationScheme(SAXParser::Val_Auto);
    parser->setDoNamespaces(true);
    parser->setDoSchema(true);
    parser->setValidationSchemaFullChecking(true);

    SaxHandler *handler = new SaxHandler();
    parser->setDocumentHandler(handler);
    parser->setErrorHandler(handler);
    
    try {
        parser->parse(xmlFileName.c_str());
        vector<MonitorDescription> monitorDescriptions 
            = handler->getMonitorDescriptions();
        for(uint i=0; i < monitorDescriptions.size(); i++) {
            monitorDescriptions[i].setTime(now);            
        }
        cout << xmlFileName << ": total number of points " 
             << monitorDescriptions.size() << endl;
#ifdef CARMA_DBMS_MYSQL
        if(DBConnection::isUp(false)) {
            try {
                DBConnection *dbc = DBConnectionFactory
                    ::createConnection(false);
                if(dbc->getSubsystemTable().rowCount() == 0) {
                    dbc->populateSubsystemTable();
                }
                for(uint i = 0; i < monitorDescriptions.size(); i++) {
                    dbc->insertMonitorDescription(monitorDescriptions[i]);
                }
            } catch (const DBConnectionException &exc) {
                cerr << "DBConnectionException caught while loading data from "
                     << xmlFileName << endl;
                exc.report();
                return 1;
            } catch (const SQLException &exc) {
                cerr << "SQLException caught while loading data from "
                     << xmlFileName << endl;
                exc.report();
                return 1;
            }
        } else {
            cout << "Database cannot be contacted so descriptions cannot be "
                 << "loaded into it" << endl;
        }
#endif
    } catch (const XMLException &ex) {
        char *message = XMLString::transcode(ex.getMessage());
        std::cerr << "An error occurred: "
                  << message
                  << std::endl;
        XMLString::release(&message);
        XMLPlatformUtils::Terminate();
        return 1;
    } catch (const MPMLException &exc) {
        cerr << "MPMLException caught while parsing " << xmlFileName << endl;
        exc.report();
        return 1;
    } 
    
    delete parser;
    
    XMLPlatformUtils::Terminate();
    
    return 0;
}
