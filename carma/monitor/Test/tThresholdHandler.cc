/**
 * $Id: tThresholdHandler.cc,v 1.5 2010/02/12 01:14:52 abeard Exp $
 *
 * @author Original Chul Gwon
 *
 * @version $Revision: 1.5 $
 * @usage tThresholdHandler xml=<file.xml> mp=Ovro1.Tiltmeter.lrTilt
 * @description
 * test program for the ThresholdHandler - currently only works if you choose an MP of type float
 *
 * @key  xml  @mandatory  string  name of xml file 
 * @key  mp   Ovro*lrTilt string  string for preliminary lookup of threshold information
 *
 * @logger TEST_FACILITY carma.test.monitor.tThresholdHandler
 *
 */

#include "carma/monitor/ThresholdHandler.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSystemWithThreshold.h"
#include "carma/dbms/MPMLException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include <iostream>
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransService.hpp>

int carma::util::Program::main() {

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

    carma::monitor::CarmaMonitorSystem cms;
    carma::monitor::MonitorSystemWithThreshold mswt(cms);

    // retrieve threshold values
    std::string pattern = getStringParameter("mp");
    std::vector<std::string> mpNames = mswt.getMonitorPointNames(pattern);
    std::vector<std::string>::iterator mpn;
    for ( mpn = mpNames.begin(); mpn != mpNames.end(); mpn++) {
      std::cout << "mp: " << *mpn << std::endl;
      std::cout << "err hi: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_HIGH_ERROR_VALUE)
		<< std::endl;
      std::cout << "err lo: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_ERROR_VALUE)
		<< std::endl;
      std::cout << "warn hi: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_HIGH_WARN_VALUE)
		<< std::endl;
      std::cout << "warn lo: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_WARN_VALUE)
		<< std::endl;
    }

    carma::monitor::ThresholdHandler *handler = 
      new carma::monitor::ThresholdHandler(&mswt);
    parser->setDocumentHandler(handler);
    parser->setErrorHandler(handler);
    
    try {
        parser->parse(xmlFileName.c_str());
    } catch (const XMLException &ex) {
        char *message = XMLString::transcode(ex.getMessage());
        std::cerr << "An error occurred: "
                  << message
                  << std::endl;
        XMLString::release(&message);
        XMLPlatformUtils::Terminate();
        return 1;
    } catch (const carma::dbms::MPMLException &exc) {
        std::cerr << "MPMLException caught while parsing " << xmlFileName << std::endl;
        exc.report();
        return 1;
    } 
    
    // retrieve threshold values
    for ( mpn = mpNames.begin(); mpn != mpNames.end(); mpn++) {
      std::cout << "mp: " << *mpn << std::endl;
      std::cout << "err hi: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_HIGH_ERROR_VALUE)
		<< std::endl;
      std::cout << "err lo: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_ERROR_VALUE)
		<< std::endl;
      std::cout << "warn hi: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_HIGH_WARN_VALUE)
		<< std::endl;
      std::cout << "warn lo: " 
		<< mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_WARN_VALUE)
		<< std::endl;
    }

    delete parser;
    
    XMLPlatformUtils::Terminate();
    
    return 0;
}
