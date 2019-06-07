/**
 * @file tMonitorSystemWithThreshold.cc
 *
 * @author Chul Gwon
 *
 * @description
 *    simple test code for working out the MonitorSystemWithThreshold class
 *
 * @key mp "" s monitor point search string
 *
 * @logger TEST_FACILITY carma.test.monitor.tMonitorSystemWithThreshold
 *
 */

#include "carma/util/Program.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSystemWithThreshold.h"

#include <iostream>

using namespace std;

int carma::util::Program::main() {
  // pattern from which mp names will be searched
  std::string pattern = getStringParameter("mp");
  
  carma::monitor::CarmaMonitorSystem cms;
  carma::monitor::MonitorSystemWithThreshold mswt(cms);

  // retrieve threshold values
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

  // set threshold values
  try {
    mswt.setThresholdValues<bool>(pattern, carma::monitor::THRESHOLD_HIGH_ERROR_VALUE, (bool)10);
  } catch (const carma::util::ErrorException &ex) {
    std::cout << ex << std::endl;
  }

  mswt.setThresholdValues<float>(pattern, carma::monitor::THRESHOLD_LOW_ERROR_VALUE,  (float)5);
  mswt.setThresholdValues<float>(pattern, carma::monitor::THRESHOLD_HIGH_WARN_VALUE,  (float)12);
  mswt.setThresholdValues<float>(pattern, carma::monitor::THRESHOLD_LOW_WARN_VALUE,   (float)7);

  for ( mpn = mpNames.begin(); mpn != mpNames.end(); mpn++) {
    std::cout << "mp: " << *mpn << std::endl;
    std::cout << "err hi: " 
	      << mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_HIGH_ERROR_VALUE)
	      << std::endl;
    std::cout << "err lo: " 
	      << mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_ERROR_VALUE)
	      << std::endl;
    // test out the exception handling
    try {
      std::cout << "warn hi: " 
		<< mswt.getThresholdValue<short>(*mpn, carma::monitor::THRESHOLD_HIGH_WARN_VALUE)
		<< std::endl;
    } catch (const carma::util::ErrorException &ex) {
      std::cout << ex << std::endl;
    }
    std::cout << "warn lo: " 
	      << mswt.getThresholdValue<float>(*mpn, carma::monitor::THRESHOLD_LOW_WARN_VALUE)
	      << std::endl;
  }

  return 0;
}
