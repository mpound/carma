// $Id: DataCollectorTest.cc,v 1.1 2011/08/18 23:25:52 abeard Exp $

#include "carma/pipeline/DataCollectorTest.h"

#include "carma/pipeline/DataContainer.h"
#include "carma/correlator/lib/CorrelatorDataTestSZA.h"

#include <pthread.h>
#include <iomanip>
#include <math.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

using namespace std;
using namespace carma::pipeline;
using namespace carma::correlator::lib;


DataCollectorTest::DataCollectorTest() {
  _dataContainer = DataContainer::getInstance();
  start();
}

DataCollectorTest::DataCollectorTest(int bandNumber) {
  _dataContainer = DataContainer::getInstance();
  _bandNumber = bandNumber;
  start();
}

DataCollectorTest::~DataCollectorTest() {
}

void DataCollectorTest::start() {
  pthread_t p;
  pthread_create(&p, (pthread_attr_t*)NULL,
		 &DataCollectorTest::staticStart,
		 (void*)this);
}

void* DataCollectorTest::staticStart(void* arg) {
  ((DataCollectorTest*)arg)->update();
}

void DataCollectorTest::update() {
  // spin forever, writing fake data to container
  CorrelatorDataTestSZA cdsza;
  cdsza.setBandNumber(_bandNumber);
  while(true) {
    CorrelatorData cd(cdsza);
    _dataContainer->fillCorrelatorData(cd);
    usleep(500000);
  }
}
