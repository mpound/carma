#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/LO1mmProxy.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

LO1mmProxy::LO1mmProxy(AntennaMaster* parent) : LOProxy(parent) {}

// Destructor function

LO1mmProxy::~LO1mmProxy() {}

void LO1mmProxy::setYigFrequency(double yigFreq)
{
  std::cout << "Calling: LO1mmProxy::setYigFrequency stub" << std::endl;
}

void LO1mmProxy::setLoFrequency(double frequency)
{
  std::cout << "Calling: LO1mmProxy::setLoFrequency stub" << std::endl;
}

void LO1mmProxy::toggleSweep(bool on)
{
  std::cout << "Calling: LO1mmProxy::toggleSweep stub" << std::endl;
}

void LO1mmProxy::toggleYigSweep(bool on)
{
  std::cout << "Calling: LO1mmProxy::toggleYigSweep stub" << std::endl;
}

void LO1mmProxy::setLoTerminatorAttenuation(unsigned short atten)
{
  std::cout << "Calling: LO1mmProxy::setLoTerminatorAttenuation stub" << std::endl;
}
