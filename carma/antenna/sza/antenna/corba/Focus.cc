#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/corba/Focus.h"

#include "carma/szautil/Exception.h"

using namespace carma::antenna::common;
using namespace sza::antenna::corba;

Focus::Focus(sza::antenna::control::AntennaMaster* parent) : Proxy(parent)
{
}

void Focus::doZTracking(bool tracking, CORBA::ULong seq)
{
  std::cout << "Focus: setTracking() stub" << std::endl;
};

void Focus::setX(float position, CORBA::ULong seq)
{
  std::cout << "Focus: setX() stub" << std::endl;
};

void Focus::setY(float position, CORBA::ULong seq)
{
  std::cout << "Focus: setY() stub" << std::endl;
};

void Focus::setZ(float position, CORBA::ULong seq)
{
  std::cout << "Focus: setZ() stub" << std::endl;
};
