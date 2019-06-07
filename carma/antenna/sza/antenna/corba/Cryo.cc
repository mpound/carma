#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/corba/Cryo.h"

#include "carma/szautil/Exception.h"

using namespace carma::antenna::common;
using namespace sza::antenna::corba;;
using namespace sza::util;

Cryo::Cryo(sza::antenna::control::AntennaMaster* parent) : Proxy(parent)
{}

void Cryo::turnCompressor(carma::antenna::common::SwitchState state)
{
  std::cout << "Cryo: turnCompressor() stub" << std::endl;
  return;
};

void Cryo::resetCompressor()
{
  std::cout << "Cryo: resetCompressor() stub" << std::endl;
  return;
};

void Cryo::fillCompressor()
{
  std::cout << "Cryo: fillCompressor() stub" << std::endl;
  return;
};

void Cryo::purgeCompressor()
{
  std::cout << "Cryo: purgeCompressor() stub" << std::endl;
  return;
};

void Cryo::reset()
{
  std::cout << "Cryo: reset() stub" << std::endl;
  return;
};
