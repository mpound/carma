#include "carma/services/TelescopeStatus.h"
using namespace carma::services;

TelescopeStatus::TelescopeStatus() :
    limits_(NO_LIMIT),
    blinded_(false),
    canBlind_(false)
{
}

TelescopeStatus::~TelescopeStatus() { }
