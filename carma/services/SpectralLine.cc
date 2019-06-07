/**
 * $Id: SpectralLine.cc,v 1.4 2005/04/07 15:57:44 cgwon Exp $
 *
 * @file SpectralLine.cc
 * @author Chul Gwon
 *
 */

#include "carma/services/SpectralLine.h"
#include "carma/services/Frequency.h"

using namespace carma::services;

// initialize with freq for CO
SpectralLine::SpectralLine() :
  frequency_(Frequency(115.271204, "Hz"))
{}

SpectralLine::~SpectralLine() {}

void SpectralLine::setFrequency(Frequency frequency) {
  frequency_ = frequency;
}

void SpectralLine::setTransition(std::string transition) {
  transition_ = transition;
}

Frequency SpectralLine::getFrequency() const {
  return frequency_;
}

std::string SpectralLine::getTransition() const {
  return transition_;
}
