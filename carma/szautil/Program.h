#ifndef SZA_UTIL_PROGRAM_H
#define SZA_UTIL_PROGRAM_H

//-----------------------------------------------------------------------
// CARMA program library
//-----------------------------------------------------------------------
#include "carma/util/Program.h"

#include <string>

using namespace carma::util;

#define PROGRAM_KEYWORDS const KeyTabEntry carma::util::ProgramBase::kKeywords_[]
#define END_OF_KEYWORDS  0,0,0,0,0

#define getParameter getStringParameter
#define getiParameter getIntParameter
#define getbParameter getBoolParameter
#define getiParameter getIntParameter
#define isDefault !parameterWasSpecified

namespace carma {
  namespace util {
    void initializeUsage();
    static std::string programUsage_;
    static std::string programDescription_;
  }
};

#define PROGRAM_INITIALIZE_USAGE void carma::util::initializeUsage()
#define PROGRAM_USAGE programUsage_
#define PROGRAM_DESCRIPTION programDescription_

#define USAGE "",

const char* const carma::util::ProgramBase::kUsage_                = "";
const char* const carma::util::ProgramBase::kVersion_              = "";
const char* const carma::util::ProgramBase::kDescription_          = "";
const bool carma::util::ProgramBase::kHaveInitialLoggerInfo_       = true;
const char * const carma::util::ProgramBase::kInitialFacilityName_ = "DEFAULT_FACILITY";
const char * const carma::util::ProgramBase::kInitialLogname_      = "carma.antenna.sza";

#endif
