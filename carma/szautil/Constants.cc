#include "carma/szautil/Constants.h"

using namespace std;

using namespace sza::util;

Speed Constants::lightSpeed_ = 
Speed(Speed::CentimetersPerSec(), 2.99792458e10);

const double Constants::hPlanckCgs_      = 6.626176e-27;
const double Constants::kBoltzCgs_       = 1.381e-16;
const double Constants::JyPerCgs_        = 1e23;
const double Constants::sigmaTCgs_       = 6.6524e-25;
const double Constants::electronMassCgs_ = 9.1095e-28;
const double Constants::protonMassCgs_   = 1.6726e-24;
Temperature Constants::Tcmb_             = Temperature(Temperature::Kelvin(), 2.726);
Length Constants::au_                    = Length(Length::Meters(), 1.49597870e11);
Length Constants::defaultEarthRadius_    = Length(Length::Meters(), 6378137.000);

/**.......................................................................
 * Constructor.
 */
Constants::Constants() {}

/**.......................................................................
 * Destructor.
 */
Constants::~Constants() {}
