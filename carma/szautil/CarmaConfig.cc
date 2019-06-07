#include "carma/szautil/CarmaConfig.h"

#include <iomanip>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CarmaConfig::CarmaConfig() 
{
  initializeCarmaPadLocations();
  initializeCurrentConfiguration();
}

/**.......................................................................
 * Destructor.
 */
CarmaConfig::~CarmaConfig() {}

/**.......................................................................
 * Return true if padNumber is occupied for the configuration
 * specified
 */
bool CarmaConfig::isOccupied(unsigned padNumber, unsigned configFlag)
{
  unsigned nPad = padLocations_.size();

  for(unsigned i=0; i < nPad; i++) {
    if(padLocations_[i].padNumber_ == padNumber) {
      return padLocations_[i].flags_ & configFlag;
    }
  }

  ThrowError("No pad number: " << padNumber << " found");
}

/**.......................................................................
 * Set up the CARMA pad locations
 */
void CarmaConfig::initializeCarmaPadLocations()
{
  padLocations_.resize(0);

  padLocations_.push_back(PadLocation(  1, -1357.28, -352.19,  44.19 + 2196.265, A_OVRO));
  padLocations_.push_back(PadLocation(  2, -1260.44,  668.77,  37.63 + 2196.265, A_OVRO));
  padLocations_.push_back(PadLocation(  3, -1015.50,  438.59,   3.71 + 2196.265, A_BIMA));
  padLocations_.push_back(PadLocation(  4,  -953.35, -131.35,  21.63 + 2196.265, A_BIMA));
  padLocations_.push_back(PadLocation(  5,  -686.44, -791.15,   3.71 + 2196.265, A_OVRO));
  padLocations_.push_back(PadLocation(  6,  -456.65,  134.73,  23.50 + 2196.265, A_BIMA|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation(  7,  -474.22, -147.81,  26.18 + 2196.265, A_BIMA|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation(  9,  -372.23, -994.38, -10.77 + 2196.265, A_OVRO));
  padLocations_.push_back(PadLocation( 10,  -328.25,  494.45,   3.71 + 2196.265, A_BIMA));
  padLocations_.push_back(PadLocation( 11,  -283.33,  231.35,   9.63 + 2196.265, A_BIMA|BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 12,  -275.78, -658.49,   3.71 + 2196.265, A_BIMA));
  padLocations_.push_back(PadLocation( 13,  -221.75, -168.88,   4.87 + 2196.265, A_BIMA|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation( 14,   112.27, -265.38,  -1.17 + 2196.265, A_BIMA|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation( 15,   192.79,  290.18,   2.58 + 2196.265, A_OVRO|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation( 20,  -236.78,  611.46,  13.13 + 2196.265, A_OVRO|BO_OVRO|B_OVRO));
  padLocations_.push_back(PadLocation( 23,  -225.80,   35.28, 2200.471, BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 24,  -155.44,  190.57, 2200.37 , BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 25,  -209.52,  -85.41, 2202.982, BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 26,   -44.59,   45.38, 2197.325, BO_BIMA|B_BIMA|DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 27,  -292.37,  112.73, 2204.672, BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 28,  -152.16,  375.58, 2213.232, BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 29,   -11.19,  -46.63, 2195.54 , BO_BIMA|CO_BIMA|DO_BIMA));
  padLocations_.push_back(PadLocation( 30,  -342.63,  -42.48, 2205.788, BO_BIMA|B_BIMA));
  padLocations_.push_back(PadLocation( 31,   -19.91,  -17.72, 2196.07 , CO_BIMA|C_BIMA|DO_BIMA|D_BIMA));
  padLocations_.push_back(PadLocation( 32,     0.00,    0.00, 2196.265, CO_BIMA|C_BIMA|DO_BIMA|D_BIMA|E_BIMA));
  padLocations_.push_back(PadLocation( 33,   -59.72,   12.99, 2196.86 , CO_OVRO|C_OVRO|DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 35,   -44.37,  -44.00, 2195.76 , B_BIMA|CO_BIMA|C_BIMA|DO_BIMA|D_BIMA));
  padLocations_.push_back(PadLocation( 36,    62.74,  -82.21, 2199.294, CO_BIMA|C_BIMA));
  padLocations_.push_back(PadLocation( 37,   112.86,   -4.88, 2200.49 , CO_OVRO|C_OVRO));
  padLocations_.push_back(PadLocation( 38,    88.19,   58.23, 2196.978, CO_OVRO|C_OVRO));
  padLocations_.push_back(PadLocation( 39,   -22.41, -117.19, 2194.774, CO_BIMA|C_BIMA));
  padLocations_.push_back(PadLocation( 40,   -65.05,  159.14, 2199.697, CO_OVRO|C_OVRO));
  padLocations_.push_back(PadLocation( 41,  -106.48,  114.87, 2198.903, CO_BIMA|C_BIMA));
  padLocations_.push_back(PadLocation( 42,  -139.46,   31.29, 2197.414, CO_BIMA|C_BIMA)),
  padLocations_.push_back(PadLocation( 43,   -99.95,  -26.05, 2196.592, CO_BIMA|C_BIMA));
  padLocations_.push_back(PadLocation( 44,  -120.91, -110.17, 2195.429, CO_OVRO|C_OVRO));
  padLocations_.push_back(PadLocation( 45,   -75.29, -212.97, 2193.684, CO_OVRO|C_OVRO));
  padLocations_.push_back(PadLocation( 46,   -11.89,   -7.21, 2196.246, DO_BIMA|D_BIMA|E_BIMA));
  padLocations_.push_back(PadLocation( 48,   -20.37,   -0.27, 2196.378, DO_BIMA|D_BIMA|E_BIMA));
  padLocations_.push_back(PadLocation( 49,    -2.62,  -16.78, 2196.12 , DO_BIMA|D_BIMA|E_OVRO));
  padLocations_.push_back(PadLocation( 51,    22.65,  -32.89, 2195.846, C_BIMA|DO_BIMA|D_BIMA));
  padLocations_.push_back(PadLocation( 52,    42.57,   -2.17, 2196.233, DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 53,    35.10,   22.32, 2196.389, DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 55,   -28.14,   63.00, 2197.07 , DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 58,   -42.00,  -10.62, 2196.402, DO_BIMA|D_BIMA));
  padLocations_.push_back(PadLocation( 60,   -34.95,  -84.87, 2195.48 , DO_OVRO|D_OVRO));
  padLocations_.push_back(PadLocation( 61,    -1.20,   10.60, 2196.18 , E_BIMA));
  padLocations_.push_back(PadLocation( 62,     8.23,   13.33, 2196.2  , D_BIMA|E_BIMA));
  padLocations_.push_back(PadLocation( 63,    -8.10,    3.15, 2196.14 , E_BIMA));
  padLocations_.push_back(PadLocation( 64,     6.80,    4.56, 2196.15 , E_BIMA));
  padLocations_.push_back(PadLocation( 66,    19.35,    3.72, 2196.1  , E_BIMA));
  padLocations_.push_back(PadLocation( 69,    11.65,   -8.96, 2196    , E_BIMA));
  padLocations_.push_back(PadLocation( 71,    23.65,   25.27, 2196.4  , E_OVRO));
  padLocations_.push_back(PadLocation( 72,     6.45,   33.52, 2196.42 , E_OVRO));
  padLocations_.push_back(PadLocation( 73,   -18.95,   36.17, 2196.67 , E_OVRO));
  padLocations_.push_back(PadLocation( 74,   -29.57,   15.84, 2196.686, E_OVRO));
  padLocations_.push_back(PadLocation( 75,    12.27,  -22.27, 2195.97 , E_OVRO));
  padLocations_.push_back(PadLocation( 76,    -9.35,  -33.38, 2195.857, H_SZA|L_SZA));
  padLocations_.push_back(PadLocation( 77,    -6.00,  -39.00, 2195.775, H_SZA|L_SZA|I_SZA));
  padLocations_.push_back(PadLocation( 78,    -4.36,  -33.63, 2195.746, H_SZA|I_SZA));
  padLocations_.push_back(PadLocation( 79,     0.84,  -35.64, 2195.727, H_SZA|L_SZA));
  padLocations_.push_back(PadLocation( 80,    -4.84,  -27.56, 2195.939, H_SZA|L_SZA));
  padLocations_.push_back(PadLocation( 81,     1.48,  -31.17, 2195.869, H_SZA|I_SZA));
  padLocations_.push_back(PadLocation( 82,     2.64,  -26.72, 2195.96 , L_SZA|I_SZA));
  padLocations_.push_back(PadLocation( 83,     1.65,  -42.58, 2195.677, L_SZA));
  padLocations_.push_back(PadLocation( 84,    -2.57,  -46.57, 2195.733, I_SZA));
  padLocations_.push_back(PadLocation( 85,   -16.23,  -32.83, 2195.86 , I_SZA));
  padLocations_.push_back(PadLocation( 86,   -13.13,  -23.32, 2196.051, I_SZA));
  padLocations_.push_back(PadLocation( 87,   -16.63,  -40.20, 2195.776, I_SZA|BP_SZA));
  padLocations_.push_back(PadLocation( 88,   -21.21,   25.95, 2200.000, H_SZA|L_SZA));
  padLocations_.push_back(PadLocation( 89,    44.79,  -12.05, 2200.000, H_SZA|L_SZA));

  padLocations_.push_back(PadLocation(101, -1335.35,  -345.11,  44.53 + 2196.265, AP_SZA));
  padLocations_.push_back(PadLocation(102, -1258.13,   643.90,  37.82 + 2196.265, AP_SZA));
  padLocations_.push_back(PadLocation(104,  -975.82,  -121.01,  21.78 + 2196.265, AP_SZA));
  padLocations_.push_back(PadLocation(106,  -476.44,   141.07,  22.91 + 2196.265, BP_SZA));
  padLocations_.push_back(PadLocation(107,  -498.09,  -151.72,  25.29 + 2196.265, AP_SZA|BP_SZA));
  padLocations_.push_back(PadLocation(109,  -352.16,  -988.16, -10.55 + 2196.265, AP_SZA));
  padLocations_.push_back(PadLocation(111,  -302.46,   234.58,   9.66 + 2196.265, BP_SZA));
  padLocations_.push_back(PadLocation(113,  -245.73,  -171.90,   5.20 + 2196.265, BP_SZA));
  padLocations_.push_back(PadLocation(114,    91.98,  -281.13,  -1.49 + 2196.265, AP_SZA|BP_SZA));
  padLocations_.push_back(PadLocation(115,   168.22,   292.95,   3.86 + 2196.265, AP_SZA|BP_SZA));
  padLocations_.push_back(PadLocation(120,  -260.65,   606.72,  11.35 + 2196.265, AP_SZA|BP_SZA));
}

/**.......................................................................
 * Return information about an antenna indicated in configFlag
 */
CarmaConfig::Antenna CarmaConfig::getAntennaInfo(unsigned configFlag)
{
  unsigned nant=0;

  Antenna ant;

  if(configFlag & BIMA) {
    ant.diameter_.setMeters(6.1);
    ant.sweptVolumeDiameter_.setMeters(2*5.6388);
    ant.elAxisHeight_.setMeters(5.198);
    ant.antFlag_ = BIMA;
    ++nant;
  }

  if(configFlag & OVRO) {
    ant.diameter_.setMeters(10.4);
    ant.sweptVolumeDiameter_.setMeters(2*7.043);
    ant.elAxisHeight_.setMeters(5.435);
    ant.antFlag_ = OVRO;
    ++nant;
  }

  if(configFlag & SZA) {
    ant.diameter_.setMeters(3.5);
    ant.sweptVolumeDiameter_.setMeters(4.41442);
    ant.elAxisHeight_.setMeters(2.7526);
    ant.antFlag_ = SZA;
    ++nant;
  }

  if(nant > 1) {
    ThrowError("Flags specify more than one antenna type");
  }

  return ant;
}

std::vector<CarmaConfig::PadLocation> CarmaConfig::getConfiguration(std::string name)
{
  std::vector<CarmaConfig::PadLocation> pads;

  if(name == "A") {
    pads = getConfiguration(A);
  } else if(name == "B") {
    pads = getConfiguration(B);
  } else if(name == "BO") {
    pads = getConfiguration(BO);
  } else if(name == "C") {
    pads = getConfiguration(C);
  } else if(name == "CO") {
    pads = getConfiguration(CO);
  } else if(name == "D") {
    pads = getConfiguration(D);
  } else if(name == "DO") {
    pads = getConfiguration(DO);
  } else if(name == "E") {
    pads = getConfiguration(E);
  } else if(name == "I") {
    pads = getConfiguration(I);
  } else if(name == "L") {
    pads = getConfiguration(L);
  } else if(name == "H") {
    pads = getConfiguration(H);
  } else if(name == "BP") {
    pads = getConfiguration(BP);
  } else if(name == "AP") {
    pads = getConfiguration(AP);
  } else {
    ThrowError("Unrecognized configuration: " << name);
  }

  return pads;
}

unsigned CarmaConfig::confNameToFlag(std::string name)
{
  if(name == "A") {
    return A;
  } else if(name == "B") {
    return B;
  } else if(name == "BO") {
    return BO;
  } else if(name == "C") {
    return C;
  } else if(name == "CO") {
    return CO;
  } else if(name == "D") {
    return D;
  } else if(name == "DO") {
    return DO;
  } else if(name == "E") {
    return E;
  } else if(name == "I") {
    return I;
  } else if(name == "L") {
    return L;
  } else if(name == "H") {
    return H;
  } else if(name == "BP") {
    return BP;
  } else if(name == "AP") {
    return AP;
  } else {
    return UNKNOWN;
  }
}

std::string CarmaConfig::confFlagToName(unsigned flag)
{
  if(flag == A) {
    return "A";
  } else if(flag == B) {
    return "B";
  } else if(flag == BO) {
    return "BO";
  } else if(flag == C) {
    return "C";
  } else if(flag == CO) {
    return "CO";
  } else if(flag == D) {
    return "D";
  } else if(flag == DO) {
    return "DO";
  } else if(flag == E) {
    return "E";
  } else if(flag == I) {
    return "I";
  } else if(flag == L) {
    return "L";
  } else if(flag == H) {
    return "H";
  } else if(flag == BP) {
    return "BP";
  } else if(flag == AP) {
    return "AP";
  } else {
    return "Unknown";
  }
}

ArrayConfig::Type CarmaConfig::confFlagToType(unsigned flag)
{
  if(flag == A) {
    return ArrayConfig::A;
  } else if(flag == B) {
    return ArrayConfig::B;
  } else if(flag == BO) {
    return ArrayConfig::BO;
  } else if(flag == C) {
    return ArrayConfig::C;
  } else if(flag == CO) {
    return ArrayConfig::CO;
  } else if(flag == D) {
    return ArrayConfig::D;
  } else if(flag == DO) {
    return ArrayConfig::DO;
  } else if(flag == E) {
    return ArrayConfig::E;
  } else if(flag == I) {
    return ArrayConfig::I;
  } else if(flag == L) {
    return ArrayConfig::L;
  } else if(flag == H) {
    return ArrayConfig::H;
  } else if(flag == BP) {
    return ArrayConfig::BP;
  } else if(flag == AP) {
    return ArrayConfig::AP;
  } else {
    return ArrayConfig::UNKNOWN;
  }
}

unsigned CarmaConfig::confTypeToFlag(ArrayConfig::Type type)
{
  if(type == ArrayConfig::A) {
    return A;
  } else if(type == ArrayConfig::B) {
    return B;
  } else if(type == ArrayConfig::BO) {
    return BO;
  } else if(type == ArrayConfig::C) {
    return C;
  } else if(type == ArrayConfig::CO) {
    return CO;
  } else if(type == ArrayConfig::D) {
    return D;
  } else if(type == ArrayConfig::DO) {
    return DO;
  } else if(type == ArrayConfig::E) {
    return E;
  } else if(type == ArrayConfig::I) {
    return I;
  } else if(type == ArrayConfig::L) {
    return L;
  } else if(type == ArrayConfig::H) {
    return H;
  } else if(type == ArrayConfig::BP) {
    return BP;
  } else if(type == ArrayConfig::AP) {
    return AP;
  } else {
    return UNKNOWN;
  }
}

std::vector<CarmaConfig::PadLocation> 
CarmaConfig::getConfiguration(unsigned flag)
{
  std::vector<PadLocation> pads;

  for(unsigned i=0; i < padLocations_.size(); i++) {
    PadLocation& loc = padLocations_[i];

    if(loc.flags_ & flag) {
      loc.ant_ = getAntennaInfo(loc.flags_ & flag);
      pads.push_back(loc);
    }
  }

  // If this is a recognized configuration, associate antenna numbers
  // with it

  if(flag & I_SZA) {
    associateSzaImagingConfiguration(pads);
  } else if(flag & H_SZA) {
    associateSzaHighDecConfiguration(pads);
  } else if(flag & L_SZA) {
    associateSzaLowDecConfiguration(pads);
  } else if(flag & BP_SZA) {
    associateSzaBPConfiguration(pads);
  } else if(flag & AP_SZA) {
    associateSzaAPConfiguration(pads);
  }

  return pads;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, CarmaConfig::PadLocation& pad)
{
  os << "Pad " << pad.padNumber_ << std::endl << std::endl
     << "E   " << pad.east_ << std::endl
     << "N   " << pad.north_ << std::endl
     << "U   " << pad.up_ << std::endl
     << std::endl
     << "Used in CARMA configurations: " << pad.listConfigs() << std::endl
     << std::endl
     << pad.ant_;
    
  return os;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, CarmaConfig::Antenna& ant)
{
  os << "Ant Type              = " << ant.antType() << std::endl; 

  os << "Ant#                  = ";
  if(ant.antNumber_ < 0) {
    os << "Unknown";
  } else {
    os << ant.antNumber_;
  }
  os << std::endl;

  os << "Diameter              = " << setw(12) << setprecision(6) 
     << ant.diameter_.centimeters() << " (cm)" << std::endl;

  os << "Swept Volume Diameter = " << setw(12) << setprecision(6) 
     << ant.sweptVolumeDiameter_.centimeters() << " (cm)" << std::endl;

  os << "Elevation Axis Height = " << setw(12) << setprecision(6) 
     << ant.elAxisHeight_.centimeters() << " (cm)" << std::endl;
    
  return os;
}

/**.......................................................................
 * Return the antenna type
 */
sza::util::AntennaType::Type CarmaConfig::Antenna::getAntType()
{
  if(antFlag_ & OVRO) {
    return AntennaType::OVRO;
  } else if(antFlag_ & BIMA) {
    return AntennaType::BIMA;
  } else if(antFlag_ & SZA) {
    return AntennaType::SZA;
  } else {
    return AntennaType::UNKNOWN;
  }
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
std::string CarmaConfig::Antenna::antType()
{
  std::ostringstream os;

  if(antFlag_ & OVRO) {
    os << "OVRO";
  } else if(antFlag_ & BIMA) {
    os << "BIMA";
  } else if(antFlag_ & SZA) {
    os << "SZA";
  } else {
    os << "Unknown";
  }

  return os.str();
}

/**.......................................................................
 * Return true if the antenna on this pad is shadowed by the passed
 * pad, with the antenna in the current pointing position
 */
bool CarmaConfig::PadLocation::isShadowed(Angle& az0, Angle& el0, PadLocation& pad1)
{
  bool shadowed = false;

  double dU1  = (pad1.up_.meters() + pad1.ant_.elAxisHeight_.meters()) - 
    (up_.meters() + ant_.elAxisHeight_.meters());

  double dN1  = pad1.north_.meters() - north_.meters();

  double dE1  = pad1.east_.meters()  - east_.meters();

  double mag  = sqrt(dE1*dE1 + dN1*dN1 + dU1*dU1);

  // Precompute cos/sin terms

  double saz = sin(az0.radians());
  double caz = cos(az0.radians());
  double sel = sin(el0.radians());
  double cel = cos(el0.radians());

  // Now get the coordinates (direction cosines) of the pointing vector

  double dU0 = mag*sel;
  double dE0 = mag*cel*saz;
  double dN0 = mag*cel*caz;

  double cdang = (dU0*dU1 + dE0*dE1 + dN0*dN1)/(mag*mag);
  double dang  = fabs(acos(cdang));

  double anglim = fabs(asin(((ant_.diameter_.meters() + pad1.ant_.sweptVolumeDiameter_.meters())/2)/mag));

  // We check that the projected separation of the dish center from
  // the center of the swept-volume sphere, orthogonal to the pointing
  // direction, is greater than the sum of the radii of the dish and
  // swept volume sphere

  if(dang < anglim) {
    shadowed = true;
  }

  return shadowed;
}

/**.......................................................................
 * Return true if the antenna on this pad is shadowed by the passed
 * pad, with the antenna in the current pointing position
 */
bool CarmaConfig::PadLocation::isShadowed(Angle& az0, Angle& el0, PadLocation& pad, 
					  bool useSweptVolumeDiameter,
					  Percent& percent, bool isPercentOfDiameter, bool verbose)
{
  bool shadowed = false;

  // Calculate the coordinates of the vector connecting the two
  // antennas

  double dU1  = (pad.up_.meters() + pad.ant_.elAxisHeight_.meters()) - 
    (up_.meters() + ant_.elAxisHeight_.meters());
  double dN1  =  pad.north_.meters() - north_.meters();
  double dE1  =  pad.east_.meters() - east_.meters();

  double mag  = sqrt(dE1*dE1 + dN1*dN1 + dU1*dU1);

  //  COUT("Magnitude of the vector is: " << mag);

  // Precompute cos/sin terms

  double saz = sin(az0.radians());
  double caz = cos(az0.radians());
  double sel = sin(el0.radians());
  double cel = cos(el0.radians());

  // Now get the coordinates (direction cosines) of the pointing vector

  double dU0 = mag*sel;
  double dE0 = mag*cel*saz;
  double dN0 = mag*cel*caz;

  double cdang = (dU0*dU1 + dE0*dE1 + dN0*dN1)/(mag*mag);
  double dang  = fabs(acos(cdang));
  double sep   = mag * sin(dang);
  double tsep  = mag * cos(dang);

  if(verbose) {
    COUT("Angle is: " << dang * 180/M_PI);
    COUT("Separation is: " << sep);
  }

  // Now get the radii to use in the shadowing calculation

  double radiusShadowedAnt  = ant_.diameter_.meters()/2;
  double radiusShadowingAnt = useSweptVolumeDiameter ? 
    pad.ant_.sweptVolumeDiameter_.meters()/2 :
    pad.ant_.diameter_.meters()/2;

  if(verbose) {
    COUT("Radius for the shadowing ant is: " << radiusShadowingAnt);
  }

  double sepLimit = radiusShadowedAnt + radiusShadowingAnt;

  if(verbose) {
    COUT("radiusShadowedAnt  " << radiusShadowedAnt);
    COUT("radiusShadowingAnt " << radiusShadowingAnt);
    COUT("Separation limit is now: " << sepLimit);
  }

  // If useSweptVolumeDiameter == true, we assume that the shadowing
  // telescope is stationary in our field of view, and use the more
  // conservative swept volume diameter.
  //
  // Else we assume the telescope is pointing in the same direction as
  // we are, and use the actual diameter.


  // We check that the projected separation of the dish center from
  // the center of the swept-volume sphere, orthogonal to the pointing
  // direction, is greater than the sum of the radii of the dish and
  // swept volume sphere.  If 

  if(verbose) {
    COUT("Sep =  " << sep << " sepLimit = " << sepLimit);
  }

  if(tsep > 0 && sep < sepLimit) {

    // If we are just checking percent of diameter overlap, the linear
    // overlap distance is all we need

    if(isPercentOfDiameter) {

      double linearOverlap = sepLimit - sep;
      double comp          = percent.percentMax1() * 2*radiusShadowedAnt;

      //      COUT("Linear overlap = " << linearOverlap);
      //      COUT("Checking against: " << percent.percentMax1() << " x " << 2*radiusShadowedAnt);

      shadowed = linearOverlap > comp;

      if(shadowed) {
	if(verbose) {
	  COUT("Ant is shadowed: overlap = " << linearOverlap << " comp = " << comp);
	}
      }

      // Else calculate the area of the dish that is actually shadowed

    } else {
     
      double area     = shadowedArea(radiusShadowedAnt, radiusShadowingAnt, sep);
      double compArea = percent.percentMax1() * M_PI*radiusShadowedAnt*radiusShadowedAnt;

      //      COUT("Area = " << area);
      //      COUT("Checking against: " << percent.percentMax1() << " x " << M_PI*radiusShadowedAnt*radiusShadowedAnt);

      shadowed = area > compArea;
    }
  }

  return shadowed;
}

/**.......................................................................
 * Return the overlap area of two circles
 *
 *                            |<---------- s ----->|
 *
 *                     _            _
 *                  -                  -        _     _
 *                -                      -   -           -  
 *              -                      /  --  \             -
 *            -                      /  -  | - \              -
 *           -                     /  -    |  - \               -
 *          -                    /   -     |   - \               -
 *         -                    /   -      |    - \               -
 *         -                  /t1  -       |    -t2\               -
 *         - <--- r1 ------- o --- - ---------- -- o ---- r2 ----> -
 *         -                       -       |    -                  -
 *         -                        -      |    -                 -
 *          -                        -     |   -                 -
 *           -                        -    |  -                 -
 *            -                         -  | -                 -
 *              -                         --                 -
 *                -                       -  -   _    _   -
 *                  -   _          _   -              
 *                      
 */
double CarmaConfig::PadLocation::shadowedArea(double radiusShadowedAnt, double radiusShadowingAnt, double sep) 
{
  double rMin = radiusShadowedAnt < radiusShadowingAnt ? radiusShadowedAnt : radiusShadowingAnt; 

  // Special case where the area of the shadowed ant is completely
  // contained within the area of the shadowing ant: shadowed area is
  // the area of the shadowed ant

  if(radiusShadowingAnt >= radiusShadowedAnt && (sep + radiusShadowedAnt) <= radiusShadowingAnt) {
    return M_PI * radiusShadowedAnt * radiusShadowedAnt;

  // Special case where the area of the shadowing ant is completely
  // contained within the area of the shadowed ant: shadowed area is
  // the area of the shadowing ant.

  } else if(radiusShadowedAnt > radiusShadowingAnt && (sep + radiusShadowingAnt) <= radiusShadowedAnt) {
    return M_PI * radiusShadowingAnt * radiusShadowingAnt;

    // If the separation is greater than the minimum radius, we use the identities:
    //
    // sep = r1 * cos(theta1) + r2 * cos(theta2)
    // r1 * sin(theta1) = r2 * sin(theta2)
    //
    // To solve for the overlap area

  } else if(sep >= rMin) {

    double r1      = radiusShadowedAnt;
    double r2      = radiusShadowingAnt;
    double cTheta1 = (sep*sep + r1*r1 - r2*r2) / (2 * sep * r1);
    double sTheta1 = sqrt(1.0 - cTheta1 * cTheta1);
    double sTheta2 = r1/r2 * sTheta1;
    double cTheta2 = (sep - r1 * cTheta1) / r2;
    double theta1  = atan2(sTheta1, cTheta1);
    double theta2  = atan2(sTheta2, cTheta2);

    double overlapArea1 = r1 * r1 * (theta1 - cTheta1 * sTheta1);
    double overlapArea2 = r2 * r2 * (theta2 - cTheta2 * sTheta2);
    double overlapArea  = overlapArea1 + overlapArea2;

    return overlapArea;

    // Else if the separation is closer than this, we use the identities:
    //
    // sep = r1 * cos(theta1) - r2 * cos(theta2)
    // r1 * sin(theta1) = r2 * sin(theta2)
    //
    // To solve for the overlap area

  } else {

    double r1      = radiusShadowedAnt > radiusShadowingAnt ? radiusShadowedAnt : radiusShadowingAnt; 
    double r2      = radiusShadowedAnt < radiusShadowingAnt ? radiusShadowedAnt : radiusShadowingAnt; 
    double cTheta1 = (sep*sep + r1*r1 - r2*r2) / (2 * sep * r1);
    double sTheta1 = sqrt(1.0 - cTheta1 * cTheta1);
    double sTheta2 = r1/r2 * sTheta1;
    double cTheta2 = (r1 * cTheta1 - sep) / r2; // Here is the only
						// difference
						// w.r.t. to the
						// previous case
    double theta1  = atan2(sTheta1, cTheta1);
    double theta2  = atan2(sTheta2, cTheta2);

    double overlapArea1 = r1 * r1 * (theta1 - cTheta1 * sTheta1);
    double overlapArea2 = r2 * r2 * (M_PI - (theta2 - cTheta2 * sTheta2)); // And
									   // the
									   // overlap
									   // area
									   // is
									   // the
									   // complement
									   // of the previous case
    double overlapArea  = overlapArea1 + overlapArea2;
    
    return overlapArea;
  }
}

/**.......................................................................
 * Associate a pad and antenna number
 */
void CarmaConfig::associatePadAndAntenna(std::vector<PadLocation>& pads, 
					 unsigned padNumber, unsigned antNumber)
{
  for(unsigned i=0; i < pads.size(); i++) {
    if(pads[i].padNumber_ == padNumber) {
      pads[i].ant_.antNumber_ = antNumber;
    }
  }
}

/**.......................................................................
 * Associate a pad in the current configuration with an antenna number
 */
void CarmaConfig::associatePadAndAntenna(unsigned padNumber, unsigned antNumber)
{
  for(unsigned i=0; i < currentConfiguration_.size(); i++) {
    if(currentConfiguration_[i].padNumber_ == padNumber) {
      currentConfiguration_[i].ant_.antNumber_ = antNumber;
    }
  }
}

/**.......................................................................
 * Return the SZA imaging configuration
 */
std::vector<CarmaConfig::PadLocation> CarmaConfig::getSzaImagingConfiguration()
{
  std::vector<PadLocation> pads = getConfiguration(I_SZA);
  return pads;
}

/**.......................................................................
 * Return the SZA B-array Paired-antenna configuration
 */
std::vector<CarmaConfig::PadLocation> CarmaConfig::getSzaBPConfiguration()
{
  std::vector<PadLocation> pads = getConfiguration(BP_SZA);
  return pads;
}

/**.......................................................................
 * SZA Imaging configuration, first used summer of 2008
 */
void CarmaConfig::
associateSzaImagingConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  associatePadAndAntenna(pads, 87, 0);
  associatePadAndAntenna(pads, 77, 1);
  associatePadAndAntenna(pads, 86, 2);
  associatePadAndAntenna(pads, 82, 3);
  associatePadAndAntenna(pads, 85, 4);
  associatePadAndAntenna(pads, 78, 5);
  associatePadAndAntenna(pads, 84, 6);
  associatePadAndAntenna(pads, 81, 7);
}

/**.......................................................................
 * A-array buddy-pad configuration, first used January OF 2009
 */
void CarmaConfig::
associateSzaAPConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  associatePadAndAntenna(pads, 102, 0);
  associatePadAndAntenna(pads, 101, 1);
  associatePadAndAntenna(pads, 114, 2);
  associatePadAndAntenna(pads, 109, 3);
  associatePadAndAntenna(pads, 115, 4);
  associatePadAndAntenna(pads, 107, 5);
  associatePadAndAntenna(pads, 104, 6);
  associatePadAndAntenna(pads, 120, 7);
}

/**.......................................................................
 * B-array buddy-pad configuration, first used Fall of 2008
 */
void CarmaConfig::
associateSzaBPConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  associatePadAndAntenna(pads,  87, 0);
  associatePadAndAntenna(pads, 111, 1);
  associatePadAndAntenna(pads, 114, 2);
  associatePadAndAntenna(pads, 106, 3);
  associatePadAndAntenna(pads, 115, 4);
  associatePadAndAntenna(pads, 107, 5);
  associatePadAndAntenna(pads, 113, 6);
  associatePadAndAntenna(pads, 120, 7);
}

/**.......................................................................
 * SZA low-dec configuration, first used 19 Feb 2009
 */
void CarmaConfig::
associateSzaLowDecConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  associatePadAndAntenna(pads, 77, 0);
  associatePadAndAntenna(pads, 80, 1);
  associatePadAndAntenna(pads, 89, 2);
  associatePadAndAntenna(pads, 88, 3);
  associatePadAndAntenna(pads, 82, 4);
  associatePadAndAntenna(pads, 83, 5);
  associatePadAndAntenna(pads, 76, 6);
  associatePadAndAntenna(pads, 79, 7);
}

/**.......................................................................
 * SZA configuration most similar to the valley-floor configuration.
 * Not yet used at the high site.
 */
void CarmaConfig::
associateSzaHighDecConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  associateSzaImagingConfiguration(pads);
}

/**.......................................................................
 * Return a configuration, sorted by antenna number
 */
std::vector<CarmaConfig::PadLocation> 
CarmaConfig::sortByAntNumber(std::vector<CarmaConfig::PadLocation>& pads)
{
  std::vector<PadLocation> sorted;

  // Find the lowest antenna number

  int iAntLow = pads[0].ant_.antNumber_;

  for(unsigned iPad=0; iPad < pads.size(); iPad++) {
    iAntLow = (pads[iPad].ant_.antNumber_ < iAntLow) ? 
      pads[iPad].ant_.antNumber_ : iAntLow;
  }

  if(iAntLow < 0) {
    ThrowError("Antenna numbers haven't been initialized");
  }

  // Assume antennas are numbered consecutively from the lowest found

  for(unsigned iAnt=iAntLow; iAnt < iAntLow+pads.size(); iAnt++) {

    // For this antenna index, look for the pad with matching antenna

    for(unsigned iPad=0; iPad < pads.size(); iPad++) {
      if(pads[iPad].ant_.antNumber_ == (int) iAnt) {
	sorted.push_back(pads[iPad]);
      }
    }

  }

  return sorted;
}

CarmaConfig::PadLocation CarmaConfig::getPadByNumber(unsigned padNumber)
{
  for(unsigned i=0; i < padLocations_.size(); i++) {
    if(padLocations_[i].padNumber_ == padNumber)
      return padLocations_[i];
  }

  ThrowError("Invalid pad number: " << padNumber);
}

void CarmaConfig::initializeCurrentConfiguration()
{
  currentConfFlag_ = NONE;
  currentConfiguration_.resize(0);
}

void CarmaConfig::setCurrentConfiguration(unsigned flag)
{
  currentConfFlag_ = flag;
  currentConfiguration_ = getConfiguration(flag);
}

void CarmaConfig::setCurrentConfiguration(ArrayConfig::Type type)
{
  currentConfFlag_ = confTypeToFlag(type);
  currentConfiguration_ = getConfiguration(currentConfFlag_);
}

void CarmaConfig::setCurrentConfiguration(std::string name)
{
  currentConfFlag_ = confNameToFlag(name);
  currentConfiguration_ = getConfiguration(name);
}

void CarmaConfig::addPad(std::vector<CarmaConfig::PadLocation>& pads)
{
  for(unsigned i=0; i < pads.size(); i++) {
    addPad(pads[i].padNumber_, pads[i].ant_.antFlag_, pads[i].ant_.antNumber_);
  }
}

void CarmaConfig::addPad(unsigned padNumber, unsigned antType, int antNumber)
{
  // Check if this pad is already in the current configuration.  If it
  // is, reassign the antenna on that pad to the new type

  for(unsigned i=0; i < currentConfiguration_.size(); i++) {
    if(currentConfiguration_[i].padNumber_ == padNumber) {
      currentConfiguration_[i].ant_ = getAntennaInfo(antType);
      currentConfiguration_[i].ant_.antNumber_ = antNumber;
      currentConfFlag_ = UNKNOWN; // This can no longer be a standard configuration
      return;
    }
  }

  // Else get the pad information, and add it to the array

  PadLocation pad = getPadByNumber(padNumber);
  pad.ant_ = getAntennaInfo(antType);
  pad.ant_.antNumber_ = antNumber;

  currentConfiguration_.push_back(pad);

  return;  
}

void CarmaConfig::removePad(std::vector<CarmaConfig::PadLocation>& pads)
{
  for(unsigned i=0; i < pads.size(); i++) {
    removePad(pads[i].padNumber_);
  }
}

void CarmaConfig::removePad(unsigned padNumber)
{
  // Search the current configuration for the specified pad

  for(std::vector<PadLocation>::iterator pad=currentConfiguration_.begin(); 
      pad != currentConfiguration_.end(); pad++) {

    if(pad->padNumber_ == padNumber) {
      currentConfiguration_.erase(pad);      
      currentConfFlag_ = UNKNOWN; // This can no longer be a standard
				  // configuration
      break;
    }

  }
}

std::vector<CarmaConfig::PadLocation> CarmaConfig::getCurrentConfiguration()
{
  return currentConfiguration_;
}

std::vector<CarmaConfig::PadLocation> CarmaConfig::getAllPads()
{
  return padLocations_;
}

/**.......................................................................
 * Return true if the antenna on this pad is shadowed by the passed
 * pad, with the antenna in the current pointing position
 */
std::string CarmaConfig::PadLocation::listConfigs()
{
  bool prev = false;
  std::ostringstream os;

  addConf(os, prev, flags_, A);
  addConf(os, prev, flags_, B);
  addConf(os, prev, flags_, BO);
  addConf(os, prev, flags_, C);
  addConf(os, prev, flags_, CO);
  addConf(os, prev, flags_, D);
  addConf(os, prev, flags_, DO);
  addConf(os, prev, flags_, E);
  addConf(os, prev, flags_, I);
  addConf(os, prev, flags_, L);
  addConf(os, prev, flags_, H);

  return os.str();
}

void CarmaConfig::PadLocation::
addConf(std::ostringstream& os, bool& prev, unsigned flags, unsigned conf)
{
  if(flags & conf) {
    if(prev) {
      os << ", " << confFlagToName(conf);
    } else {
      os << confFlagToName(conf);
    }

    prev = true;
  }
}

CarmaConfig::PadLocation CarmaConfig::findNearest(Length east, Length north)
{
  double dx = east.meters()  - currentConfiguration_[0].east_.meters();
  double dy = north.meters() - currentConfiguration_[0].north_.meters();
  double distMin2 = dx*dx + dy*dy;
  double dist2;
  
  PadLocation padMin = currentConfiguration_[0];

  for(unsigned i=0; i < currentConfiguration_.size(); i++) {
    dx = east.meters()  - currentConfiguration_[i].east_.meters();
    dy = north.meters() - currentConfiguration_[i].north_.meters();
    dist2 = dx*dx + dy*dy;

    if(dist2 < distMin2) {
      distMin2 = dist2;
      padMin = currentConfiguration_[i];
    }
  }

  return padMin;
}

CarmaConfig::CarmaConfig(const CarmaConfig& conf)
{
  *this = (CarmaConfig&) conf;
}

CarmaConfig::CarmaConfig(CarmaConfig& conf)
{
  *this = conf;
}

void CarmaConfig::operator=(const CarmaConfig& conf)
{
  *this = (CarmaConfig&)conf;
}

void CarmaConfig::operator=(CarmaConfig& conf)
{
  currentConfFlag_      = conf.currentConfFlag_;
  padLabels_            = conf.padLabels_;
  padLocations_         = conf.padLocations_;
  currentConfiguration_ = conf.currentConfiguration_;
}

