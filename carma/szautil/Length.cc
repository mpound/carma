#include "carma/szautil/Length.h"
#include "carma/szautil/Frequency.h"

#include <iomanip>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Length::Length() 
{
  initialize();
}

/**.......................................................................
 * Copy constructor.
 */
Length::Length(const Length& length) 
{
  setCentimeters(length.centimeters());
}

Length::Length(const Centimeters& units, double cm)
{
  setCentimeters(cm);
}

Length::Length(const Meters& units, double m)
{
  setMeters(m);
}

Length::Length(const Kilometers& units, double km)
{
  setKilometers(km);
}

/**.......................................................................
 * Destructor.
 */
Length::~Length() {}

/** .......................................................................
 * Add two Lengths
 */
Length Length::operator+(Length& length)
{
  Length sum;
  sum.setCentimeters(cm_ + length.centimeters());
  return sum;
}

void Length::operator+=(const Length& length)
{
  return operator+=((Length&) length);
}

void Length::operator+=(Length& length)
{
  cm_ += length.cm_;
}

void Length::operator-=(const Length& length)
{
  return operator-=((Length&) length);
}

void Length::operator-=(Length& length)
{
  COUT("Sibtracting " << length.cm_ << " from " << cm_);
  cm_ -= length.cm_;
}

/** .......................................................................
 * Add two Lengths
 */
Length Length::operator+(const Length& length)
{
  Length sum;
  sum.setCentimeters(cm_ + length.centimeters());
  return sum;
}

/** .......................................................................
 * Subtract two Lengths
 */
Length Length::operator-(Length& length)
{
  Length diff;
  diff.setCentimeters(cm_ - length.centimeters());
  return diff;
}

/** .......................................................................
 * Subtract two Lengths
 */
Length Length::operator-(const Length& length)
{
  return operator-((Length&) length);
}

/** .......................................................................
 * Divide a length by a constant
 */
Length Length::operator/(double fac)
{
  Length div;
  div.setCentimeters(cm_ / fac);
  return div;
}

/** .......................................................................
 * Divide a length by a constant
 */
void Length::operator/=(double fac)
{
  setCentimeters(cm_ / fac);
}

/** .......................................................................
 * Divide two Lengths
 */
double Length::operator/(const Length& length)
{
  return operator/((Length&) length);
}

double Length::operator/(Length& length)
{
  return cm_/length.centimeters();
}

/** .......................................................................
 * Multiply a length by a constant
 */
Length Length::operator*(double multFac)
{
  Length mult;
  mult.setCentimeters(cm_ * multFac);
  return mult;
}

/** .......................................................................
 * Multiply a length by a constant
 */
void Length::operator*=(double multFac)
{
  setCentimeters(cm_ * multFac);
}

/**.......................................................................
 * Allows cout << Length
 */
std::ostream& 
sza::util::operator<<(std::ostream& os, Length& length)
{
  os << setw(14) << setprecision(8) << length.meters() << " (m)";
  return os;
}

/**.......................................................................
 * Allows cout << Length
 */
std::ostream& 
sza::util::operator<<(std::ostream& os, const Length& length)
{
  return operator<<(os, (Length&) length);
}

void Length::initialize()
{
  setCentimeters(0.0);
}

// Return the length as a light travel time

Time Length::time()
{
  return Time(Time::Seconds(), cm_/Frequency::lightSpeed_.centimetersPerSec());
}

void Length::setCentimeters(double cm) 
{
  cm_ = cm;
  finite_ = finite(cm);
}

Vector<Length> sza::util::operator*(Matrix<double>& mat, Vector<Length>& vec)
{
  LogStream errStr;
  
  if(mat.nCol_==0 || vec.size()==0) {
    ThrowError("Zero dimension encountered");
  }
	
  if(mat.nCol_ != vec.size()) {
    ThrowError("Vector has incompatible dimensions");
  }
	
  // Now do the calculation
	
  Vector<Length> result;
	
  result.resize(mat.nRow_);
	
  bool first = true;
	
  for(unsigned iRow=0; iRow < mat.nRow_; iRow++)
    for(unsigned j=0; j < mat.nCol_; j++) {
      if(first) {
	result[iRow]  = vec[j] * mat.data_[iRow][j];
	first = false;
      } else {
	result[iRow] += vec[j] * mat.data_[iRow][j];
      }
    }
	
  return result;
}

Vector<Length> sza::util::operator*(Vector<double>& inpVec, Length& fac)
{
  Vector<Length> retVec(inpVec.size());

  for(unsigned i=0; i < inpVec.size(); i++) {
    retVec[i].setMeters(inpVec[i] * fac.meters());
  }

  return retVec;
}

bool Length::operator==(const Length& length) 
{
  return operator==((Length&) length);
}

bool Length::operator==(Length& length) 
{
  return fabs(length.microns() - microns()) < 1e-6;
}
