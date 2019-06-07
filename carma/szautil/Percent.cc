#include "carma/szautil/Percent.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Percent::Percent() 
{
  percentMax1_ = 0.0;
}

Percent::Percent(const Max1& unit, double percent) 
{
  setPercentMax1(percent);
}

Percent::Percent(const Max100& unit, double percent) 
{
  setPercentMax100(percent);
}

/**.......................................................................
 * Destructor.
 */
Percent::~Percent() {}

void Percent::setPercentMax1(double percent)
{
  percentMax1_ = percent;
}

void Percent::setPercentMax100(double percent)
{
  percentMax1_ = percent / 100;
}

double Percent::percentMax1()
{
  return percentMax1_;
}

double Percent::percentMax100()
{
  return percentMax1_ * 100;
}


