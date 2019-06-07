#include "carma/util/AstroBand.h"

using namespace std;

using namespace carma::util;

const unsigned AstroBand::nBandMax_ = 40;

/**.......................................................................
 * Constructor.
 */
AstroBand::AstroBand() 
{
  bandNo_ = 0;
}

AstroBand::AstroBand(unsigned bandNo) 
{
  bandNo_ = bandNo;
}

AstroBand::AstroBand(const AstroBand& ab)
{
  *this = ab;
}

void AstroBand::operator=(const AstroBand& ab)
{
  *this = (AstroBand&)ab;
}

void AstroBand::operator=(AstroBand& ab)
{
  bandNo_ = ab.bandNo_;
}

/**.......................................................................
 * Destructor.
 */
AstroBand::~AstroBand() {}

void AstroBand::setTo(unsigned astroBandNo)
{
  bandNo_ = astroBandNo;
}
