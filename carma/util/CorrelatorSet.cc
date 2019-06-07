#include "carma/util/CorrelatorSet.h"
using namespace std;

using namespace carma;
using namespace carma::util;

/**.......................................................................
 * Constructor.
 */
CorrelatorSet::CorrelatorSet() 
{
  initialize(carma::util::CORR_NONE);
}

CorrelatorSet::CorrelatorSet(util::CorrelatorType corr)
{
  *this = corr;
}

/**.......................................................................
 * Destructor.
 */
CorrelatorSet::~CorrelatorSet() {}

/**.......................................................................
 * Initialize this correlator set
 */
void CorrelatorSet::initialize(util::CorrelatorType corr)
{
  corrSet_ = corr;
}

CorrelatorType CorrelatorSet::corrType() const  { return  corrSet_; }

/**.......................................................................
 * Return a string representation of this correlator set
 */
std::string CorrelatorSet::corrTypeString() const
{
  std::ostringstream os;

  bool first=true;

  if(corrSet_ == carma::util::CORR_NONE)
    return "CORR_NONE";

  if(corrSet_ & carma::util::CORR_SPECTRAL) {
    os << "CORR_SPECTRAL";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_WIDEBAND) {
    os << (first ? "" : "+") << "CORR_WIDEBAND";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_C3GMAX8) {
    os << (first ? "" : "+") << "CORR_C3GMAX8";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_C3GMAX23) {
    os << (first ? "" : "+") << "CORR_C3GMAX23";
    first = false;
  }

  return os.str();
}

/**.......................................................................
 * Return a string representation of this correlator set
 */
std::string CorrelatorSet::mpString() const
{
  std::ostringstream os;

  bool first=true;

  if(corrSet_ == carma::util::CORR_NONE)
    return "NONE";

  if(corrSet_ & carma::util::CORR_SPECTRAL) {
    os << "SPECTRAL";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_WIDEBAND) {
    os << (first ? "" : "+") << "WIDEBAND";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_C3GMAX8) {
    os << (first ? "" : "+") << "C3GMAX8";
    first = false;
  }

  if(corrSet_ & carma::util::CORR_C3GMAX23) {
    os << (first ? "" : "+") << "C3GMAX23";
    first = false;
  }

  return os.str();
}

/**.......................................................................
 * Return true if this set represents a single correlator
 */
bool CorrelatorSet::isSingleCorrelator() const
{
  return nCorrelator() == 1;
}

/**.......................................................................
 * Return the number of correlators contained in this set
 */
unsigned CorrelatorSet::nCorrelator() const
{
  unsigned nCorr=0;

  if(includes(carma::util::CORR_SPECTRAL))
    ++nCorr;

  if(includes(carma::util::CORR_WIDEBAND))
    ++nCorr;

  if(includes(carma::util::CORR_C3GMAX8))
    ++nCorr;

  if(includes(carma::util::CORR_C3GMAX23))
    ++nCorr;

  return nCorr;
}

/**.......................................................................
 * Return true if this set is empty
 */
bool CorrelatorSet::isEmpty() const
{
  return corrSet_ == carma::util::CORR_NONE;
}

/**.......................................................................
 * Return true if this set contains all correlators
 */
bool CorrelatorSet::isAll() const
{
  return corrSet_ == carma::util::CORR_ALL;
}

/**.......................................................................
 * Return true if this set represents any of the following
 * single correlators
 */
bool CorrelatorSet::isSpectral() const
{
  return corrSet_ == carma::util::CORR_SPECTRAL;
}

bool CorrelatorSet::isWideband() const
{
  return corrSet_ == carma::util::CORR_WIDEBAND;
}

bool CorrelatorSet::isC3gMax8() const
{
  return corrSet_ == carma::util::CORR_C3GMAX8;
}

bool CorrelatorSet::isC3gMax23() const
{
  return corrSet_ == carma::util::CORR_C3GMAX23;
}

/**.......................................................................
 * Return true if this set includes any of the following
 * single correlators
 */
bool CorrelatorSet::includesSpectral() const
{
  return includes(carma::util::CORR_SPECTRAL);
}

bool CorrelatorSet::includesWideband() const
{
  return includes(carma::util::CORR_WIDEBAND);
}

bool CorrelatorSet::includesC3gMax8() const
{
  return includes(carma::util::CORR_C3GMAX8);
}

bool CorrelatorSet::includesC3gMax23() const
{
  return includes(carma::util::CORR_C3GMAX23);
}

void
CorrelatorSet::addSpectral() 
{
  addCorrelator(carma::util::CORR_SPECTRAL);
}

void CorrelatorSet::addWideband()
{
  return addCorrelator(carma::util::CORR_WIDEBAND);
}

void
CorrelatorSet::addC3gMax8() 
{
  return addCorrelator(carma::util::CORR_C3GMAX8);
}

void
CorrelatorSet::addC3gMax23()
{
  return addCorrelator(carma::util::CORR_C3GMAX23);
}

/**.......................................................................
 * Return true if the passed correlator is the first one (in
 * bit order) in the set
 */
bool CorrelatorSet::firstCorrelatorIs(util::CorrelatorType corr) const
{
  if(includes(carma::util::CORR_SPECTRAL) && corr==carma::util::CORR_SPECTRAL)
    return true;

  if(includes(carma::util::CORR_WIDEBAND) && corr==carma::util::CORR_WIDEBAND)
    return true;

  if(includes(carma::util::CORR_C3GMAX8) && corr==carma::util::CORR_C3GMAX8)
    return true;

  if(includes(carma::util::CORR_C3GMAX23) && corr==carma::util::CORR_C3GMAX23)
    return true;

  return false;
}

/**.......................................................................
 * Return true if this set includes the specified correlator set
 */
bool CorrelatorSet::includes(const util::CorrelatorType corr) const
{

  // No set includes CORR_NONE, unless our set is also CORR_NONE

  if(corr == carma::util::CORR_NONE) {
    return isEmpty();
  } else {
    return (corrSet_ & corr) == corr;
  }

}

/**.......................................................................
 * Return true if this set includes the specified correlator set
 */
bool CorrelatorSet::includes(const CorrelatorSet& set) const
{
  return includes(set.corrSet_);
}

/**.......................................................................
 * Add a correlator to this set
 */
void CorrelatorSet::addCorrelator(util::CorrelatorType corr)
{
  corrSet_ |= corr;
}

/**.......................................................................
 * Remove a correlator from this set
 */
void CorrelatorSet::removeCorrelator(util::CorrelatorType corr)
{
  corrSet_ &= ~corr;
}

void CorrelatorSet::operator=(util::CorrelatorType corr)
{
  initialize(corr);
}

bool CorrelatorSet::operator==(util::CorrelatorType corr)
{
  return (corrSet_ & corr) == corr;
}

bool CorrelatorSet::operator==( const CorrelatorSet & corrSet ) const
{
  return (corrSet_ == corrSet.corrSet_ );
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
carma::util::operator<<(ostream& os, const CorrelatorSet& set)
{
  os << set.corrTypeString();
  return os;
}

/**.......................................................................
 * Return vector of correlator Designations that this set includes
 */
std::vector<carma::util::CorrelatorType> 
CorrelatorSet::getControlCorrelatorDesignations() const
{
  std::vector<util::CorrelatorType> corrs;

  if(includesSpectral())
    corrs.push_back(carma::util::CORR_SPECTRAL);

  if(includesWideband())
    corrs.push_back(carma::util::CORR_WIDEBAND);

  if(includesC3gMax8())
    corrs.push_back(carma::util::CORR_C3GMAX8);

  if(includesC3gMax23())
    corrs.push_back(carma::util::CORR_C3GMAX23);

  return corrs;
}

/**.......................................................................
 * Return vector of correlator Designations that this set includes
 */
util::CorrelatorType CorrelatorSet::getFirstControlCorrelatorDesignation() const
{
  if(firstCorrelatorIs(carma::util::CORR_SPECTRAL))
    return carma::util::CORR_SPECTRAL;

  if(firstCorrelatorIs(carma::util::CORR_WIDEBAND))
    return carma::util::CORR_WIDEBAND;

  if(firstCorrelatorIs(carma::util::CORR_C3GMAX8))
    return carma::util::CORR_C3GMAX8;

  if(firstCorrelatorIs(carma::util::CORR_C3GMAX23))
    return carma::util::CORR_C3GMAX23;

  return carma::util::CORR_NONE;
}

util::CorrelatorType CorrelatorSet::getControlCorrelatorDesignation() const
{
  return corrSet_;
}

bool CorrelatorSet::isC3g() const { return ( isC3gMax8() || isC3gMax23() ); }
