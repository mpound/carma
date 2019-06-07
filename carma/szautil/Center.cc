#include "carma/szautil/Center.h"

using namespace std;

using namespace sza::array;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Center::Center(sza::array::SourceId srcId,
			       AntNum::Id antennas) :
  srcId_(srcId), antennas_(antennas) 
{
  for(unsigned i=0; i < 3; i++)
    ephem_[i] = 0.0;

  init_CacheWindow(&window_);

  source_.reset();
}

/**.......................................................................
 * Destructor.
 */
Center::~Center() {}

/**.......................................................................
 * Add antennas from the set associated with this center.
 */
void Center::addAntennas(AntNum::Id antennas)
{
  unsigned lmask = static_cast<unsigned>(antennas_);
  unsigned rmask = static_cast<unsigned>(antennas);
  antennas_ = static_cast<AntNum::Id>(lmask | rmask);
}

/**.......................................................................
 * Remove antennas from the set associated with this center.
 */
void Center::removeAntennas(AntNum::Id antennas)
{
  unsigned lmask = static_cast<unsigned>(antennas_);
  unsigned rmask = static_cast<unsigned>(antennas);
  antennas_ = static_cast<AntNum::Id>(lmask & ~rmask);
}

/**.......................................................................
 * Remove antennas from the set associated with this center.
 */
bool Center::isEmpty()
{
  return antennas_ == AntNum::ANTNONE;
}

/**.......................................................................
 * Return the name of this pointing center
 */
std::string  Center::getName()
{
  return srcId_.name;
}

/**.......................................................................
 * Return the catalog number of this pointing center
 */
unsigned Center::getCatalogNumber()
{
  return srcId_.number;
}

/**.......................................................................
 * Return the type of this pointing center
 */
sza::array::SourceType Center::getType()
{
  return srcId_.type;
}

/**.......................................................................
 * Return the set of antennas associated with this pointing
 * center.
 */
AntNum::Id Center::getAntennas()
{
  return antennas_;
}

/**.......................................................................
 * Return true if a set of antnenas are asociated with this
 * pointing center.
 */
bool Center::isSet(AntNum::Id antennas)
{
  AntNum antSet(antennas_);

  return antSet.isSet(antennas);
}

/**.......................................................................
 * Return a pointer to the ephemeric cache of this source, where
 * appropriate
 */
double* Center::getEphemerisCache() 
{ 
  return ephem_;
}

/** .......................................................................
 * Return a pointer to the window over which the last cached
 * values are valid.
 */
struct CacheWindow* Center::getWindow()
{
  return &window_;
}

/**.......................................................................
 * Return a pointer to this object's source id
 */
sza::array::SourceId* Center::getSourceId() 
{
  return &srcId_;
}

/**.......................................................................
 * Return a pointer to this object's source
 */
sza::util::Source* Center::getSource() 
{
  return &source_;
}
