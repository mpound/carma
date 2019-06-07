#include "carma/szautil/Coordinates.h"
#include "carma/szautil/PtSrcTiler.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
PtSrcTiler::PtSrcTiler() {}

/**.......................................................................
 * Destructor.
 */
PtSrcTiler::~PtSrcTiler() {}

/**.......................................................................
 * Construct the list of fields to search
 */
std::vector<PtSrcTiler::Field> PtSrcTiler::constructFields(HourAngle ra,      Declination dec, 
							   Angle fieldRadius, Angle totalRadius)
{
  // Start off by initializing the vector with the center field

  std::vector<Field> fields;

#if 1
  // Calculate the minimum number of hex layers needed for complete
  // coverage out to the total radius

  double c30 = cos(30.0/180 * M_PI);
  Angle fieldSeparation = Angle(Angle::Radians(), fieldRadius.radians()*2*c30);
  
  // Radius out to completion for n layers (single field -> n=0):
  //             
  //   Rc = [{n * sep} y] + [{r} x + {sep} y]
  //
  // So:
  //
  //   Rc >= R ==> Rc^2 >= R^2 ==> (n*sep + sep)^2 + r^2 >= R^2
  // 
  //        | sqrt(R^2 - r^2) |
  //   n >= | --------------- | - 1
  //        |       sep       |
  //
  //
  double tr = totalRadius.radians();
  double fr = fieldRadius.radians();
  double sr = fieldSeparation.radians();

  // If the total radius is less than the field radius, just search
  // the total radius

  if(totalRadius <= fieldRadius) {
    fields.push_back(Field(ra, dec, totalRadius));
    return fields;
  }

  COUT("fieldRad = " << fieldRadius);
  COUT("totalRad = " << totalRadius);

  unsigned nLayer=0;
  nLayer = unsigned(ceil(sqrt(fabs(tr*tr - fr*fr))/sr));
  
  COUT("n = " << nLayer);
  
  unsigned iStart = 0;
  unsigned iStop  = 1;

  for(unsigned iLayer=0; iLayer <= nLayer; iLayer++) {
    addLayer(iLayer, fieldRadius, ra, dec, fields);
  }
#endif

  return fields;
}

void PtSrcTiler::addLayer(unsigned iLayer, Angle& fieldRad, HourAngle& ra0, Declination& dec0, 
			  std::vector<Field>& fields)
{ 
#if 1
  // The number of fields per side

  double dAngDeg    = iLayer == 0 ? 0.0 : 60.0/iLayer;
  unsigned nPerSide = iLayer == 0 ? 1 : iLayer;
  unsigned nSide    = iLayer == 0 ? 1 : 6;

  COUT("nPerSide is: " << nPerSide);
  COUT("nSide is:    " << nSide);

  Angle fieldSep = Angle(Angle::Radians(), fieldRad.radians() * 2 * cos(30.0/180 * M_PI));
  Angle theta;
  Angle rho;

  // We will iterate around vertices, starting at theta = 0

  HourAngle ra;
  Declination dec;

  for(unsigned iSide=0; iSide < nSide; iSide++) {

    for(unsigned iFld=0; iFld < nPerSide; iFld++) {

      // Get the coordinates of the starting field for this side.  This
      // will be the vertex field
      
      theta.setDegrees(iSide * 60.0 + iFld * dAngDeg);

      rho.setDegrees((iLayer * fieldSep.degrees() * cos(30.0/180*M_PI))/
		     cos((30.0 - (iFld) * dAngDeg)/180 * M_PI));

      Coordinates::raDecAndThetaRhoToRaDec(ra0, dec0, theta, rho, ra, dec);

      fields.push_back(Field(ra, dec, fieldRad));
    }
  }
#endif
}

/**.......................................................................
 * For the current field, add all nearest neighbors in a hex pattern
 */
std::vector<PtSrcTiler::Field> PtSrcTiler::Field::addNeighbors(std::vector<Field>& fields, 
							       HourAngle& ra0, Declination& dec0,
							       Angle& min)
{
  std::vector<Field> newFields;
  bool first=true;
  Angle rho, theta;
  rho.setRadians(2*radius_.radians() * cos(30.0/180 * M_PI));

  // Iterate over all possible nearest neighbors, some of which may
  // already exist

  HourAngle ra;
  Declination dec;

  for(unsigned i=0; i < 6; i++) {

    // If this neighbor doesn't already exist

    if(neighbors_[i] == 0) {

      theta.setDegrees(i*60.0);
      Coordinates::raDecAndThetaRhoToRaDec(ra_, dec_, theta, rho, ra, dec);
      Field neighbor(ra, dec, radius_);

      // Register ourselves as a neighbor of the field we just created

      neighbor.addNeighbor(i, this);
      newFields.push_back(neighbor);

      // And add it as a neighbor of ours
 
      neighbors_[i] = &fields[fields.size()-1];

      Angle sep = neighbor.distance(ra0, dec0);

      if(first) {
	min = sep;
	first = false;
      }

      if(sep < min)
	min = sep;
    }

  }

  return newFields;
}

PtSrcTiler::Field::Field()
{
  neighbors_.resize(6);
  for(unsigned i=0; i < 6; i++) {
    neighbors_[i] = 0;
  }
}

PtSrcTiler::Field::Field(HourAngle ra, Declination dec, Angle radius)
{
  ra_     = ra;
  dec_    = dec;
  radius_ = radius;

  neighbors_.resize(6);
  for(unsigned i=0; i < 6; i++) {
    neighbors_[i] = 0;
  }
}

PtSrcTiler::Field::Field(const Field& fld)
{
  ra_     = fld.ra_;
  dec_    = fld.dec_;
  radius_ = fld.radius_;

  neighbors_.resize(6);
  for(unsigned i=0; i < 6; i++) {
    neighbors_[i] = fld.neighbors_[i];
  }
}

/**.......................................................................
 * Allow a field to add itself as our neighbor.  
 */
void PtSrcTiler::Field::addNeighbor(unsigned index, Field* fld)
{
  index = (index+3)%5;
  neighbors_[index] = fld;
}

/**.......................................................................
 * Return the distance of this field from the given ra/dec
 */
Angle PtSrcTiler::Field::distance(HourAngle ra, Declination dec)
{
  Vector<double> v1(3);
  Vector<double> v2(3);

  v1[0] = cos(dec_.radians()) * cos(ra_.radians());
  v1[1] = cos(dec_.radians()) * sin(ra_.radians());
  v1[2] = sin(dec_.radians());

  v2[0] = cos(dec.radians()) * cos(ra.radians());
  v2[1] = cos(dec.radians()) * sin(ra.radians());
  v2[2] = sin(dec.radians());

  Angle dist;
  dist.setRadians(acos(v1*v2));

  return dist;
}
