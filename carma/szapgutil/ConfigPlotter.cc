#include "carma/szapgutil/ConfigPlotter.h"
#include "carma/szapgutil/PgUtil.h"

#include "pgplot/cpgplot.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Return true if the passed rectangle overlaps with this rectangle
 */
bool ConfigPlotter::Rectangle::overlaps(ConfigPlotter::Rectangle& r)
{
  if(xLeft()   > r.xRight()  || 
     xRight()  < r.xLeft()   || 
     yTop()    < r.yBottom() || 
     yBottom() > r.yTop())
    return false;

  return true;
};

/**.......................................................................
 * Return true if the passed circle overlaps with this rectangle
 */
bool ConfigPlotter::Rectangle::overlaps(ConfigPlotter::Circle& c)
{
  double dx = x_ - c.x_;
  double dy = y_ - c.y_;
  double theta = atan2(dy, dx) * 180/M_PI;

  // atan2 return between -pi and +pi

  if(theta < 0.0)
    theta += 360.0;

  // As defined theta =   0 at y =   0, x = +ve
  //            theta =  90 at y = +ve, x =   0
  //            theta = 180 at y =   0, x = -ve
  //            theta = 270 at y = -ve, x =   0

  double xBl  = x_ - xWidth_/2;
  double yBl  = y_ - yWidth_/2;
  double dxBl = (xBl - c.x_);
  double dyBl = (yBl - c.y_);
  double rBl  = sqrt(dxBl * dxBl + dyBl * dyBl);

  double xTl  = x_ - xWidth_/2;
  double yTl  = y_ + yWidth_/2;
  double dxTl = (xTl - c.x_);
  double dyTl = (yTl - c.y_);
  double rTl  = sqrt(dxTl * dxTl + dyTl * dyTl);

  double xBr  = x_ + xWidth_/2;
  double yBr  = y_ - yWidth_/2;
  double dxBr = (xBr - c.x_);
  double dyBr = (yBr - c.y_);
  double rBr  = sqrt(dxBr * dxBr + dyBr * dyBr);

  double xTr  = x_ + xWidth_/2;
  double yTr  = y_ + yWidth_/2;
  double dxTr = (xTr - c.x_);
  double dyTr = (yTr - c.y_);
  double rTr  = sqrt(dxTr * dxTr + dyTr * dyTr);

  double rB =  (y_ -  yWidth_/2) - c.y_;
  double rT = c.y_ - (y_ + yWidth_/2);
  double rR =  (x_ -  xWidth_/2) - c.x_;
  double rL = c.x_ - (x_ + xWidth_/2);

  if(theta == 0.0) {
    return rL < c.r_;
  } else if(theta > 0.0 && theta < 90.0) {
    return rBl < c.r_;
  } else if(theta == 90.0) {
    return rB < c.r_;
  } else if(theta > 90.0 && theta < 180.0) {
    return rBr < c.r_;
  } else if(theta == 180.0) {
    return rR < c.r_;
  } else if(theta > 180.0 && theta < 270.0) {
    return rTr < c.r_;
  } else if(theta == 270.0) {
    return rT < c.r_;
  } else if(theta > 270.0 && theta < 360.0) {
    return rTl < c.r_;
  } 
};

/**.......................................................................
 * Constructor.
 */
ConfigPlotter::ConfigPlotter() 
{
  pgplotId_     = 0;
  fixedLims_    = false;
  useMarks_     = false;
  useLabels_    = true;
  overplot_     = false;
  useFillFlags_ = false;
  fillStyle_    = FILL_OUTLINE;
}

void ConfigPlotter::openDevice(std::string device)
{
  if(pgplotId_ <= 0)
    pgplotId_ = cpgopen(device.c_str());

  if(pgplotId_ <= 0)
    ThrowError("Unable to open pgplot device: " << device);
}

/**.......................................................................
 * Destructor.
 */
ConfigPlotter::~ConfigPlotter() {}

/**.......................................................................
 * Plot configuration
 */
void ConfigPlotter::
plotConfiguration(std::vector<CarmaConfig::PadLocation>& pads)
{
  std::vector<Rectangle> labels;

  if(pgplotId_ <= 0)
    ThrowError("No plot device");
  
  cpgslct(pgplotId_);
  cpgask(0);

  if(!overplot_) {

    cpgpage();

    // Get the current plot limits

    getPlotLimits(pads, currXmin_, currXmax_, currYmin_, currYmax_, 0.2);

    if(PgUtil::usedefs_) {
      currXmin_ = PgUtil::xmin_;
      currXmax_ = PgUtil::xmax_;
      currYmin_ = PgUtil::ymin_;
      currYmax_ = PgUtil::ymax_;
    }

  }

  cpgsci(1);
  cpgsch(1.0);
  cpgslw(1);
  
  cpgsvp(0.1, 0.9, 0.1, 0.9);

  cpgsfs(2);

  cpgswin(currXmin_, currXmax_, currYmin_, currYmax_);
  cpgwnad(currXmin_, currXmax_, currYmin_, currYmax_);

  if(!overplot_) {
    cpgbox("BCNST",0,0,"BCNST",0,0);
  }

  float yoff = 1.0/40 * (currYmax_ - currYmin_);

  std::ostringstream os;

  float ch;
  cpgqch(&ch);

  cpgsch(0.4);

  // Iterate over pads

  unsigned nMark = xMarks_.size();

  std::vector<CarmaConfig::PadLocation*> nearbyPads(nMark);
  std::vector<double> distNear(nMark);

  double xCurr, yCurr, dist;

  //------------------------------------------------------------
  // Write labels by the antennas
  //------------------------------------------------------------

  for(unsigned i=0; i < pads.size(); i++) {

    xCurr = pads[i].east_.meters();
    yCurr = pads[i].north_.meters();

    cpgsci(getColor(pads[i]));

    // If there is no antenna on this pad, just plot the pad

    if(pads[i].ant_.antFlag_ == CarmaConfig::UNKNOWN) {

      float xpts[4];
      float ypts[4];

      float padWidth = 3;

      xpts[0] = xCurr - padWidth/2;
      xpts[1] = xCurr + padWidth/2;
      xpts[2] = xCurr + padWidth/2;
      xpts[3] = xCurr - padWidth/2;

      ypts[0] = yCurr + padWidth/2;
      ypts[1] = yCurr + padWidth/2;
      ypts[2] = yCurr - padWidth/2;
      ypts[3] = yCurr - padWidth/2;

      cpgpoly(4, xpts, ypts);

      // Else plot the antenna

    } else {

      if(useFillFlags_ && fillFlags_[i]) {
	cpgshs(45.0, 0.3, 0.0);
	cpgsfs(fillStyle_);
      } else {
	cpgsfs(2);
      }

      int ls;
      cpgqls(&ls);
      cpgsls(getLineStyle(pads[i]));
      cpgcirc(xCurr, yCurr, pads[i].ant_.diameter_.meters()/2); 
      cpgsls(ls);

      if(az_.size() > i) {
	double r = pads[i].ant_.diameter_.meters()/2;
	int ci;
	cpgqci(&ci);
	cpgsci(8);
	cpgarro(xCurr, yCurr, xCurr + r*sin(az_[i].radians()), yCurr + r*cos(az_[i].radians()));
	cpgsci(ci);
      }

      if(el_.size() > i) {
	double r = pads[i].ant_.diameter_.meters()/2;
	int ci;
	cpgqci(&ci);
	cpgsci(10);
	cpgarro(xCurr, yCurr, xCurr + r*cos(el_[i].radians()), yCurr + r*sin(el_[i].radians()));
	cpgsci(ci);
      }
    }

    os.str("");

    if(pads[i].ant_.antNumber_ >= 0) {
      os << "C" << pads[i].ant_.antNumber_ << ", ";
    }
    
    os << "P" << pads[i].padNumber_;

    // Write labels only if the text isn't huge compared to the
    // antenna sizes

    unsigned nchar = os.str().length();
    double strSize = nchar * 1.0/40 * ch;
    double antSize = pads[i].ant_.diameter_.meters() / (currYmax_ - currYmin_);

    if(antSize > strSize || true) {

      double x, y, xNear, yNear;
      getLabelLocation(pads[i], pads, os.str(), x, y, xNear, yNear, labels, 180.0);

      //------------------------------------------------------------
      // If requested to print labels, do it now
      //------------------------------------------------------------

      if(useLabels_) {

	// Only write text if it is inside the current plot window
	
	if(x > currXmin_ && x < currXmax_ && y > currYmin_ && y < currYmax_) {
	  cpgptxt(x, y, 0.0, 0.5, os.str().c_str());
	}
      }

    }

    //------------------------------------------------------------
    // Iterate over marked locations
    //------------------------------------------------------------

    if(useMarks_) {

      for(unsigned iMark=0; iMark < nMark; iMark++) {

	// Check if this point is nearer to the mark than any previous
	// antenna
	
	dist = sqrt((xCurr - xMarks_[iMark]) * (xCurr - xMarks_[iMark]) + 
		    (yCurr - yMarks_[iMark]) * (yCurr - yMarks_[iMark]));
	
	if(i == 0 || dist < distNear[iMark]) {
	  distNear[iMark]   = dist;
	  nearbyPads[iMark] = &pads[i];
	}

      } // Done iterating over marked locations
    }

  } // Done iterating over pads

  // Set the character height back to what it was

  cpgsch(ch);

  // If displaying marks, mark the pad nearest to the user-selection
  
  if(useMarks_) {
    
    cpgsfs(1);
    
    for(unsigned iMark=0; iMark < nMark; iMark++) {
      cpgsci(getColor(*nearbyPads[iMark]));
      cpgcirc(nearbyPads[iMark]->east_.meters(), 
	      nearbyPads[iMark]->north_.meters(),
	      nearbyPads[iMark]->ant_.diameter_.meters()/2); 
    }
    
    cpgsfs(2);
  }
  
  cpgsci(1);
  
  cpglab("E(m)", "N(m)", "");
}

void ConfigPlotter::getPlotLimits(std::vector<CarmaConfig::PadLocation>& pads, 
				  double& xmin, double& xmax, 
				  double& ymin, double& ymax, double frac)
{
  double eps = 1e-6;

  // If using fixed limits, just set them and return

  if(fixedLims_) {
    xmin = fixXmin_;
    xmax = fixXmax_;
    ymin = fixYmin_;
    ymax = fixYmax_;

    // But check that they yield a valid range

    if(!(xmax > xmin)) {
      xmax += eps;
      xmin -= eps;
    }

    if(!(ymax > ymin)) {
      ymax += eps;
      ymin -= eps;
    }

    // Else determine the limits from the data themselves

  } else {

    // But make sure the data array isn't empty

    if(pads.size() > 0) {
      xmin = xmax = pads[0].east_.meters();
      ymin = ymax = pads[0].north_.meters();

      double e,n,r,emin, emax, nmin, nmax;
      for(unsigned i=0; i < pads.size(); i++) {

	e = pads[i].east_.meters();
	n = pads[i].north_.meters();

	r = pads[i].ant_.diameter_.meters()/2;

	emin = e - r;
	emax = e + r;

	nmin = n - r;
	nmax = n + r;

	xmin = (emin < xmin) ? emin : xmin;
	xmax = (emax > xmax) ? emax : xmax;
	ymin = (nmin < ymin) ? nmin : ymin;
	ymax = (nmax > ymax) ? nmax : ymax;
      }

      // If it is, just set the limits to eps

    } else {
      xmin = -eps;
      xmax = +eps;
      ymin = -eps;
      ymax = +eps;
    }
  }

  // For plot purposes, widen the limits by 10%

  double xr = xmax - xmin;
  double yr = ymax - ymin;

  xmin -= frac*xr;
  xmax += frac*xr;

  ymin -= frac*yr;
  ymax += frac*yr;
}

int ConfigPlotter::getLineStyle(CarmaConfig::PadLocation& pad)
{
  if(pad.ant_.antFlag_ & (CarmaConfig::SZA | CarmaConfig::BIMA | CarmaConfig::OVRO)) {

    switch (pad.ant_.subarrayNumber_) {
    case 2:
      return 1;
      break;
    case 1:
      return 2;
      break;
    case 3:
      return 3;
      break;
    case 4:
      return 5;
      break;
    default:
      return 4;
      break;
    }

  } else {
    return 1;
  }
}

int ConfigPlotter::getColor(CarmaConfig::PadLocation& pad)
{
  if(pad.ant_.antFlag_ & CarmaConfig::SZA) {
    return pad.ant_.tracking_ ? PgUtil::MAGENTA : PgUtil::RED;
  } else if(pad.ant_.antFlag_ & CarmaConfig::BIMA) {
    return pad.ant_.tracking_ ? PgUtil::YELLOW : PgUtil::RED;
  } else if(pad.ant_.antFlag_ & CarmaConfig::OVRO) {
    return pad.ant_.tracking_ ? PgUtil::PALE_BLUE : PgUtil::RED;
  } else {
    return PgUtil::LIGHT_GRAY;
  }
}

void ConfigPlotter::
setFixedLimits(double xmin, double xmax, double ymin, double ymax) 
{
  fixXmin_ = xmin < xmax ? xmin : xmax;
  fixXmax_ = xmax > xmin ? xmax : xmin;
  fixYmin_ = ymin < ymax ? ymin : ymax;
  fixYmax_ = ymax > ymin ? ymax : ymin;
}

void ConfigPlotter::
useFixedLimits(bool use)
{
  fixedLims_ = use;
}

CarmaConfig::PadLocation 
ConfigPlotter::
mark(std::vector<CarmaConfig::PadLocation>& pads, double x, double y)
{
  // Set up to use marks in subsequent display

  useMarks_ = true;

  // And return the pad location nearest to the mark

  double distNear;
  double xCurr, yCurr, dist;
  sza::util::CarmaConfig::PadLocation* padNear = 0;

  for(unsigned i=0; i < pads.size(); i++) {

    xCurr = pads[i].east_.meters();
    yCurr = pads[i].north_.meters();

    dist = sqrt((xCurr - x) * (xCurr - x) + 
		(yCurr - y) * (yCurr - y));

    if(i == 0 || dist < distNear) {
      distNear = dist;
      padNear = &pads[i];
    }
  }

  // Add this location to the list of marked locations

  xMarks_.push_back(x);
  yMarks_.push_back(y);

  // And return a pointer to the nearest pad to this locations

  return *padNear;
}

void ConfigPlotter::clearMarks()
{
  useMarks_ = false;

  xMarks_.resize(0);
  yMarks_.resize(0);
}

/**.......................................................................
 * Find a suitable location for this label that doesn't overlap any
 * other antennas
 */
void ConfigPlotter::
getLabelLocation(CarmaConfig::PadLocation& pad, 
		 std::vector<CarmaConfig::PadLocation>& pads, 
		 std::string label, 
		 double& x, double& y, double& xNear, double& yNear,
		 std::vector<Rectangle>& labels, double thetaDeg)
{
  // Get the location, in window coordinates of the current pad

  double xCurr = pad.east_.meters();
  double yCurr = pad.north_.meters();

  float ch;
  cpgqch(&ch);

  unsigned nchar   = label.length();

  // Get the string size in window coordinates

  double strSize         = nchar * 1.0/40 * ch * (currYmax_ - currYmin_);
  double charAspectRatio = 1.0;
  double strHalfWidth    = strSize/2 * charAspectRatio;
  double strHalfHeight   = 1.0/40 * ch/2 * (currYmax_ - currYmin_);
  double antRadius       = (pad.ant_.diameter_.meters())/2;

  // Now iterate around the current antenna, looking for a suitable
  // place to locate this label

  double dTheta=45;
  unsigned nTheta = (unsigned)(360.0/dTheta);
  double xCenter, yCenter;
  double theta;

  bool overlap = false;

  //------------------------------------------------------------
  // If no actual angle was passed, iterate trying to find a position
  // that doesn't overlap
  //------------------------------------------------------------

  if(thetaDeg > 360.0) {
    for(unsigned iTheta=0; iTheta < nTheta; iTheta++) {
      theta = iTheta * dTheta;

      getCoords(theta, xCurr, yCurr, antRadius, strHalfWidth, strHalfHeight,
		xCenter, yCenter, xNear, yNear);

      //-----------------------------------------------------------------------
      // See if the current position overlaps with any plotted antenna
      //-----------------------------------------------------------------------
      
      overlap = false;
      
      for(unsigned iPad=0; iPad < pads.size(); iPad++) {
	CarmaConfig::PadLocation& padCurr = pads[iPad];
	
	Circle c(padCurr.east_.meters(), padCurr.north_.meters(), padCurr.ant_.diameter_.meters()/2);
	Rectangle r(xCenter, yCenter, 2*strHalfWidth, 2*strHalfHeight);
	
	overlap = overlap || r.overlaps(c);
	
	if(overlap) {
	  break;
	}
      }
      
      // If this label already overlaps, continue -- don't even bother
      // checking label overlaps.
      
      if(overlap)
	continue;
      
      for(unsigned iLabel=0; iLabel < labels.size(); iLabel++) {
	
	Rectangle r(xCenter, yCenter, 2*strHalfWidth, 2*strHalfHeight);
	Rectangle& lab = labels[iLabel];	

	overlap = overlap || r.overlaps(lab);
	
	if(overlap) {
	  break;
	}
      }
      
      // If this position doesn't overlap with anything, stop searching
      
      if(!overlap) {
	break;
      }
    }
  } else {
    getCoords(thetaDeg, xCurr, yCurr, antRadius, strHalfWidth, strHalfHeight,
	      xCenter, yCenter, xNear, yNear);

  }

  x = xCenter;
  y = yCenter;

  // If we are exiting the loop, means that we have found a
  // non-overlapping rectangle -- add it to the list of known
  // rectangles

  Rectangle r(xCenter, yCenter, 2*strHalfWidth, 2*strHalfHeight);
  labels.push_back(r);
}


void ConfigPlotter::getCoords(double theta, double xCurr, double yCurr, double antRadius, double strHalfWidth, double strHalfHeight,
			      double& xCenter, double& yCenter, double& xNear, double& yNear)
{
  // If theta == 0, we are directly above the 
  if(theta == 0.0) {
    xCenter = xCurr;
    yCenter = yCurr + antRadius + strHalfHeight;

    xNear   = xCenter;
    yNear   = yCenter;

  } else if(theta > 0.0 && theta < 90.0) {

    xCenter = xCurr + antRadius * cos(theta/180.0 * M_PI) + strHalfWidth;
    yCenter = yCurr + antRadius * sin(theta/180.0 * M_PI);

    xNear   = xCenter - strHalfWidth;
    yNear   = yCenter;

  } else if(theta == 90.0) {

    xCenter = xCurr + antRadius + strHalfWidth;
    yCenter = yCurr - strHalfHeight;

    xNear   = xCenter - strHalfWidth;
    yNear   = yCenter + strHalfHeight;

  } else if(theta > 90.0 && theta < 180.0) {

    xCenter = xCurr - antRadius * cos(theta/180.0 * M_PI) + strHalfWidth;
    yCenter = yCurr - antRadius * sin(theta/180.0 * M_PI) - 2*strHalfHeight;

    xNear   = xCenter - strHalfWidth;
    yNear   = yCenter + strHalfHeight;

  } else if(theta == 180.0) {

    xCenter = xCurr;
    yCenter = yCurr - antRadius - 2*strHalfHeight;

    xNear   = xCenter;
    yNear   = yCenter + strHalfHeight;

  } else if(theta > 180.0 && theta < 270.0) {

    xCenter = xCurr + antRadius * cos(theta/180.0 * M_PI) - strHalfWidth;
    yCenter = yCurr + antRadius * sin(theta/180.0 * M_PI) - 2*strHalfHeight;

    xNear   = xCenter + strHalfWidth;
    yNear   = yCenter + strHalfHeight;

  } else if(theta == 270.0) {

    xCenter = xCurr - antRadius - strHalfWidth;
    yCenter = yCurr - strHalfHeight;

    xNear   = xCenter + strHalfWidth;
    yNear   = yCenter + strHalfHeight;

  } else {
    xCenter = xCurr - antRadius * cos(theta/180.0 * M_PI) - strHalfWidth;
    yCenter = yCurr - antRadius * sin(theta/180.0 * M_PI);

    xNear   = xCenter + strHalfWidth;
    yNear   = yCenter;
  } 
}

void ConfigPlotter::useLabels(bool label)
{
  useLabels_ = label;
}

void ConfigPlotter::overplot(bool doOverplot)
{
  overplot_ = doOverplot;
}

/**.......................................................................
 * Return true if the specified rectangle overlaps the specified
 * circle
 */
bool ConfigPlotter::rectangleOverlapsCircle(double xCenterRect,   double yCenterRect,   double rectWidth, double rectHeight, 
					    double xCenterCircle, double yCenterCircle, double circleRadius)
{
  double dx = xCenterRect - xCenterCircle;
  double dy = yCenterRect - yCenterCircle;
  double theta = atan2(dy, dx) * 180/M_PI;

  // atan2 return between -pi and +pi

  if(theta < 0.0)
    theta += 360.0;

  double xBl = xCenterRect - rectWidth/2;
  double yBl = yCenterRect - rectHeight/2;
  double dxBl = (xBl - xCenterCircle);
  double dyBl = (yBl - yCenterCircle);
  double rBl = sqrt(dxBl * dxBl + dyBl * dyBl);

  double xTl = xCenterRect - rectWidth/2;
  double yTl = yCenterRect + rectHeight/2;
  double dxTl = (xTl - xCenterCircle);
  double dyTl = (yTl - yCenterCircle);
  double rTl = sqrt(dxTl * dxTl + dyTl * dyTl);

  double xBr = xCenterRect + rectWidth/2;
  double yBr = yCenterRect - rectHeight/2;
  double dxBr = (xBr - xCenterCircle);
  double dyBr = (yBr - yCenterCircle);
  double rBr = sqrt(dxBr * dxBr + dyBr * dyBr);

  double xTr = xCenterRect + rectWidth/2;
  double yTr = yCenterRect + rectHeight/2;
  double dxTr = (xTr - xCenterCircle);
  double dyTr = (yTr - yCenterCircle);
  double rTr = sqrt(dxTr * dxTr + dyTr * dyTr);

  double rB = (yCenterRect - rectHeight/2) - yCenterCircle;
  double rT = yCenterCircle - (yCenterRect + rectHeight/2);
  double rR = (xCenterRect - rectWidth/2) - xCenterCircle;
  double rL = xCenterCircle - (xCenterRect + rectWidth/2);

  if(theta == 0.0) {
    return rB < circleRadius;
  } else if(theta > 0.0 && theta < 90.0) {
    return rBl < circleRadius;
  } else if(theta == 90.0) {
    return rL < circleRadius;
  } else if(theta > 90.0 && theta < 180.0) {
    return rTl < circleRadius;
  } else if(theta == 180.0) {
    return rT < circleRadius;
  } else if(theta > 180.0 && theta < 270.0) {
    return rTr < circleRadius;
  } else if(theta == 270.0) {
    return rR < circleRadius;
  } else if(theta > 270.0 && theta < 360.0) {
    return rBr < circleRadius;
  } 
}

void ConfigPlotter::setFillStyle(FillStyle style)
{
  fillStyle_ = style;
}

void ConfigPlotter::useFillFlags(bool use)
{
  useFillFlags_ = use;
}

void ConfigPlotter::setFillFlags(std::vector<bool> fillFlags)
{
  fillFlags_ = fillFlags;
}

void ConfigPlotter::setAz(std::vector<Angle> az)
{
  az_ = az;
}

void ConfigPlotter::setEl(std::vector<Angle> el)
{
  el_ = el;
}

std::ostream& sza::util::operator<<(std::ostream& os, ConfigPlotter::Circle& c)
{
  os << "xCenter = " << c.x_ << " yCenter = " << c.y_ << " radius  = " << c.r_ << std::endl;
  return os;
}

std::ostream& sza::util::operator<<(std::ostream& os, ConfigPlotter::Rectangle& r)
{
  os << "xCenter = " << r.x_ << " yCenter = " << r.y_ << " xWidth  = " << r.xWidth_ << " yWidth  = " << r.yWidth_ << std::endl;

  return os;
}
