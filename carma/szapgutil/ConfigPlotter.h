// $Id: ConfigPlotter.h,v 1.1 2012/06/27 18:05:07 eml Exp $

#ifndef SZA_UTIL_CONFIGPLOTTER_H
#define SZA_UTIL_CONFIGPLOTTER_H

/**
 * @file ConfigPlotter.h
 * 
 * Tagged: Wed Sep  3 11:54:27 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2012/06/27 18:05:07 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/CarmaConfig.h"

#include <vector>

namespace sza {
  namespace util {

    enum FillStyle {
      FILL_SOLID         = 1,
      FILL_OUTLINE       = 2,
      FILL_HATCHED       = 3,
      FILL_CROSS_HATCHED = 4,
    };

    class ConfigPlotter {
    public:

      /**.......................................................................
       * Class to encapsulate a circle
       */
      class Circle {
      public:
  
	Circle(double x, double y, double r) {
	  x_ = x;
	  y_ = y;
	  r_ = r;
	};
  
	friend std::ostream& operator<<(std::ostream& os, Circle& c);

	double x_;
	double y_;
	double r_;
      };

      /**.......................................................................
       * Class to encapsulate a rectangle
       */
      class Rectangle {
      public:

	Rectangle(double xCenter, double yCenter, double xWidth, double yWidth) {
	  x_      = xCenter;
	  y_      = yCenter;
	  xWidth_ = xWidth;
	  yWidth_ = yWidth;
	};
  
	friend std::ostream& operator<<(std::ostream& os, Rectangle& r);

	bool overlaps(Circle& c);
	bool overlaps(Rectangle& r);
  
	double xLeft() {
	  return x_ - xWidth_/2;
	}

	double xRight() {
	  return x_ + xWidth_/2;
	}

	double yTop() {
	  return y_ + yWidth_/2;
	}

	double yBottom() {
	  return y_ - yWidth_/2;
	}

	double x_;
	double y_;
	double xWidth_;
	double yWidth_;
      };

      /**
       * Constructor.
       */
      ConfigPlotter();

      /**
       * Destructor.
       */
      virtual ~ConfigPlotter();

      void plotConfiguration(std::vector<CarmaConfig::PadLocation>& pads);

      void openDevice(std::string device);

      int pgplotId_;

      void setFixedLimits(double xmin, double xmax, double ymin, double ymax);

      void useFixedLimits(bool use);

      CarmaConfig::PadLocation mark(std::vector<CarmaConfig::PadLocation>& pads,
				    double x, double y);

      void clearMarks();

      void getLabelLocation(CarmaConfig::PadLocation& pad, std::vector<CarmaConfig::PadLocation>& pads, std::string label, 
			    double& x, double& y, double& xNear, double& yNear, std::vector<Rectangle>& labels, double thetaDeg=400);

      void getCoords(double theta, double xCurr, double yCurr, double antRadius, double strHalfWidth, double strHalfHeight,
		     double& xCenter, double& yCenter, double& xNear, double& yNear);

      void useLabels(bool label);
      void overplot(bool doOverplot);
      void useFillFlags(bool use);
      void setFillStyle(FillStyle style);
      void setFillFlags(std::vector<bool> fillFlags);
      void setAz(std::vector<Angle> az);
      void setEl(std::vector<Angle> el);

    private:

      FillStyle fillStyle_;
      bool overplot_;
      bool useLabels_;
      bool fixedLims_;
      bool useFillFlags_;

      double fixXmin_, fixXmax_;
      double fixYmin_, fixYmax_;

      double currXmin_, currXmax_;
      double currYmin_, currYmax_;

      std::vector<double> xMarks_, yMarks_;
      bool useMarks_;

      std::vector<bool> fillFlags_;
      std::vector<Angle> az_;
      std::vector<Angle> el_;

      void getPlotLimits(std::vector<CarmaConfig::PadLocation>& pads, 
			 double& xmin, double& xmax, double& ymin, double& ymax,
			 double frac);
      
      int getColor(CarmaConfig::PadLocation& pad);
      int getLineStyle(CarmaConfig::PadLocation& pad);

      bool rectangleOverlapsCircle(double xCenterRect,   double yCenterRect,   double rectWidth, double rectHeight, 
				   double xCenterCircle, double yCenterCircle, double circleRadius);

    }; // End class ConfigPlotter

    std::ostream& operator<<(std::ostream& os, ConfigPlotter::Circle& c);
    std::ostream& operator<<(std::ostream& os, ConfigPlotter::Rectangle& r);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONFIGPLOTTER_H
