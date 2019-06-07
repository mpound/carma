// $Id: PgUtil.h,v 1.4 2014/04/30 23:38:57 eml Exp $

#ifndef SZA_UTIL_PGUTIL_H
#define SZA_UTIL_PGUTIL_H

/**
 * @file PgUtil.h
 * 
 * Tagged: Fri Aug 22 11:03:53 PDT 2008
 * 
 * @version: $Revision: 1.4 $, $Date: 2014/04/30 23:38:57 $
 * 
 * @author Erik Leitch.
 */
#include "carma/szapgutil/color_tab.h"

#include <string>
#include <vector>

namespace sza {
  namespace util {

    class PgUtil {
    public:

      /**
       * Constructor.
       */
      PgUtil();

      /**
       * Destructor.
       */
      virtual ~PgUtil();

      enum {
	BLACK        = 0,
	WHITE        = 1,
	RED          = 2,
	GREEN        = 3,
	DEEP_BLUE    = 4,
	PALE_BLUE    = 5,
	MAGENTA      = 6,
	YELLOW       = 7,
	ORANGE       = 8,
	YELLOW_GREEN = 9,
	SAGE_GREEN   = 10,
	SLATE_BLUE   = 11,
	PURPLE       = 12,
	PINK         = 13,
	DARK_GRAY    = 14,
	DARK_GREY    = 14,
	LIGHT_GRAY   = 15,
	LIGHT_GREY   = 15,
      };

      static int open(std::string device);
      static void close();
      static void subplot(int nx, int ny);
      static void advance();

      static void setInteractive(bool inter);
      static void useXaxisTimeLabeling(bool use);

      static void greyScale(std::vector<double>& zdata, int nx, int ny, 
			    double xmina=0, double xmaxa=1, double ymina=0, double ymaxa=1, 
			    double *flag=0,double z1=0, double z2=0, 
			    char *xlab="", char *ylab="", char *title="", char *unit="");
      
      static void greyScale(int ndata, double *zdata, int nx, int ny, 
			    double xmina=0, double xmaxa=1, double ymina=0, double ymaxa=1, 
			    double *flag=0,double z1=0, double z2=0, 
			    char *xlab="", char *ylab="", char *title="", char *unit="");
      
      static void greyScale(int ndata, float *zdata, int nx, int ny, 
			    float xmina=0, float xmaxa=1, float ymina=0, float ymaxa=1, 
			    float *flag=0,float z1=0, float z2=0, 
			    char *xlab="", char *ylab="", char *title="", char *unit="");
      
      static void grayScale(int ndata, float *zdata, int nx,int ny, 
			    float xmina=0, float xmaxa=1, float ymina=0, float ymaxa=1, 
			    float *flag=0,float z1=0, float z2=0, 
			    char *xlab="", char *ylab="", char *title="", char *unit="");
      
      // Plot a simple line plot.
      
      static void linePlot(std::vector<double>& xarr, std::vector<double>& yarr, 
			   char* xlab="", char* ylab="", char* title="", 
			   bool doLine=true);

      static void linePlot(int narr, double* xarr, double* yarr, 
			   char* xlab="", char* ylab="", char* title="", 
			   bool doLine=true);

      static void linePlot(int narr, float* xarr, float* yarr, 
			   char* xlab="", char* ylab="", char* title="", 
			   bool doLine=true);

      static void linePlot(std::vector<double>& yarr, bool doLine=true);

      static void setOverplot(bool overplot) {
	overplot_ = overplot;
      }

      static void setWnad(bool wnad) {
        wnad_ = wnad;
      }
      
      static void setWin(bool win) {
        win_ = win;
      }

      static void setBox(bool box) {
        box_ = box;
      }

      static void setVp(bool vp) {
        vp_ = vp;
      }

      static void setTick(bool tick) {
	tick_ = tick;
      }

      static void setLabel(bool label) {
	label_ = label;
      }

      static void setWedge(bool wedge) {
	wedge_ = wedge;
      }

      static void plotPoints(bool plot) {
	plotPoints_ = plot;
      }

      static void setViewport(float xleft, float xright, float ybot, float ytop);

      static void setUsedefs(bool usedefs) {
	usedefs_ = usedefs;
      }

      static void setXmin(float xmin) {
	xmin_ = xmin;
      }

      static void setXmax(float xmax) {
	xmax_ = xmax;
      }

      static void setYmin(float ymin) {
	ymin_ = ymin;
      }

      static void setYmax(float ymax) {
	ymax_ = ymax;
      }

      static void setColormap(std::string cmap);

      static void queryDevice(bool& wasopen);
      static bool haveCursor();

      static void wnad(float xmin, float xmax, float ymin, float ymax);

      static void setMarkerColor(int colorIndex) {
	markerColor_ = colorIndex;
      }

      static int getMarkerColor() {
	return markerColor_;
      }

      static void setMarkerSymbol(int markerSymbol) {
	markerSymbol_ = markerSymbol;
      }

      static void setMarkerSize(float markerSize) {
	markerSize_ = markerSize;
      }

    public:

      static int markerColor_;
      static int markerSymbol_;
      static float markerSize_;
      static bool xaxisTimeLabeling_;
      static bool interactive_;
      static Cmap* cmap_;
      static bool overplot_;
      static bool plotPoints_;
      static bool vp_;
      static bool box_;      
      static bool win_;      
      static bool wnad_;
      static bool tick_;
      static bool label_;
      static bool wedge_;
      static bool usedefs_;
      static float xmin_, xmax_;
      static float ymin_, ymax_;

      // Make a gray scale plot

      static int v_grey2(int ndata, float *zdata, int nx,int ny, 
			 float xmina=0, float xmaxa=1, float ymina=0, float ymaxa=1, 
			 float* flag=0,float z1=0, float z2=0, 
			 char* xlab="", char* ylab="", char* title="", char* unit="");

      // Plot a simple line plot.
      
      static int v_lplot(int narr, float* xarr, float* yarr, 
			 char* xlab="", char* ylab="", char* title="", 
			 bool doLine=true, bool doErr=false);

      static int v_radplot(float data[],int nbin, float rmin, float rmax, 
			   float xmin, float xmax, float ymin, float ymax, int nx, int ny);


      // Module for v_lplot.

      static int v_lnewx(float xmins, float xmaxs, float ymins, float ymaxs, 
			 float *xmin, float *xmax, float *ymin, float *ymax, 
			 int narr,float xarr[], float yarr[]);
	
      // Module for v_lplot.

      static int v_lwrite(int narr, float xarr[], float yarr[], float xmin,
			  float xmax, float ymin, float ymax);

      // Module for v_lplot.

      static int v_lnum(int narr, float xarr[], float yarr[], float xmin,
			float xmax, float ymin, float ymax);

      // Module for v_lplot.
      
      static int v_lten(int narr, float xarr[], float yarr[], float xmin,
			float xmax, float ymin, float ymax);

      // Module for v_hist.

      static int v_lzoom(float xmins, float xmaxs, float ymins, float ymaxs, 
			 float *xmin, float *xmax, float *ymin, float *ymax);

      // Module for v_lplot -- Draw the line plot.
      
      static int v_ldraw(int narr, float xarr[], float yarr[], 
			 std::string xlab, std::string ylab, std::string title,
			 bool doLine, bool doErr,
			 double xmin, double xmax, double ymin, double ymax);
      
      static void pgpTxt(float x, float y, float angle, float fjust, std::string text);


    }; // End class PgUtil

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_PGUTIL_H

/*  LocalWords:  useTimeLabeling
 */
