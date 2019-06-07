#include <iomanip>
#include <iostream>
#include <fstream>

#include "carma/szautil/ArchiveReader.h"
#include "carma/szautil/BitMask.h"
#include "carma/szautil/Date.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/String.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szapgutil/PgUtil.h"

#include "pgplot/cpgplot.h"

#include <vector>
#include <fstream>

using namespace std;
using namespace sza::util;

static const double eps = 1e-12;

PROGRAM_KEYWORDS = {
  { "datfile",     "",    "s", USAGE "File to parse"},
  { "webfile",     "t",   "b", USAGE "True if this is a file created through the web interface"},
  { "pngfileroot", "",    "s", USAGE "Root of any output file"},
  { "combine",     "f",   "b", USAGE "True to make a single plot"},
  { "norm",        "f",   "b", USAGE "If true, (and combine=t) normalize to 1"},
  { "points",      "t",   "b", USAGE "If true, draw points.  If false, draw lines"},
  { "showinvalid", "t",   "b", USAGE "If true, show invalid data.  If false, don't"},
  { "interactive", "f",   "b", USAGE "True for interactive plotting"},
  { "setymin",     "f",   "b", USAGE "True to specify ymin"},
  { "ymin",        "0.0", "d", USAGE "Ymin to specify"},
  { "setymax",     "f",   "b", USAGE "True to specify ymax"},
  { "ymax",        "0.0", "d", USAGE "Ymax to specify"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

//------------------------------------------------------------
// An object to encapsulate a single column from a data file
//------------------------------------------------------------

struct Column {
  std::string name_;
  unsigned id_;
  std::string units_;
  bool isNumeric_;
  double min_;
  double max_;
  std::vector<float> data_;
  std::vector<bool> valid_;

  Column() {
    isNumeric_ = true; // True unless proven false
  };

};

//------------------------------------------------------------
// An object to encapsulate all columns of a data file
//------------------------------------------------------------

struct PlotAxes {

  std::vector<unsigned> goodColorIndices_;
  float xvp1_;
  float xvp2_;
  float yvp1_;
  float yvp2_;

  float xVpPhys1_;
  float xVpPhys2_;
  float yVpPhys1_;
  float yVpPhys2_;

  float xVsPhys1_;
  float xVsPhys2_;
  float yVsPhys1_;
  float yVsPhys2_;

  float chSize_;
  float chPhysSize_;

  unsigned nNumericCols_;

  double ymin_;
  double ymax_;
  unsigned nRow_;
  unsigned startMjdDayNo_;
  std::vector<Date> dates_;
  std::vector<float> mjds_;
  std::vector<Column> columns_;

  bool setYmin_;
  bool setYmax_;
  bool norm_;

  float yminDef_;
  float ymaxDef_;

  bool lastPointWasValid_;
  double lastX_;
  double lastY_;

  bool drawPoints_;
  bool showInvalid_;

  void setDrawPoints(bool drawPoints) {
    drawPoints_ = drawPoints;
  }

  void setShowInvalid(bool showInvalid) {
    showInvalid_ = showInvalid;
  }

  void  initializeColorIndices();

  PlotAxes() {
    xvp1_ = 0.1;
    xvp2_ = 0.9;
    yvp1_ = 0.1;
    yvp2_ = 0.9;

    nNumericCols_ = 0;

    drawPoints_   = true;
    showInvalid_  = true;

    initializeColorIndices();
  };

  void calculateViewportCoordinates();
  void drawData(Column& col, unsigned markerSymbol, unsigned colorIndex);
};

PlotAxes readFile(std::string fileName, bool webFile, bool norm);
void getColumns(std::ifstream& ifStr, PlotAxes& axes, bool webFile);
Column parseColumn(String& line, bool webFile);
void getData(std::ifstream& ifStr, PlotAxes& axes, bool webFile);

void drawLabels(std::string xlab, std::string ylab, std::string title);
void drawAxis(PlotAxes& axes, float xmin, float xmax, float ymin, float ymax, bool xAxisTimeLabeling, bool combine);
void setPlotWindow(float xmin, float xmax, float ymin, float ymax);

/**.......................................................................
 * Parse through an output file of the type produced by my archive reader,
 * and return the data as plottable objects
 */
PlotAxes readFile(std::string fileName, bool webFile, bool norm)
{
  PlotAxes axes;
  axes.norm_ = norm;

  std::ifstream ifStr;
  ifStr.open(fileName.c_str(), ios::in);

  if(!ifStr) {
    ThrowError("Unable to open file: " << fileName);
  }

  getColumns(ifStr, axes, webFile);

  getData(ifStr, axes, webFile);

  ifStr.close();

  return axes;
}

/**.......................................................................
 * Parse all data out of the file
 */
void getData(std::ifstream& ifStr, PlotAxes& axes, bool webFile)
{
  String line, dateStr, timeStr, item;
  Date date;
  unsigned iRow = 0;
  unsigned nCol = axes.columns_.size();
  bool firstValid = true;
  std::string validStr;

  //------------------------------------------------------------
  // Iterate over all lines in the file
  //------------------------------------------------------------

  while(!ifStr.eof()) {

    line.initialize();
    getline(ifStr, line.str());

    line.advanceToNextNonWhitespaceChar();

    if(!line.isEmpty()) {
      dateStr = line.findNextStringSeparatedByChars(" ");
      timeStr = line.findNextStringSeparatedByChars(" ");
    
      axes.dates_[iRow].setTo(dateStr.str(), timeStr.str());

      if(iRow == 0) {
	axes.startMjdDayNo_ = (unsigned)axes.dates_[0].getMjd();
      }


      axes.mjds_[iRow] = (axes.dates_[iRow].getMjd() - axes.startMjdDayNo_)*24*60*60;

      //      COUT(std::setw(15) << std::setprecision(12) << "date = " << axes.dates_[iRow].getMjd() << " start MJD = " << axes.startMjdDayNo_);
      //      COUT("mjd = " << axes.mjds_[iRow]);

      RegDate regDate;
      regDate.setMjd(axes.dates_[iRow].getMjd());

      try {

	for(unsigned iCol=0; iCol < nCol; iCol++) {
	  Column& col = axes.columns_[iCol];
	  
	  item = line.findNextStringSeparatedByChars(" ");
	  
	  if(col.isNumeric_) {
	    try {
	      double value = item.toDouble();
	      col.data_[iRow] = value;

	      col.min_ = (iRow == 0 ? value : (value < col.min_ ? value : col.min_));
	      col.max_ = (iRow == 0 ? value : (value > col.max_ ? value : col.max_));

	      if(firstValid) {
		axes.ymin_ = value;
		axes.ymax_ = value;
		firstValid = false;
	      } else {
		axes.ymin_ = (value < axes.ymin_ ? value : axes.ymin_);
		axes.ymax_ = (value > axes.ymax_ ? value : axes.ymax_);
	      }

	    } catch(...) {
	      col.isNumeric_ = false;
	    }
	  }
	 
	  if(col.isNumeric_ && iRow==0)
	    ++axes.nNumericCols_;
	}
	
	item = line.findNextStringSeparatedByChars(" ", true);

	// Ignore the first bit (index 1) of the validity bitmask -- it refers to the date

	validStr = item.str();

	for(unsigned iCol=0; iCol < nCol; iCol++) 
	  axes.columns_[iCol].valid_[iRow] = (validStr[iCol+1]=='1');

      } catch(Exception& err) {
	COUT("Caught an error: " << err.what());
      }

      ++iRow;
    }

  }

  //------------------------------------------------------------
  // Now iterate over columns to see if we have valid ranges
  //------------------------------------------------------------

  if(axes.norm_) {

    axes.ymin_ = 0.0;
    axes.ymax_ = 1.0;

    for(unsigned iCol=0; iCol < nCol; iCol++) {
      Column& col = axes.columns_[iCol];
      
      if(col.isNumeric_) {

	double yrange = col.max_ - col.min_;
	if(fabs(yrange) <= eps)
	  yrange = col.max_ > eps ? col.max_ : 1.0;

	for(unsigned iRow=0; iRow < axes.nRow_; iRow++)
	  col.data_[iRow] = (col.data_[iRow] - col.min_) / yrange;
	
	col.min_ = 0.0;
	col.max_ = 1.0;
      }

    }
  }

}

/**.......................................................................
 * Read columns out of an mpstore output file
 */
void getColumns(std::ifstream& ifStr, PlotAxes& axes, bool webFile)
{
  String line;
  bool nextLineIsColumnHeader = false;
  bool nextLineIsData         = false;
  
  while(!ifStr.eof() && !nextLineIsData) {

    line.initialize();
    getline(ifStr, line.str());

    if(line.contains("Estimated number of rows = ")) {
      axes.nRow_ = 
	line.findNextInstanceOf("Estimated number of rows = ", true, " ", false).toInt();
      continue;
    }

    if(nextLineIsColumnHeader) {

      if(line.contains("Date/Time (UTC)")) {
	nextLineIsData = true;
	nextLineIsColumnHeader = false;
      } else if(!line.isEmpty()) {
	try {
	  line.advanceToNextNonWhitespaceChar();
	  Column col = parseColumn(line, webFile);
	  axes.columns_.push_back(col);
	} catch(...) {
	}
      }

    }

    if(line.contains("Monitor Point Canonical Name"))
      nextLineIsColumnHeader = true;
  }

  // Now resize the columns to contain the correct number of rows

  for(unsigned iCol=0; iCol < axes.columns_.size(); iCol++) {
    axes.columns_[iCol].data_.resize(axes.nRow_);
    axes.columns_[iCol].valid_.resize(axes.nRow_);
  }

  axes.dates_.resize(axes.nRow_);
  axes.mjds_.resize(axes.nRow_);
}

/**.......................................................................
 * Parse a single column specification from a line
 */
Column parseColumn(String& line, bool webFile)
{
  Column col;

  String item = line.findNextStringSeparatedByChars(" ", true);
  if(item.isEmpty()) {
    ThrowError("Invalid column specification");
  }

  // If this is a web file, the line will start with a '#', so we have
  // to read an extra token to get the name

  if(webFile) {
    item = line.findNextStringSeparatedByChars(" ", true);
    if(item.isEmpty()) {
      ThrowError("Invalid column specification");
    }
  }

  col.name_ = item.str();

  item = line.findNextStringSeparatedByChars(" ", true);
  if(item.isEmpty()) {
    ThrowError("Invalid column specification");
  }

  col.id_ = item.toInt();

  item = line.findNextStringSeparatedByChars(" ", true);
  if(item.isEmpty()) {
    ThrowError("Invalid column specification");
  }

  col.units_ = item.str();

  return col;
}

//------------------------------------------------------------
// Test programs
//------------------------------------------------------------

void plotTest();
void plotFile(std::string fileName, bool webFile, std::string pngFileRoot, bool combine, bool inter, 
	      bool setYmin, float yminDef, bool setYmax, float ymaxDef, bool norm, bool points, bool showInvalid);

int Program::main(void)
{
  std::string datFile     = Program::getParameter("datfile");
  bool webFile            = Program::getBoolParameter("webfile");
  std::string pngFileRoot = Program::getParameter("pngfileroot");
  bool combine            = Program::getBoolParameter("combine");
  bool inter              = Program::getBoolParameter("interactive");
  bool setYmin            = Program::getBoolParameter("setymin");
  bool setYmax            = Program::getBoolParameter("setymax");
  float ymin              = Program::getDoubleParameter("ymin");
  float ymax              = Program::getDoubleParameter("ymax");
  bool norm               = Program::getBoolParameter("norm");
  bool points             = Program::getBoolParameter("points");
  bool showInvalid        = Program::getBoolParameter("showinvalid");

  plotFile(datFile, webFile, pngFileRoot, combine, inter, setYmin, ymin, setYmax, ymax, norm, points, showInvalid);

  return 0;
}

/**.......................................................................
 * Plot the data in a file
 */
void plotFile(std::string fileName, bool webFile, std::string pngFileRoot, bool combine, bool inter, 
	      bool setYmin, float yminDef, bool setYmax, float ymaxDef, bool norm, bool points, bool showInvalid)
{
  //------------------------------------------------------------
  // Now read data
  //------------------------------------------------------------

  PlotAxes axes = readFile(fileName, webFile, combine && norm);

  axes.setYmin_ = setYmin;
  axes.setYmax_ = setYmax;
  axes.yminDef_ = yminDef;
  axes.ymaxDef_ = ymaxDef;

  axes.setDrawPoints(points);
  axes.setShowInvalid(showInvalid);

  //------------------------------------------------------------
  // And make plots
  //------------------------------------------------------------

  std::ostringstream os;

  if(inter) {
    cpgopen("/xs");
  } else {
    if(combine) {
      os << pngFileRoot << ".png/png";
      cpgopen(os.str().c_str());
      cpgsci(1);
      cpgsch(1.0);
      std::cout << pngFileRoot << ".png";
    }
  }

  std::ostringstream xlabel, ylabel, title, legend;

  RegDate date(axes.startMjdDayNo_, 0);
  xlabel << "Hours since " << date.formatCarmaString() << " UT";

  // If combining plots into a single panel, override the min/max of
  // the window with defaults

  float xmin, xmax, ymin, ymax;

  xmin = axes.mjds_[0];
  xmax = axes.mjds_[axes.mjds_.size()-1];

  //  COUT("xmin = " << xmin << " xmax = " << xmax);

  ymin = axes.ymin_;
  ymax = axes.ymax_;

  double xrange = xmax - xmin;
  double yrange = ymax - ymin;

  //  COUT("xrange = " << xrange << " yrange = " << yrange);

  xmin -= xrange * 0.1;
  xmax += xrange * 0.1;
  ymin -= yrange * 0.1;
  ymax += yrange * 0.1;

  if(combine) {
    drawAxis(axes, xmin, xmax, ymin, ymax, true, combine);
    drawLabels(xlabel.str(), "Value", "");
  }

  // Now iterate over columns, plotting any that are plottable

  unsigned iCi     = 0;
  int colorIndex   =  axes.goodColorIndices_[iCi];

  int markerSymbol = -5;
  unsigned nCol    = axes.columns_.size();

  // Iterate over columns, making individual plots (if combine==false)
  // or overplotting (if combine=true)

  unsigned iNumericCol = 0;

  for(unsigned iCol=0; iCol < nCol; iCol++) {

    Column& col = axes.columns_[iCol];

    if(!col.isNumeric_)
      continue;

    title.str("");
    title << col.name_ << " (" << col.units_ << ")";

    legend.str("");
    legend << col.name_;

    // If not combining, use the same marker symbol and color index
    // for each plot

    if(!combine) {

      os.str("");
      os << pngFileRoot << "_" << iNumericCol << ".png/png";

      if(!inter)
	cpgopen(os.str().c_str());

      std::cout << pngFileRoot << "_" << iNumericCol << ".png ";

      cpgsci(1);
      cpgsch(1.0);

      colorIndex   = 10;
      markerSymbol = 16;

      // Page to the next plot

      cpgpage();

      // And re-draw the axis and labels prior to drawing the data

      ymin = col.min_;
      ymax = col.max_;

      yrange = ymax - ymin;

      ymin -= yrange*0.1;
      ymax += yrange*0.1;

      drawAxis(axes, xmin, xmax, ymin, ymax, true, combine);
      drawLabels(xlabel.str(), "Value", title.str().c_str());

    } else {

      // Where we want to write the current legend, in fraction of the
      // viewport

      float yFrac = 1.0 - (float)(iNumericCol + 0.5)/axes.nNumericCols_;

      //      COUT("iNumericCol = " << iNumericCol << " yFrac = " << yFrac);
      // Convert to world coordinates

      float yPtCoord   = axes.yVpPhys1_ + (axes.yVpPhys2_ - axes.yVpPhys1_) * yFrac;
      float yTextCoord = yPtCoord - axes.chPhysSize_/2;

      // X-coordinate of the point will be 2 character widths from the
      // right edge of the plot
      //
      // X-coordinate of the text will be 4 character widths from the
      // right edge of the plot

      float xPtCoord   = axes.xVpPhys2_ + 2 * axes.chPhysSize_;
      float xTextCoord = axes.xVpPhys2_ + 4 * axes.chPhysSize_;

      // Set the viewport to the entire viewsurface (otherwise points
      // plotted outside the vp won't be displayed), set the window to
      // physical coordinates, and write the marker and string

      cpgsvp(0, 1, 0, 1);
      //      COUT("About to call cpgswin 0");
      setPlotWindow(axes.xVsPhys1_, axes.xVsPhys2_, axes.yVsPhys1_, axes.yVsPhys2_);

      //      COUT("Just set window to: " << axes.xVsPhys1_ << " " <<  axes.xVsPhys2_ << " " <<  axes.yVsPhys1_ << " " << axes.yVsPhys2_);

      // Draw a point two character widths from the edge of the right
      // side of the plot

      cpgsci(colorIndex);

      if(axes.drawPoints_)
	cpgpt1(xPtCoord, yPtCoord, markerSymbol);
      else
	cpgptxt(xPtCoord, yTextCoord, 0.0, 0.0, "-");

      cpgptxt(xTextCoord, yTextCoord, 0.0, 0.0, (char*)legend.str().c_str());
    }

    // Now draw the data

    cpgsvp(axes.xvp1_, axes.xvp2_, axes.yvp1_, axes.yvp2_);
    //    COUT("About to call cpgswin 1");
    setPlotWindow(xmin, xmax, 
		  axes.setYmin_ ? axes.yminDef_ : ymin, 
		  axes.setYmax_ ? axes.ymaxDef_ : ymax);

    axes.drawData(col, markerSymbol, colorIndex);

    // Step the color index and marker symbol if we are combining
    // into one plot
    
    iCi = (iCi + 1) % axes.goodColorIndices_.size();
    colorIndex = axes.goodColorIndices_[iCi];
    
    if(markerSymbol == 23)
      markerSymbol = -5;
    else if(markerSymbol == -3)
      markerSymbol = 0;
    else
      ++markerSymbol;

    if(!inter) {
      if(!combine) {
	cpgclos();
      }
    }

    ++iNumericCol;
  }

  if(inter) {
    cpgclos();
  } else {
    if(combine) {
      cpgclos();
    }
  }
}

/**.......................................................................
 * Draw the plot box
 */
void drawAxis(PlotAxes& axes, 
	      float xmin, float xmax, float ymin, float ymax, 
	      bool xAxisTimeLabeling, bool combine)
{
  // If combining plots, re-calculate the size of the viewport

  if(combine) 
    axes.calculateViewportCoordinates();

  cpgsvp(axes.xvp1_, axes.xvp2_, axes.yvp1_, axes.yvp2_);
  //  COUT("About to call cpgswin 2");
  setPlotWindow(xmin, xmax, 
		axes.setYmin_ ? axes.yminDef_ : ymin, 
		axes.setYmax_ ? axes.ymaxDef_ : ymax);

  if(xAxisTimeLabeling) {
    cpgtbox("ZHBCNST",0.0,0,"BCVNST",0.0,0);
  } else {
    cpgbox("BCNST",0.0,0,"BCVNST",0.0,0);
  }
}

/**.......................................................................
 * Draw the plot labels
 */
void drawLabels(std::string xlab, std::string ylab, std::string title)
{
  cpglab(xlab.c_str(), "", title.c_str());
  cpgmtxt("L", 4, 0.5, 0.5, ylab.c_str());
}

void  PlotAxes::initializeColorIndices()
{
  goodColorIndices_.push_back(3);
  goodColorIndices_.push_back(5);
  goodColorIndices_.push_back(6);
  goodColorIndices_.push_back(7);
  goodColorIndices_.push_back(8);
  goodColorIndices_.push_back(9);
  goodColorIndices_.push_back(10);
  goodColorIndices_.push_back(11);
  goodColorIndices_.push_back(15);
}

/**.......................................................................
 * Make a single plot of data
 */
void PlotAxes::drawData(Column& col, unsigned markerSymbol, unsigned colorIndex)
{
  if(!col.isNumeric_)
    return;

  // Iterate over all rows of this column, displaying the data with
  // the specified marker symbol and color index

  for(unsigned iRow=0; iRow < nRow_; iRow++) {
    
    if(!col.valid_[iRow] && !showInvalid_)
      continue;

    float x = mjds_[iRow];
    float y = col.data_[iRow];

    if(drawPoints_) {
      if(col.valid_[iRow]) {
	cpgsci(colorIndex);
      } else {
	cpgsci(2);
      }
      
      cpgpt(1, &x, &y, markerSymbol);

    } else {

      if(iRow==0) {
	lastX_ = x;
	lastY_ = y;
	lastPointWasValid_ = col.valid_[iRow];
      } else {

	if(lastPointWasValid_) {
	  cpgsci(colorIndex);
	} else {
	  cpgsci(2);
	}

	cpgmove(lastX_, lastY_);
	cpgdraw(x, y);

	lastX_ = x;
	lastY_ = y;
	lastPointWasValid_ = col.valid_[iRow];
      }

    }
  }
}

/**.......................................................................
 * Want to draw points at arbitrary position beyond the right edge of
 * the viewport.  The viewport is 
 */
void PlotAxes::calculateViewportCoordinates()
{
  float x1, x2, y1, y2;

  // Get the size of the view surface, in physical coordinates
  // (inches)

  cpgqvsz(1, &x1, &x2, &y1, &y2);

  // Character height of 1.0 corresponds to 1/40 the height of the
  // view surface.  Adjust by the aspect ratio of the plot surface

  chPhysSize_ = (y2-y1)/40 * 0.5;

  //  COUT("x1 = " << x1 << " x2 = " << x2 << " y1 = " << y1 << " y2 = " << y2);
  //  COUT("chSize = " << chPhysSize_);

  // Calculate the longest character string we will have to display

  unsigned nChar=0;
  unsigned nCol=0;

  for(unsigned iCol=0; iCol < columns_.size(); iCol++) {
    Column& col = columns_[iCol];

    if(col.isNumeric_) {
      unsigned nameLen = col.name_.size();
      nChar = nameLen > nChar ? nameLen : nChar;
      ++nCol;
    }
  }

  // Add 8 character heights to the length

  float legendStringSize = (nChar+8) * chPhysSize_;

  //  COUT("nChar = " << nChar << " size = " << legendStringSize);

  // Now we have the maximum legend size in normalized device
  // coordinates.  We want to leave at least this much buffer to the
  // right of the plot:

  xvp1_ = 0.1;
  xvp2_ = 1.0 - legendStringSize / (x2-x1);
  yvp1_ = 0.1;
  yvp2_ = 0.9;

  // Store the physical coordinates of the viewport too

  xVpPhys1_ = xvp1_ * (x2 - x1);
  xVpPhys2_ = xvp2_ * (x2 - x1);
  yVpPhys1_ = yvp1_ * (y2 - y1);
  yVpPhys2_ = yvp2_ * (y2 - y1);

  xVsPhys1_ = x1;
  xVsPhys2_ = x2;
  yVsPhys1_ = y1;
  yVsPhys2_ = y2;
}

void setPlotWindow(float xmin, float xmax, float ymin, float ymax)
{
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;

  if(xrange < eps) {
    xmin -= 0.1 * (xmin < eps ? 1.0 : xmin);
    xmax += 0.1 * (xmin < eps ? 1.0 : xmin);
  }

  //  COUT("yrange = " << yrange << " eps = " << eps << (yrange < eps));

  if(yrange < eps) {
    ymin -= 0.1 * (ymin < eps ? 1.0 : ymin);
    ymax += 0.1 * (ymin < eps ? 1.0 : ymin);
  }

  //  COUT("Caling cpgsinwith xmin = " << xmin << " xmax = " << xmax << " ymin = " << ymin << " ymax = " << ymax);
  cpgswin(xmin, xmax, ymin, ymax);
}
