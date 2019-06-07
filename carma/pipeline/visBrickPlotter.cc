// $Id: visBrickPlotter.cc,v 1.4 2014/04/30 23:38:54 eml Exp $

#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorHeader.h"

#include "carma/pipeline/VisBrickReader.h"

#include "carma/util/complexManip.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include "carma/szautil/Date.h"
#include "carma/szautil/DirList.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/RangeParser.h"
#include "carma/szautil/Sort.h"
#include "carma/szautil/String.h"

#include "carma/szapgutil/PgUtil.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <sstream>
#include <cerrno>
#include <string>

using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;


typedef vector< carma::correlator::lib::CorrelatorBand> CorrBands;
typedef vector< carma::correlator::lib::CorrelatorBaseline> CorrBaselines;
typedef vector< ::std::complex< float > > Spectra;

typedef enum {
  CORR_SL,
  CORR_WB
} CorrType;

typedef enum {
  PLOT_TIME,
  PLOT_REIM,
  PLOT_SPEC
} PlotType;

typedef enum {
  UPPER,
  LOWER,
  BOTH,
  AUTO 
} SidebandType;

struct BaselineArg {
  int input1;
  int input2;
  SidebandType sideband;
};

typedef enum {
  TYPE_REAL,
  TYPE_IMAG,
  TYPE_PHASE,
  TYPE_AMP,
  TYPE_BFMASK
} DataType;

struct Limits {
  double xMin_;
  double xMax_;

  double yMin_;
  double yMax_;

  double xRef_;
  double yRef_;

  bool first_;

  Limits() {
    first_ = true;
  }

  void reset() {
    first_ = true;
  }

  void adjust(Limits& limits, bool bitmask=false, bool istime=false) {
    adjust(limits.xMin_, limits.yMin_, bitmask, istime);
    adjust(limits.xMax_, limits.yMax_, bitmask, istime);
  }

  void adjust(double x, double y, bool bitmask=false, bool istime=false) 
  {
    if(first_) {

      //------------------------------------------------------------
      // If this is MJD, truncate the reference to the nearest day
      // boundary
      //------------------------------------------------------------

      if(istime) {
	xRef_ = (double)((unsigned)(x));
      } else {
	xRef_ = x;
      }

      yRef_ = y;
      xMin_ = xMax_ = x;
      yMin_ = yMax_ = y;
      first_ = false;
    } else {
      xMin_ = (xMin_ < x) ? xMin_ : x;
      xMax_ = (xMax_ > x) ? xMax_ : x;
      yMin_ = (yMin_ < y) ? yMin_ : y;
      yMax_ = (yMax_ > y) ? yMax_ : y;
    }

    if(bitmask) {
      yMin_ = -0.5;      
      yMax_ = 33.0;
    }
  };

  void setPgplotWindow(PlotType type, bool useMjd) {

    sza::util::PgUtil::setUsedefs(true);
    
    double xRange = xMax_ - xMin_;
    double yRange = yMax_ - yMin_;

    double xMin, xMax;

    if(type == PLOT_TIME) {
      xMin = (xMin_ - xRef_);
      xMax = (xMax_ - xRef_);

      if(useMjd) {
	xMin   *= 24*60*60;
	xMax   *= 24*60*60;
	xRange *= 24*60*60;
      }

    } else {
      xMin = xMin_;
      xMax = xMax_;
    }

    if(xRange < 1e-12)
      xRange = 1.0;

    if(yRange < 1e-12)
      yRange = 1.0;

    sza::util::PgUtil::setXmin(xMin  - 0.1*xRange);
    sza::util::PgUtil::setXmax(xMax  + 0.1*xRange);
    sza::util::PgUtil::setYmin(yMin_ - 0.1*yRange);
    sza::util::PgUtil::setYmax(yMax_ + 0.1*yRange);
  };

};

struct ChanPlotManager {
  Limits limits_;
  double xRef_;
  std::vector<double> x_;
  std::vector<double> y_;
};

struct SpecPlotManager {
  Limits limits_;
  std::vector<double> re_;
  std::vector<double> im_;
  std::vector<unsigned> n_;
  std::vector<double> x_;
  std::vector<double> y_;

  SpecPlotManager() {
    reset();
  }

  void reset() {
    limits_.reset();
    n_.resize(0);
    x_.resize(0);
    y_.resize(0);
    re_.resize(0);
    im_.resize(0);
  }

  void cacheSpectrum(DataType dataType)
  {
    x_.resize(n_.size());
    y_.resize(n_.size());

    switch (dataType) {
    case TYPE_REAL:
      y_ = re_;
      break;
    case TYPE_IMAG:
      y_ = im_;
      break;
    case TYPE_PHASE:
      {
	for(unsigned i=0; i < n_.size(); i++) {
	  complex<float> dval(re_[i], im_[i]);
	  y_[i] = phase(dval);
	}
      }
      break;
    default:
      {
	for(unsigned i=0; i < n_.size(); i++) {
	  complex<float> dval(re_[i], im_[i]);
	  y_[i] = amp(dval);
	}
      }
      break;
    }

    for(unsigned i=0; i < n_.size(); i++) {
      x_[i] = i;
      limits_.adjust(x_[i], y_[i]);
    }
  }
};

struct BandManager {
  Limits limits_;
  SpecPlotManager specPlot_;
  std::map<unsigned, ChanPlotManager> chanMap_;

  void reset() {
    limits_.reset();
  }
};

struct PlotArgs {
  DataType dataType_;
  PlotType plotType_;
  Limits limits_;
  bool useMjd_;    // If true and PLOT_TIME, plot in MJD, else plot in frame number
  bool overplot_;  // If true, make separate plots for each band, else overplot all
  bool autoScale_; // If true, autoscale plots
  std::string baselineStr_;
  BaselineArg baselineArg_;
  double ymin_;
  double ymax_;
  bool blink_;
  bool useFile_;
  std::string startDate_;
  std::string stopDate_;

  bool useFrame_;
  sza::util::String frame_;

  std::vector<unsigned> requestedBandVec_;
  std::vector<unsigned> requestedChanVec_;

  std::map<unsigned, unsigned> requestedFrameMap_;
  std::map<unsigned, unsigned> requestedBandMap_;
  std::map<unsigned, unsigned> requestedChanMap_;

  std::map<unsigned, BandManager> bandMap_;

  // Cache any spectra maintained by this object.  As each band
  // spectrum is calculated, we store the limits in the parent band
  // manager, and adjust the limits in the global object too

  void cacheSpectra() {
    for(std::map<unsigned, BandManager>::iterator iter=bandMap_.begin(); iter != bandMap_.end(); iter++) {
      BandManager& bm = iter->second;
      bm.specPlot_.cacheSpectrum(dataType_);
      bm.limits_ = bm.specPlot_.limits_;

      if(bm.specPlot_.n_.size() > 0)
	limits_.adjust(bm.limits_);
    }
  }

  void rescaleIfRequested() {
    if(!autoScale_) {
      sza::util::PgUtil::setYmin(ymin_);
      sza::util::PgUtil::setYmax(ymax_);
    }
  }

  std::string getDate(unsigned frameNo) {
    return sza::util::Date::mjdToCal(carma::util::Time::MJD(frameNo));
  }

  void reset() {
    limits_.reset();
  }
};

/**.......................................................................
 * Parse a baseline specification
 */
BaselineArg
parseBaselineString( const string & baselineString )
{
  BaselineArg answer;

  const string::size_type 
    delineatorPosition = baselineString.find_first_of( "-" );
  const string::size_type 
    input2LastPosition = baselineString.find_last_of( "1234567890" );

  if ( delineatorPosition == string::npos ||
       input2LastPosition == string::npos ||
       input2LastPosition < delineatorPosition ) {
    throw CARMA_EXCEPTION( IllegalArgumentException,
			   "Baseline argument must be of form m-n[UL] where m and n "
			   "are input numbers and U or L optionally designates Upper "
			   "or Lower sideband." );
  }

  const string::difference_type input2StringLength = 
    input2LastPosition - delineatorPosition;

  const string::size_type
    flavorPosition = baselineString.find_first_of( "ULul" );

  const string input1String = baselineString.substr( 0,
						     delineatorPosition );
  const string input2String = 
    baselineString.substr( delineatorPosition + 1,
			   input2StringLength );

  istringstream in1( input1String );
  in1 >> answer.input1;
  istringstream in2( input2String );
  in2 >> answer.input2;

  ostringstream parsed;
  parsed << answer.input1 << "-" << answer.input2;

  if ( flavorPosition == string::npos ) {
    if ( answer.input1 == answer.input2 ) {
      answer.sideband = AUTO;
    } else {
      answer.sideband = BOTH;
    }
  } else {

    const string flavorString = baselineString.substr( flavorPosition );

    if ( flavorString == "U" ) {
      answer.sideband = UPPER;
      parsed << "U";
    } else if ( flavorString == "u" ) {
      answer.sideband = UPPER;
      parsed << "u";
    } else if ( flavorString == "L" ) {
      answer.sideband = LOWER;
      parsed << "L";
    } else if ( flavorString == "l" ) {
      answer.sideband = LOWER;
      parsed << "l";
    }
  }

  if ( baselineString != parsed.str() ) {
    throw CARMA_EXCEPTION( IllegalArgumentException,
			   "Baseline argument must be of form m-n[UL] where m and n "
			   "are input numbers and U or L optionally designates Upper "
			   "or Lower sideband." );
  }  

  return answer;
}

void getData(const RecordsByFrameMap& recs, PlotArgs& pArgs);
void getData(PlotArgs& pArgs, const frameType& dxval, const complex<float>& dyval, unsigned bfmask, double& xval, double& yval);
void plotData(PlotArgs& pArgs);

void getAndPlotData(std::string fileName, PlotArgs& pArgs);

std::vector<std::string> getFileList(CorrType corrType);
std::vector<std::string> getFileList(CorrType corrType, std::string startStr, std::string stopStr);

DataType dataTypeOf(std::string typeStr)
{
  sza::util::String str(sza::util::String::toLower(typeStr));

  if(str.contains("amp"))
    return TYPE_AMP;
  else if(str.contains("bfmask"))
    return TYPE_BFMASK;
  else if(str.contains("phase"))
    return TYPE_PHASE;
  else if(str.contains("imag"))
    return TYPE_IMAG;
  else
    return TYPE_REAL;
}

PlotType plotTypeOf(std::string typeStr)
{
  sza::util::String str(sza::util::String::toLower(typeStr));

  if(str.contains("time"))
    return PLOT_TIME;
  else if(str.contains("spec"))
    return PLOT_SPEC;
  else
    return PLOT_REIM;
}

CorrType corrTypeOf(std::string typeStr)
{
  sza::util::String str(sza::util::String::toLower(typeStr));

  if(str.contains("wb"))
    return CORR_WB;
  else
    return CORR_SL;
}

double getHighestBitValue(double bitmask);
void timeBitPlot(std::vector<double>& x, ChanPlotManager& pm, std::string xlabel, std::string title, bool doLine, unsigned& first); 
std::string getBfMaskReason(unsigned iBit);
std::string printBits(unsigned int iVal);

/**.......................................................................
 * @author Erik Leitch
 *
 * @description
 * \nReads specified visbrick and plots data.  When no options
 *   prints a collated summary of records.
 *
 * @usage visBrickReader file=<visbrick file> [frame=integ# [band=<band #>] 
 *
 * @key file "" string Visbrick filename. 
 * @key band * string Print details about a band or bands
 * @key chan * string Print details about a particular channel or channels
 * @key frame * string Print details about a particular frame or frames
 * @key baseline 1-2L string Print data about a baseline (requires band).
 *         \n\tString in m-n form with optional sideband delineator of U or 
 *         \n\tL for Upper or Lower sideband (default is both).  For example 
 *         \n\t'1-2U' specifies baseline 1-2 upper sideband.
 * @key verify true bool Verify visbrick correctness & exit with warning if not.
 * @key datatype amp string Type of data to plot (if plottype = time|spec) options are: real/imag/phase/amp/bfmask
 * @key plottype time string Type of plot to create (time/spec/reim)
 * @key auto true bool True to autoscale plots to the data, false to use specified limits
 * @key overplot false bool True to overplot all traces
 * @key mjd true bool True to plot vs. time, else vs. frame number
 * @key blink false bool True to blink through frames if plottype=spec, instead of vector average
 * @key start "" string Start date to plot (ie, 2014-Apr-28:19:47:06.5)
 * @key conv  "" string Date to convert
 * @key stop  "" string Stop date to plot (ie, 2014-Apr-28:19:47:06.5)
 * @key corr wb string Correlator to plot (sl|wb) if file is not specified
 * @key ymin @noDefault double ymin to use for plots (instead of auto-scale)
 * @key ymax @noDefault double ymax to use for plots (instead of auto-scale)
 *
 * @logger DEFAULT_FACILITY carma.pipeline.visBrickReader
 */
int Program::main()
{
  std::string fileName       = getStringParameter("file");
  std::string baselineStr    = getStringParameter("baseline");
  sza::util::String bandStr  = getStringParameter("band");
  sza::util::String chanStr  = getStringParameter("chan");
  sza::util::String frameStr = getStringParameter("frame");
  std::string dataTypeStr    = getStringParameter("datatype");
  std::string plotTypeStr    = getStringParameter("plottype");
  bool autoScale             = getBoolParameter("auto");
  bool overplot              = getBoolParameter("overplot");
  bool useMjd                = getBoolParameter("mjd");
  bool blink                 = getBoolParameter("blink");
  std::string startDate      = getStringParameter("start");
  std::string stopDate       = getStringParameter("stop");
  std::string convDate       = getStringParameter("conv");
  bool useFile               = parameterWasSpecified("file");
  std::string corrStr        = getStringParameter("corr");
  double ymin                = 0;
  double ymax                = 0;
  
  // Inhibit warnings about this damn unused global var from color_table.h
  n_std_cmap += 0;

  PlotArgs pArgs;

  try {
    pArgs.useFrame_ = parameterWasSpecified("frame");
    pArgs.frame_    = frameStr;

    //------------------------------------------------------------
    // Get the list of files we will be reading from
    //------------------------------------------------------------

    pArgs.useFile_ = useFile;

    std::vector<std::string> fileList;
    if(!useFile) {
      CorrType corrType = corrTypeOf(corrStr);
      fileList = getFileList(corrType, startDate, stopDate);
      for(unsigned i=0; i < fileList.size(); i++) {
	COUT(fileList[i]);
      }

      pArgs.startDate_ = startDate;
      pArgs.stopDate_  = stopDate;

    } else {
      fileList.push_back(getStringParameter("file"));
    }

    //------------------------------------------------------------
    // If not autoscaling, get the user-specified limits
    //------------------------------------------------------------

    if(!autoScale) {

      if(parameterWasSpecified("ymin")) {
	ymin = getDoubleParameter("ymin");
      } else {
	ReportError("You must specify ymin if auto=false");
	return 1;
      }

      if(parameterWasSpecified("ymax")) {
	ymax = getDoubleParameter("ymax");
      } else {
	ReportError("You must specify ymax if auto=false");
	return 1;
      }
    }

    if(parameterWasSpecified("conv")) {
      sza::util::Date conv;
      conv.setToDateAndTime(convDate);
      COUT("Date " << conv << " corresponds to frame " << carma::util::Time::computeClosestFrame(conv.mjd()));
      return 0;
    }

    DataType dataType = dataTypeOf(dataTypeStr);
    PlotType plotType = plotTypeOf(plotTypeStr);

    //------------------------------------------------------------
    // Now parse arguments
    //------------------------------------------------------------

    sza::util::RangeParser parser;
    BaselineArg baseline = parseBaselineString(baselineStr);

    //------------------------------------------------------------
    // Now parse bands
    //------------------------------------------------------------

    if(parameterWasSpecified("band")) {
      pArgs.requestedBandVec_ = parser.extractIndexRange(bandStr, 1, 24);
    } else {
      bandStr = "*";
      pArgs.requestedBandVec_ = parser.extractIndexRange(bandStr, 1, 24);
    }

    for(unsigned i=0; i < pArgs.requestedBandVec_.size(); i++) {
      unsigned bandNo = pArgs.requestedBandVec_[i];
      pArgs.requestedBandMap_[bandNo] = bandNo;
    }

    if(parameterWasSpecified("chan")) {
      pArgs.requestedChanVec_ = parser.extractIndexRange(chanStr, 1, 1000);
    } else {
      chanStr = "*";
      pArgs.requestedChanVec_ = parser.extractIndexRange(chanStr, 1, 1000);
    }

    for(unsigned i=0; i < pArgs.requestedChanVec_.size(); i++) {
      unsigned chanNo = pArgs.requestedChanVec_[i];
      pArgs.requestedChanMap_[chanNo] = chanNo;
    }

    //------------------------------------------------------------
    // Finally, construct the plot args we need to extract and plot the
    // data
    //------------------------------------------------------------

    pArgs.plotType_    = plotType;
    pArgs.dataType_    = dataType;
    pArgs.autoScale_   = autoScale;
    pArgs.overplot_    = overplot;
    pArgs.useMjd_      = useMjd;
    pArgs.baselineStr_ = baselineStr;
    pArgs.baselineArg_ = baseline;
    pArgs.ymin_        = ymin;
    pArgs.ymax_        = ymax;
    pArgs.blink_       = blink;

    //------------------------------------------------------------
    // Now iterate over files in the list
    //------------------------------------------------------------

    for(unsigned i=0; i < fileList.size(); i++) {
      getAndPlotData(fileList[i], pArgs);
    }

    if(!(pArgs.plotType_ == PLOT_SPEC && pArgs.blink_ == true))
      plotData(pArgs);

  } catch(sza::util::Exception& err) {
    COUT(err.what());
    return 1;
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
    return 1;
  }

  return 0;
}

/**.......................................................................
 * Get the data requested from a particular file
 */
void getAndPlotData(std::string fileName,
		    PlotArgs& pArgs)
{
  //------------------------------------------------------------
  // Read the file
  //------------------------------------------------------------
  
  CorrelatorVisBrickReader reader(fileName, true);

  //------------------------------------------------------------
  // Iterate over all dates in this file
  //------------------------------------------------------------

  const RecordsByFrameMap recs = reader.getRecordsKeyedByFrame();

  unsigned firstFrame = recs.begin()->first;
  unsigned lastFrame  = recs.rbegin()->first;

  std::map<unsigned, unsigned> actualFrameMap;

  for(RecordsByFrameMap::const_iterator r = recs.begin(); r != recs.end(); ++r ) {
    //    COUT("Frame " << r->first << " (" << sza::util::Date::mjdToCal(carma::util::Time::MJD(r->first)) << ")");
    actualFrameMap[r->first] = r->first;
  }

  COUT("File " << fileName << " contains frames" << std::endl
       << " from: " << firstFrame << " (" << sza::util::Date::mjdToCal(carma::util::Time::MJD(firstFrame)) << ")" << std::endl 
       << "   to: " << lastFrame  << " (" << sza::util::Date::mjdToCal(carma::util::Time::MJD(lastFrame)) << ")");

  //------------------------------------------------------------
  // Parse frame range, either from date, if specified, or from frame
  // string
  //------------------------------------------------------------
 
  sza::util::RangeParser parser;
  sza::util::String frameStr;

  if(!pArgs.useFile_) {
    std::ostringstream os;
    
    sza::util::Date start;
    start.setToDateAndTime(pArgs.startDate_);
  
    sza::util::Date stop;
    stop.setToDateAndTime(pArgs.stopDate_);

    unsigned newFirstFrame = carma::util::Time::computeClosestFrame(start.mjd());
    unsigned newLastFrame  = carma::util::Time::computeClosestFrame(stop.mjd());

    if(newFirstFrame >= firstFrame && newFirstFrame <= lastFrame)
      firstFrame = newFirstFrame;

    if(newLastFrame >= firstFrame && newLastFrame <= lastFrame)
      lastFrame = newLastFrame;

    os << "[" << firstFrame << "-" << lastFrame << "]";

    frameStr = os.str();

  } else {

    if(pArgs.useFrame_) {
      frameStr = pArgs.frame_;
    } else {
      frameStr = "*";
    }

  }

  std::vector<unsigned> frames = parser.extractIndexRange(frameStr, firstFrame, lastFrame);

  //------------------------------------------------------------
  // Now construct a frame map from frames within our range that are
  // actually in this file
  //------------------------------------------------------------

  for(unsigned i=0; i < frames.size(); i++) {
    unsigned frameNo = frames[i];

    if(actualFrameMap.find(frameNo) != actualFrameMap.end())
      pArgs.requestedFrameMap_[frameNo] = frameNo;
  }

  //------------------------------------------------------------
  // If plotting spectra and blinking through individual integrations,
  // iterate over integrations
  //------------------------------------------------------------

  if(pArgs.plotType_ == PLOT_SPEC && pArgs.blink_ == true) {

    for(unsigned i=0; i < frames.size(); i++) {

      if(actualFrameMap.find(frames[i]) == actualFrameMap.end())
	continue;
      
      pArgs.requestedFrameMap_.clear();
      pArgs.requestedFrameMap_[frames[i]] = frames[i];
      
      pArgs.reset();

      getData(recs, pArgs);
      plotData(pArgs);

      std::cout << "\r Plotting integration: " << frames[i] << " (" << pArgs.getDate(frames[i]) << ")";
      fflush(stdout);

      struct timespec ts;
      ts.tv_sec = 0;
      ts.tv_nsec = 500000000;
      nanosleep(&ts, 0);
    }

    //------------------------------------------------------------
    // Else just accumulate plots into a single object
    //------------------------------------------------------------

  } else {
    getData(recs, pArgs);
  }
}

/**.......................................................................
 * Plot data previously accumulated
 */
void plotData(PlotArgs& pArgs)
{
  bool first = true;

  //------------------------------------------------------------
  // First calculate the number of bands that we will be plotting
  //------------------------------------------------------------

  std::map<unsigned, BandManager>& bandMap = pArgs.bandMap_;

  unsigned nBand = 0;
  for(unsigned iBand=0; iBand < pArgs.requestedBandVec_.size(); iBand++) {
    unsigned bandNo = pArgs.requestedBandVec_[iBand];

    if(bandMap.find(bandNo) != bandMap.end()) {
      ++nBand;
    }
  }

  //------------------------------------------------------------
  // Determine how to subdivide the plot surface
  //------------------------------------------------------------

  unsigned nx=1, ny=nBand;

  if(nBand > 4) {
    nx = (unsigned)ceil(sqrt((double)nBand));
    ny = nx;
  }

  sza::util::PgUtil::open("/xs");

  //------------------------------------------------------------
  // If overplotting, we make one plot, else nx x ny plots
  //------------------------------------------------------------

  if(!pArgs.overplot_) {
    sza::util::PgUtil::subplot(nx, ny);
  }

  sza::util::PgUtil::setInteractive(false);
  sza::util::PgUtil::useXaxisTimeLabeling(pArgs.plotType_ == PLOT_TIME && pArgs.useMjd_);
  sza::util::PgUtil::setWnad(pArgs.plotType_ == PLOT_REIM);

  if(pArgs.plotType_ == PLOT_SPEC)
    pArgs.cacheSpectra();

  //------------------------------------------------------------
  // Now we have the number of plots we'll be making.  Iterate over
  // all bands that were requested
  //------------------------------------------------------------

  std::string title;
  std::ostringstream os;

  unsigned nChan = 0;
  for(unsigned iBand=0; iBand < pArgs.requestedBandVec_.size(); iBand++) {

    //------------------------------------------------------------
    // If this band was found, plot all traces for this band
    //------------------------------------------------------------

    unsigned bandNo = pArgs.requestedBandVec_[iBand];

    if(pArgs.bandMap_.find(bandNo) != pArgs.bandMap_.end()) {

      BandManager& bm = pArgs.bandMap_[bandNo];

      // If overplotting all bands, only set up the axes on the first
      // plotted band, and set them up to match the global min/max.
      // Else reset for each band to match that bands min/max
      
      if(pArgs.overplot_) {
	sza::util::PgUtil::setOverplot(true);
	sza::util::PgUtil::setWin(first);
	sza::util::PgUtil::setBox(first);
	pArgs.limits_.setPgplotWindow(pArgs.plotType_, pArgs.useMjd_);
	pArgs.rescaleIfRequested();
	
      } else {
	sza::util::PgUtil::setOverplot(false);
	sza::util::PgUtil::setWin(true);
	sza::util::PgUtil::setBox(true);
	bm.limits_.setPgplotWindow(pArgs.plotType_, pArgs.useMjd_);
	pArgs.rescaleIfRequested();
      }
      
      first = false;

      os.str("");
      os << pArgs.baselineStr_;

      if(!pArgs.overplot_)
	os << " Band " << bandNo;

      std::map<unsigned, ChanPlotManager>& chanVecMap = bm.chanMap_;

      //------------------------------------------------------------
      // Iterate over all channels for this band.  Only zero the
      // channel count at the start of each band if we are not
      // overplotting, since we use it to determine the color of
      // successive traces
      //------------------------------------------------------------

      if(!pArgs.overplot_)
	nChan = 0;
      
      //------------------------------------------------------------
      // Now determine the type of plot we are making -- spectral
      // plot, or time plot
      // 
      // First spectral plotting:
      //------------------------------------------------------------

      if(pArgs.plotType_ == PLOT_SPEC) {
	SpecPlotManager& sm = bm.specPlot_;

	if(sm.x_.size() == 0) {
	  COUT("No channels found... continuing");
	  continue;
	}

	if(pArgs.plotType_ == PLOT_SPEC)
	  sza::util::PgUtil::setMarkerColor(10);
	else
	  sza::util::PgUtil::setMarkerColor(((nChan) % 10) + 1);

	sza::util::PgUtil::linePlot(sm.x_, sm.y_, (char*)"", (char*)"", 
				    (char*)os.str().c_str(), sm.x_.size() > 1);

	sza::util::PgUtil::setOverplot(true);
	sza::util::PgUtil::setWin(false);
	sza::util::PgUtil::setBox(false);
	
	++nChan;

	//------------------------------------------------------------
	// Else time/reim plotting:
	//------------------------------------------------------------

      } else {

	unsigned first = 0xffffffff;
	for(unsigned iChan=0; iChan < pArgs.requestedChanVec_.size(); iChan++) {
	  unsigned chanNo = pArgs.requestedChanVec_[iChan];

	  if(chanVecMap.find(chanNo) != chanVecMap.end()) {
	    ChanPlotManager& pm = chanVecMap[chanNo];

	    //------------------------------------------------------------
	    // Rescale x relative to the global minimum if overplotting
	    // all traces, else rescale to the minimum for this band.
	    // 
	    // But only for time plots
	    //------------------------------------------------------------

	    sza::util::PgUtil::setMarkerColor(((nChan) % 10) + 1);

	    //------------------------------------------------------------
	    // If this is a time plot, rescale the x-axis relative to
	    // the first point
	    //------------------------------------------------------------

	    bool doLine = (pArgs.plotType_ != PLOT_REIM) && pm.x_.size() > 1;

	    if(pArgs.plotType_ == PLOT_TIME) {
	      std::vector<double> x = pm.x_;

	      for(unsigned i=0; i < x.size(); i++) 
		x[i] = (x[i] - (pArgs.overplot_ ? pArgs.limits_.xRef_ : bm.limits_.xRef_))*24*60*60;

	      std::string timeRef = sza::util::Date::mjdToCal(pArgs.overplot_ ? pArgs.limits_.xRef_ : bm.limits_.xRef_);
	      ostringstream ostime;
	      ostime << "Time since " << timeRef;

	      if(pArgs.dataType_ == TYPE_BFMASK)
		timeBitPlot(x, pm, ostime.str(), os.str(), doLine, first);
	      else
		sza::util::PgUtil::linePlot(x, pm.y_, (char*)ostime.str().c_str(), (char*)"", (char*)os.str().c_str(), doLine);

	    } else {
	      sza::util::PgUtil::linePlot(pm.x_, pm.y_, (char*)"", (char*)"", 
					  (char*)os.str().c_str(), doLine);
	    }

	    sza::util::PgUtil::setOverplot(true);
	    sza::util::PgUtil::setWin(false);
	    sza::util::PgUtil::setBox(false);

	    ++nChan;
	  }
	}
      }
    }
  }

  sza::util::PgUtil::close();
}

/**.......................................................................
 * Make a plot of bit values vs. time
 */
void timeBitPlot(std::vector<double>& x, ChanPlotManager& pm, std::string xlabel, std::string title, bool doLine, unsigned& first) 
{
  unsigned needsPlotting = 0x0;

  std::vector<double> y = x;

  for(unsigned i=0; i < pm.y_.size(); i++) {
    needsPlotting |= (unsigned)pm.y_[i];
    y[i] = 0.0;
  }

  //------------------------------------------------------------
  // First plot a bogus trace to define the plot limits
  //------------------------------------------------------------

  y[0] = -0.5;
  y[1] = 33.0;

  int color = sza::util::PgUtil::getMarkerColor();
  sza::util::PgUtil::setMarkerColor(0);
  sza::util::PgUtil::linePlot(x, y, (char*)xlabel.c_str(), (char*)"", (char*)title.c_str(), doLine);

  sza::util::PgUtil::setMarkerColor(color);

  sza::util::PgUtil::setOverplot(true);
  sza::util::PgUtil::setWin(false);
  sza::util::PgUtil::setBox(false);

  //------------------------------------------------------------
  // Now iterate over bits, plotting traces for any bits that were set
  //------------------------------------------------------------

  for(unsigned iBit=0; iBit < 32; iBit++) {

    //------------------------------------------------------------
    // If any ith bits were set, plot this trace.  Else do nothing
    //------------------------------------------------------------

    if((needsPlotting >> iBit) & 0x1) {

      for(unsigned iDat=0; iDat < pm.y_.size(); iDat++) {
	//y[iDat] = (double) iBit + 0.5 * (((unsigned)(pm.y_[iDat]) >> iBit) & 0x1);
	y[iDat] = -1.0 + (double) (iBit+1) * (((unsigned)(pm.y_[iDat]) >> iBit) & 0x1);
      }

      sza::util::PgUtil::linePlot(x, y, (char*)"", (char*)"", (char*)"", doLine);

      // Only print this bit definition the first time it is plotted

      if((first >> iBit) & 0x1) {
	sza::util::PgUtil::pgpTxt(x[0], (float)(iBit), 0.0, 0.0, getBfMaskReason(iBit));
	first &= ~(0x1 << iBit);
      }
    }
  }
}

/**.......................................................................
 * Get the data requested
 */
void getData(const RecordsByFrameMap& recs, PlotArgs& pArgs)
{
  std::map<unsigned, BandManager>& bandChanVecMap = pArgs.bandMap_;

  for(RecordsByFrameMap::const_iterator r = recs.begin(); r != recs.end(); ++r ) {

    //------------------------------------------------------------
    // If this frame is in our map of requested frames
    //------------------------------------------------------------

    if(pArgs.requestedFrameMap_.find(r->first) != pArgs.requestedFrameMap_.end()) {

      //------------------------------------------------------------
      // Iterate over all bands found in this integration
      //------------------------------------------------------------

      const vector<carma::correlator::lib::CorrelatorBand>& bands = r->second->getBands();
      for(unsigned iBand=0; iBand < bands.size(); iBand++) {
	const CorrelatorBand& band = bands[iBand];
	unsigned bandNo = band.getBandNumber();

	//------------------------------------------------------------
	// If this band is in our map of requested bands
	//------------------------------------------------------------

	if(pArgs.requestedBandMap_.find(bandNo) != pArgs.requestedBandMap_.end()) {

	  try {
	    BandManager& bm = bandChanVecMap[bandNo];

	    if(pArgs.plotType_ == PLOT_SPEC && pArgs.blink_)
	      bm.reset();

	    std::map<unsigned, ChanPlotManager>& chanVecMap = bm.chanMap_;

	    //------------------------------------------------------------
	    // Get data for the requested baseline
	    //------------------------------------------------------------
	
	    BaselineArg& baseline = pArgs.baselineArg_;
	    const CorrelatorBaseline& base = band.getBaseline(baseline.input1, baseline.input2);

	    const CorrelatorSideband* sb = 0;
	    switch (baseline.sideband) {
	    case UPPER:
	      sb = &base.getUpperSideband();
	      break;
	    case LOWER:
	      sb = &base.getLowerSideband();
	      break;
	    default:
	      ThrowError("You must specify a sideband type for plotting");
	      break;
	    }
	
	    const DataVector& dv = sb->getData();
	    unsigned bfmask = sb->getValidReason();

	    //------------------------------------------------------------
	    // If we are accumulating spectra, we ignore the channel
	    // selection and put the data into our SpecPlotManager
	    //------------------------------------------------------------

	    if(pArgs.plotType_ == PLOT_SPEC) {

	      SpecPlotManager& pm = bm.specPlot_;

	      if(pArgs.plotType_ == PLOT_SPEC && pArgs.blink_)
		pm.reset();

	      for(unsigned iChan=0; iChan < dv.size(); iChan++) {

		double re = dv[iChan].real();
		double im = dv[iChan].imag();

		//------------------------------------------------------------
		// If this is the first time we've encountered this
		// channel, just store the current value
		//------------------------------------------------------------

		if(pm.n_.size() == iChan) {
		  pm.re_.push_back(re);
		  pm.im_.push_back(im);
		  pm.n_.push_back(0);

		  //------------------------------------------------------------
		  // Else accumulate the vector average for this channel
		  //------------------------------------------------------------

		} else {
		  double remean = pm.re_[iChan];
		  double immean = pm.im_[iChan];
		  unsigned n    = pm.n_[iChan] + 1;

		  remean += (re-remean) / n;
		  immean += (im-immean) / n;
	      
		  pm.re_[iChan] = remean;
		  pm.im_[iChan] = immean;
		  pm.n_[iChan]  = n;
		}
	      
	      }
	  
	    } else {

	      for(unsigned iChan=0; iChan < dv.size(); iChan++) {
		unsigned chanNo = iChan + 1;
	    
		// If this channel is in our map of requested channels, proceed
	    
		if(pArgs.requestedChanMap_.find(chanNo) != pArgs.requestedChanMap_.end()) {
		  ChanPlotManager& pm = chanVecMap[chanNo];
	      
		  //------------------------------------------------------------
		  // Get the requested data val
		  //------------------------------------------------------------

		  double xval, yval;
		  getData(pArgs, r->first, dv[iChan], bfmask, xval, yval);

		  pm.x_.push_back(xval);
		  pm.y_.push_back(yval);

		  //------------------------------------------------------------
		  // Store the min/max for this channel
		  //------------------------------------------------------------

		  pm.limits_.adjust(xval, yval, pArgs.dataType_ == TYPE_BFMASK, pArgs.plotType_ == PLOT_TIME);

		  //------------------------------------------------------------
		  // Store the min/max globally
		  //------------------------------------------------------------
	    
		  pArgs.limits_.adjust(xval, yval, pArgs.dataType_ == TYPE_BFMASK, pArgs.plotType_ == PLOT_TIME);

		  //------------------------------------------------------------
		  // Store the min/max for this band
		  //------------------------------------------------------------

		  bm.limits_.adjust(xval, yval, pArgs.dataType_ == TYPE_BFMASK, pArgs.plotType_ == PLOT_TIME);

		} // End if channel is in our map
	      } // End iterate over all channels in this sideband
	    }
	  } catch(sza::util::Exception& err) {
	    COUT("err.what()");
	  } catch(carma::util::ErrorException& err) {
	    COUT("Caught an error: " << err.what());
	  }
	} // End if band is in our map
      } // End iterate over all bands in this frame
    } // End if frame is in our map
  } // End iterate over all frames in this file
}

void getData(PlotArgs& pArgs, const frameType& dxval, const complex<float>& dyval, unsigned bfmask, double& xval, double& yval)
{
  if(pArgs.plotType_ == PLOT_TIME) {

    switch (pArgs.dataType_) {
    case TYPE_REAL:
      yval = dyval.real();
      break;
    case TYPE_IMAG:
      yval = dyval.imag();
      break;
    case TYPE_PHASE:
      yval = phase(dyval);
      break;
    case TYPE_BFMASK:
      yval = (double) bfmask;
      break;
    default:
      yval = amp(dyval);
      break;
    }

    xval = pArgs.useMjd_ ? carma::util::Time::MJD(dxval) : dxval;

  } else if(pArgs.plotType_ == PLOT_REIM) {
    xval = dyval.real();
    yval = dyval.imag();
  }
}

/**.......................................................................
 * Get a directory listing of all files for this correlator
 */
std::vector<std::string> getFileList(CorrType corrType, std::string startStr, std::string stopStr)
{
  std::vector<std::string> files = getFileList(corrType);

  sza::util::Date startDate;
  startDate.setToDateAndTime(startStr);
  
  sza::util::Date stopDate;
  stopDate.setToDateAndTime(stopStr);
  
  unsigned firstFrame = carma::util::Time::computeClosestFrame(startDate.mjd());
  unsigned lastFrame  = carma::util::Time::computeClosestFrame(stopDate.mjd());

  COUT("First frame = " << firstFrame << " lastFrame = " << lastFrame);

  std::vector<std::string> bracketingFiles;
  for(unsigned i=0; i < files.size(); i++) {

    sza::util::String currStr(files[i]);
    unsigned currStartFrame = currStr.findNextInstanceOf("_", true, " ", false).toInt();

    sza::util::String nextStr;
    unsigned nextStartFrame;

    if(i < files.size()-1) {
      nextStr = files[i+1];
      nextStartFrame = nextStr.findNextInstanceOf("_", true, " ", false).toInt();
    }

    //------------------------------------------------------------
    // If the start frame for this file is after the last frame we are
    // interested in, then stop
    //------------------------------------------------------------

    if(currStartFrame > lastFrame)
      break;

    //------------------------------------------------------------
    // Else if the start frame is after the first frame, add it to our
    // list
    //------------------------------------------------------------

    if(currStartFrame >= firstFrame) {
      bracketingFiles.push_back(files[i]);

      //------------------------------------------------------------
      // Else if this file is before our start time, but the next one
      // is after, add it
      //------------------------------------------------------------

    } else if(i < files.size()-1) {

      if(currStartFrame < firstFrame && nextStartFrame > firstFrame) {
	bracketingFiles.push_back(files[i]);
      }

      //------------------------------------------------------------
      // Else add it if this is the last file
      //------------------------------------------------------------

    } else if(i == files.size()) {
      bracketingFiles.push_back(files[i]);
    }
  }

  return bracketingFiles;
}

/**.......................................................................
 * Get a directory listing of all files for this correlator
 */
std::vector<std::string> getFileList(CorrType corrType)
{
  sza::util::DirList dl("/opt/visbrick", false);

  std::list<sza::util::DirList::DirEnt> fileList = dl.getFiles();
  
  std::vector<std::string> files;

  std::ostringstream os;

  if(corrType == CORR_SL) {

    for(std::list<sza::util::DirList::DirEnt>::iterator iter=fileList.begin(); iter != fileList.end(); iter++) {
      sza::util::String str(iter->name_);
      if(str.contains("slVisBrickData_")) {
	os.str("");
	os << iter->path_ << "/" << iter->name_;
	files.push_back(os.str());
      }
    }

  } else {
    for(std::list<sza::util::DirList::DirEnt>::iterator iter=fileList.begin(); iter != fileList.end(); iter++) {
      sza::util::String str(iter->name_);
      if(str.contains("wbVisBrickData_")) {
	os.str("");
	os << iter->path_ << "/" << iter->name_;
	files.push_back(os.str());
      }
    }
  }

  std::vector<std::string> sortedFiles;
  sortedFiles = sza::util::Sort::sort(files);

  return sortedFiles;
}

double getHighestBitValue(double bitmask)
{
  unsigned iVal = (unsigned)(bitmask);

  for(int i=31; i >= 0; i--) {
    if((iVal >> i) & 0x1)
      return (double)i;
  }

  return 0.0;
}

std::string getBfMaskReason(unsigned iBit)
{
  switch(iBit) {
  case 0:
    return "A1 Phaselock";
    break;
  case 1:
    return "A2 Phaselock";
    break;
  case 2:
    return "A1 Major Tracking";
    break;
  case 3:
    return "A2 Major Tracking";
    break;
  case 4:
    return "A1 Tsys Bad";
    break;
  case 5:
    return "A2 Tsys Bad";
    break;
  case 6:
    return "A1 Shadowed";
    break;
  case 7:
    return "A2 Shadowed";
    break;
  case 8:
    return "A1 Offline";
    break;
  case 9:
    return "A2 Offline";
    break;
  case 10:
    return "A1 Minor Tracking";
    break;
  case 11:
    return "A2 Minor Tracking";
    break;
  case 12:
    return "A1 Cal State";
    break;
  case 13:
    return "A2 Cal State";
    break;
  case 24:
    return "Band offline";
    break;
  case 25:
    return "Unmapped Signal";
    break;
  case 26:
    return "Monitor Data Bad";
    break;
  case 27:
    return "Bad Channel Count";
    break;
  case 28:
    return "No Rx in Sideband";
    break;
  case 29:
    return "Corr Data Missing";
    break;
  case 30:
    return "Corr Data Invalid";
    break;
  default:
    return "Unknown";
    break;
  }
}

std::string printBits(unsigned int iVal)
{
  std::ostringstream os;
  //    os << iVal << " = " << std::hex << iVal << " = ";
  for(unsigned i=0; i < 32; i++) {
    if(i%4 == 0) {
      os << " ";
    }
    os << ((iVal >> i) & 0x1);
  }
  return os.str();
}
