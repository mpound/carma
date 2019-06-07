#include "carma/antenna/sza/antenna/control/AntennaControl.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/AntennaCorba.h"
#include "carma/antenna/sza/antenna/corba/CarmaMonitorMap.h"
#include "carma/antenna/sza/antenna/corba/CarmaMonitorPointHandler.h"
#include "carma/antenna/sza/antenna/corba/CarmaMonitorPointTranslator.h"
#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

#include "carma/antenna/sza/antenna/canbus/CalTertNew.h"
#include "carma/antenna/sza/antenna/canbus/CalTertOld.h"
#include "carma/antenna/sza/antenna/canbus/Yig.h"

#include "carma/szautil/CalPos.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Temperature.h"

#include "carma/util/ErrorException.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/Corba.h"
#include "carma/monitor/SzaSubsystem.h"
#endif

using namespace std;
using namespace sza::antenna::canbus;
using namespace sza::antenna::corba;
using namespace sza::util;

#if DIR_USE_ANT_CORBA
using namespace carma::monitor;
#endif

// Macros used to access monitor points

#if DIR_USE_ANT_CORBA

#define GET_MP(mpRef, boardName, regName, mpFn)			\
  try {								\
    mpRef.szaBlock_ = share_->getReg(boardName, regName);	\
  } catch(...) {						\
    mpRef.szaBlock_ = 0;					\
  }								\
  mpRef.carmaMp_  = &(antennaMonitor_->mpFn);

#define GET_SZA_DATA(mpRef, dataPtr)		\
  share_->readReg(mpRef.szaBlock_, dataPtr);

#define SET_CARMA_VALUE(mpRef, dataRef)		\
  mpRef.carmaMp_->setValue(dataRef);

#define GET_SZA_DATA_EXT(mpRef, dataPtr)	\
  mpRef.share_->readRegNoLock(mpRef.szaBlock_, dataPtr);

#define SET_CARMA_BOOL_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointBool*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_CHAR_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointChar*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_UCHAR_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointByte*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_SHORT_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointShort*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_INT_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointInt*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_STRING_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointString*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_FLOAT_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointFloat*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_DOUBLE_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointDouble*)mpRef.carmaMp_)->setValue(dataRef);

#define SET_CARMA_ENUM_VALUE(mpRef, dataRef)				\
  ((carma::monitor::MonitorPointEnum*)mpRef.carmaMp_)->setValue((const long)dataRef, 0);

#define SET_CARMA_BOOL_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointBool*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_CHAR_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointChar*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_UCHAR_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointByte*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_SHORT_VALUE_IDX(mpRef, dataRef, idx)				\
  ((carma::monitor::MonitorPointShort*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_INT_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointInt*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_FLOAT_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointFloat*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#define SET_CARMA_DOUBLE_VALUE_IDX(mpRef, dataRef, idx)			\
  ((carma::monitor::MonitorPointDouble*)mpRef.carmaMpVec_[idx])->setValue(dataRef);

#endif

/**.......................................................................
 * Constructor.
 */
CarmaMonitorPointHandler::CarmaMonitorPointHandler(AntennaCorba* parent) 
{
  share_          = 0;
  antennaMonitor_ = 0;
  initialized_    = false;
  newCaltert_     = false;

  initialize(parent);
}

void CarmaMonitorPointHandler::initialize(AntennaCorba* parent)
{
  share_          = parent->getShare();
  newCaltert_     = parent->parent()->parent()->newCaltert();

#if DIR_USE_ANT_CORBA

  antennaMonitor_ = 0;

  // Now allocate the monitor subsystem for this antenna

  try {
    antennaMonitor_ = new carma::monitor::SzaSubsystem(parent->getCarmaAntennaIndex());
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
  }

  if(!antennaMonitor_) {
    ThrowError("Unable to allocate antennaMonitor_ object");
  }
#endif

  extractCommonMonitorPoints();
  extractSpecificMonitorPoints();
  extractSzaCanRcvdMonitorPoints();
  extractAllSzaMonitorPoints();

  initialized_ = true;
}

/**.......................................................................
 * Destructor.
 */
CarmaMonitorPointHandler::~CarmaMonitorPointHandler() {}

/**.......................................................................
 * Get pointers to the CARMA antenna common monitor points expected by
 * the control system
 */
void CarmaMonitorPointHandler::extractCommonMonitorPoints()
{
#if DIR_USE_ANT_CORBA

  // Get the current time

  GET_MP(time_,               "", "", timestamp());

  // Get the online status of this subsystem

  GET_MP(online_,             "", "", online());

  // Get the AntennaCommon::initialized monitor point

  GET_MP(antennaInitialized_, "", "", antennaCommon().initialized());

  //------------------------------------------------------------
  // Drive monitor points
  //------------------------------------------------------------

  GET_MP(sourcename_,     "tracker", "source",     antennaCommon().drive().sourcename());
  GET_MP(rightAscension_, "tracker", "equat_geoc", antennaCommon().drive().rightAscension());
  GET_MP(declination_,    "tracker", "equat_geoc", antennaCommon().drive().declination());
  GET_MP(errorSky_,       "tracker", "errors",     antennaCommon().drive().errorSky());
  GET_MP(state_,          "tracker", "state",      antennaCommon().drive().state());
  GET_MP(safeState_,      "",        "",           antennaCommon().drive().safeState());  // Default to 'SAFE' for SZA
  GET_MP(safeAzLow_,      "",        "",           antennaCommon().drive().safeAzLow());  // Default to full AZ range
  GET_MP(safeAzHigh_,     "",        "",           antennaCommon().drive().safeAzHigh()); // Default to full AZ range
  GET_MP(safeElLow_,      "",        "",           antennaCommon().drive().safeElLow());  // Default to full EL range
  GET_MP(safeElHigh_,     "",        "",           antennaCommon().drive().safeElHigh()); // Default to full EL range
  GET_MP(mode_,           "tracker", "state",      antennaCommon().drive().mode());       // Not recorded for SZA

  //------------------------------------------------------------
  // Track monitor points
  //------------------------------------------------------------

  GET_MP(requestedAzimuth_,   "tracker", "expected", antennaCommon().drive().track().requestedAzimuth());
  GET_MP(actualAzimuth_,      "tracker", "actual",   antennaCommon().drive().track().actualAzimuth());
  GET_MP(errorAzimuth_,       "tracker", "errors",   antennaCommon().drive().track().errorAzimuth());
  GET_MP(errorAzimuthSky_,    "tracker", "errors",   antennaCommon().drive().track().errorAzimuthSky());
  GET_MP(azimuthRate_,        "",        "",         antennaCommon().drive().track().azimuthRate());       // Not settable for SZA -- default to fixed value
  GET_MP(requestedElevation_, "tracker", "expected", antennaCommon().drive().track().requestedElevation());
  GET_MP(actualElevation_,    "tracker", "actual",   antennaCommon().drive().track().actualElevation());
  GET_MP(errorElevation_,     "tracker", "errors",   antennaCommon().drive().track().errorElevation());
  GET_MP(elevationRate_,      "",        "",         antennaCommon().drive().track().elevationRate());     // Not settable for SZA -- default to fixed value
  GET_MP(wrapLogic_,          "",        "",         antennaCommon().drive().track().wrapLogic());         // Not settable for SZA -- default to fixed value
  GET_MP(emergencyOff_,       "pmac",    "axis_stat",antennaCommon().drive().track().emergencyOff());
  GET_MP(manualSwitch_,       "pmac",    "axis_stat",antennaCommon().drive().track().manualSwitch());
  GET_MP(trackTolerance_,     "",        "",         antennaCommon().drive().track().trackTolerance());    // Not settable for SZA -- default to fixed value

  //------------------------------------------------------------
  // Point monitor points
  //------------------------------------------------------------

  GET_MP(mountOffsetAz_,      "tracker", "collimation",  antennaCommon().drive().point().mountOffsetAz());
  GET_MP(mountOffsetEl_,      "tracker", "collimation",  antennaCommon().drive().point().mountOffsetEl());
  GET_MP(offsetAz_,           "tracker", "horiz_off",    antennaCommon().drive().point().offsetAz());
  GET_MP(offsetEl_,           "tracker", "horiz_off",    antennaCommon().drive().point().offsetEl());
  GET_MP(refraction_,         "tracker", "refraction",   antennaCommon().drive().point().refraction());
  GET_MP(refractionModel_,    "tracker", "axis",         antennaCommon().drive().point().refractionModel());
  GET_MP(magnitude_,          "tracker", "tilts",        antennaCommon().drive().point().magnitude());
  GET_MP(direction_,          "tracker", "tilts",        antennaCommon().drive().point().direction());

  //------------------------------------------------------------
  // Constants monitor points
  //------------------------------------------------------------

  GET_MP(coefChange_,          "",        "",             antennaCommon().drive().point().constants().coefChange());

  GET_MP(optElCollErr_,        "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(0).elCollErr());
  GET_MP(optCrossElCollErr_,   "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(0).crossElCollErr());
  GET_MP(optSag_,              "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(0).sag());

  GET_MP(rx1mmElCollErr_,      "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(1).elCollErr());
  GET_MP(rx1mmCrossElCollErr_, "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(1).crossElCollErr());
  GET_MP(rx1mmSag_,            "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(1).sag());

  GET_MP(rx3mmElCollErr_,      "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(2).elCollErr());
  GET_MP(rx3mmCrossElCollErr_, "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(2).crossElCollErr());
  GET_MP(rx3mmSag_,            "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(2).sag());

  GET_MP(rx1cmElCollErr_,      "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(3).elCollErr());
  GET_MP(rx1cmCrossElCollErr_, "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(3).crossElCollErr());
  GET_MP(rx1cmSag_,            "",        "",             antennaCommon().drive().point().constants().apertureCoefficients(3).sag());

  GET_MP(selectedApert_,       "tracker", "axis",         antennaCommon().drive().point().constants().selectedApert());

  //------------------------------------------------------------
  // Limit monitor points
  //------------------------------------------------------------

  GET_MP(azSwLimit_,        "pmac",    "axis_stat", antennaCommon().drive().limit().azSwLimit());
  GET_MP(elSwLimit_,        "pmac",    "axis_stat", antennaCommon().drive().limit().elSwLimit());
  GET_MP(azHwLimit_,        "tracker", "state",     antennaCommon().drive().limit().azHwLimit());
  GET_MP(elHwLimit_,        "tracker", "state",     antennaCommon().drive().limit().elHwLimit());
  GET_MP(azLowSwLimitVal_,  "tracker", "az_limits", antennaCommon().drive().limit().azLowSwLimitVal());
  GET_MP(azHighSwLimitVal_, "tracker", "az_limits", antennaCommon().drive().limit().azHighSwLimitVal());
  GET_MP(azLowHwLimitVal_,  "",        "",          antennaCommon().drive().limit().azLowHwLimitVal());  // Not recorded for SZA
  GET_MP(azHighHwLimitVal_, "",        "",          antennaCommon().drive().limit().azHighHwLimitVal()); // Not recorded for SZA
  GET_MP(elLowSwLimitVal_,  "tracker", "el_limits", antennaCommon().drive().limit().elLowSwLimitVal());
  GET_MP(elHighSwLimitVal_, "tracker", "el_limits", antennaCommon().drive().limit().elHighSwLimitVal());
  GET_MP(elLowHwLimitVal_,  "",        "",          antennaCommon().drive().limit().elLowHwLimitVal());  // Not recorded for SZA
  GET_MP(elHighHwLimitVal_, "",        "",          antennaCommon().drive().limit().elHighHwLimitVal()); // Not recorded for SZA

  //------------------------------------------------------------
  // LO monitor points
  //------------------------------------------------------------

  GET_MP(oscFreq_,    "",    "",             antennaCommon().lO().oscFreq()); // Always 10 MHz
  GET_MP(yigFreq_,    "yig", "frequency",    antennaCommon().lO().yigFreq());
  GET_MP(yigIFLevel_, "yig", "ifLevel",      antennaCommon().lO().yigIFLevel());
  GET_MP(yigError_,   "yig", "errorVoltage", antennaCommon().lO().yigError());
  GET_MP(yigState_,   "yig", "lockState",    antennaCommon().lO().yigState());
  GET_MP(yigSweep_,   "yig", "sweepStatus",  antennaCommon().lO().yigSweep());
  GET_MP(loFreq_,     "",    "",             antennaCommon().lO().loFreq()); // Always 10 MHz
  GET_MP(loSweep_,    "yig", "sweepStatus",  antennaCommon().lO().loSweep());
  GET_MP(loState_,    "yig", "lockState",    antennaCommon().lO().loState());

  //------------------------------------------------------------
  // Receivers monitor points
  //------------------------------------------------------------

  GET_MP(currentRx_,       "",        "",            antennaCommon().receivers().currentRx());   // Not currently recorded for SZA
  GET_MP(rxState_,         "",        "",            antennaCommon().receivers().rxState());     // Not currently recorded for SZA
  GET_MP(rxTsysState_,     "",        "",            antennaCommon().receivers().rxTsysState()); // No correspondent for SZA
  GET_MP(rxTsys_,          "",        "",            antennaCommon().receivers().rxTsys());      // No correspondent for SZA
  GET_MP(rxOffsetAz_,      "tracker", "collimation", antennaCommon().receivers().rxOffsetAz());
  GET_MP(rxOffsetEl_,      "tracker", "collimation", antennaCommon().receivers().rxOffsetEl());
  GET_MP(compressorState_, "",        "",            antennaCommon().receivers().compressorState()); // Not recorded for SZA
  GET_MP(dewarTemp_,       "rx",      "tempSensor",  antennaCommon().receivers().dewarTemp()); 

  //------------------------------------------------------------
  // Location monitor points
  //------------------------------------------------------------

  GET_MP(latitude_,  "tracker", "siteActual", antennaCommon().location().latitude());
  GET_MP(longitude_, "tracker", "siteActual", antennaCommon().location().longitude());
  GET_MP(altitude_,  "tracker", "siteActual", antennaCommon().location().altitude());

  //------------------------------------------------------------
  // Optical telescope monitor points
  //------------------------------------------------------------
/* These are handled by a separate process, the carmaOpticaltelescope.

  GET_MP(sizeX_,          "", "", antennaCommon().opticalTel().sizeX());     // N/A for SZA     
  GET_MP(sizeY_,          "", "", antennaCommon().opticalTel().sizeY());
  GET_MP(offsetX_,        "", "", antennaCommon().opticalTel().offsetX());
  GET_MP(offsetY_,        "", "", antennaCommon().opticalTel().offsetY());
  GET_MP(azFov_,          "", "", antennaCommon().opticalTel().azFov());
  GET_MP(elFov_,          "", "", antennaCommon().opticalTel().elFov());
  GET_MP(imageRotation_,  "", "", antennaCommon().opticalTel().imageRotation());
  GET_MP(cover_,          "", "", antennaCommon().opticalTel().cover());
*/

  //------------------------------------------------------------
  // Optics monitor points
  //------------------------------------------------------------

  GET_MP(polarization_,   "", "", antennaCommon().optics().polarization()); // N/A for SZA.  Default to fixed value

  //------------------------------------------------------------
  // Calibrator monitor points
  //------------------------------------------------------------

  GET_MP(calState_,       "caltert", "calibPosReq", antennaCommon().calibrator().calState());     // Combination of calibPosReq and calibState
  GET_MP(skyTemp_,        "",        "",            antennaCommon().calibrator().skyTemp());      // Don't construct for SZA
  GET_MP(ambTemp_,        "caltert", "calibTemp",   antennaCommon().calibrator().ambTemp());
  GET_MP(fixedTemp_,      "",        "",            antennaCommon().calibrator().fixedTemp());    // No correspondent for SZA
  GET_MP(partialTrans_,   "",        "",            antennaCommon().calibrator().partialTrans()); // No correspondent for SZA
  GET_MP(spillOver_,      "",        "",            antennaCommon().calibrator().spillOver());    // No correspondent for SZA

  //------------------------------------------------------------
  // Secondary monitor points
  //------------------------------------------------------------

  GET_MP(focusState_,     "", "", antennaCommon().secondary().focusState()); // Focus is fixed for SZA
  GET_MP(focusZ_,         "", "", antennaCommon().secondary().focusZ());     // Focus is fixed for SZA
#endif
}

/**.......................................................................
 * Get pointers to the CARMA antenna common monitor points specific to
 * the SZA
 */
void CarmaMonitorPointHandler::extractSpecificMonitorPoints()
{
#if DIR_USE_ANT_CORBA

  //------------------------------------------------------------
  // Antenna IF (PAM) monitor points
  //------------------------------------------------------------

  GET_MP(totalPamAtten_,  "ifmod", "totalAtten",   antennaIfContainer().antennaIF().attenSet());
  GET_MP(inputPamAtten_,  "ifmod", "inputAtten",   antennaIfContainer().antennaIF().setInputAtten());
  GET_MP(outputPamAtten_, "ifmod", "outputAtten",  antennaIfContainer().antennaIF().setOutputAtten());
  GET_MP(ifTotalPower_,   "ifmod", "ifTotalPower", antennaIfContainer().antennaIF().ifOutTotalPower());


  //------------------------------------------------------------
  // Varactor monitor points
  //------------------------------------------------------------

  GET_MP(varLockStatus_,   "varactor", "lockStatus",         gunn1cm().varactor().lockStatus());
  GET_MP(varSweepEnabled_, "varactor", "sweepStatus",        gunn1cm().varactor().sweepEnabled());
  GET_MP(varGunnEnabled_,  "varactor", "statusRegister",     gunn1cm().varactor().gunnEnabled());
  GET_MP(varNoiseMeter_,   "varactor", "noiseMeterVoltage",  gunn1cm().varactor().noiseMeter());
  GET_MP(varIfLevel_,      "varactor", "ifLevel",            gunn1cm().varactor().ifLevel());
  GET_MP(varLoopGain_,     "varactor", "loopGainResistance", gunn1cm().varactor().loopGainResistance());
  GET_MP(varGunnCurrent_,  "varactor", "biasCurrent",        gunn1cm().varactor().gunnCurrent());

  //------------------------------------------------------------
  // Bias monitor points
  //------------------------------------------------------------

  GET_MP(biasLockStatus_,   "bias", "phaseLockState",       bias().phaseLockState());
  GET_MP(biasSweepStatus_,  "bias", "sweepStatus",          bias().sweepStatus());
  
#endif
}

/**.......................................................................
 * Write values to the CARMA antenna common monitor points
 */
void CarmaMonitorPointHandler::stageCommonMonitorPoints()
{
  // Do nothing if not initialized

  if(!initialized_)
    return;

#if DIR_USE_ANT_CORBA

  TimeVal time;
  time.setToCurrentTime();
  SET_CARMA_DOUBLE_VALUE(time_, time.getMjd());
  SET_CARMA_BOOL_VALUE(online_, true);

  sza::util::Angle angle;

  // Get the current receiver

  sza::util::Rx::Id rxId;
  share_->getRx(rxId);

  // Get the current control system initialization state, and write it
  // to the monitor system

  bool initialized;
  share_->getInitialized(initialized);
  SET_CARMA_BOOL_VALUE(antennaInitialized_, initialized);

  //============================================================
  // Drive monitor points
  //============================================================
	  
  //------------------------------------------------------------
  // sourcename
  //------------------------------------------------------------

  unsigned char ucharData[SRC_LEN+1];
  GET_SZA_DATA(sourcename_, ucharData);
  std::string str((const char*)ucharData);
  SET_CARMA_STRING_VALUE(sourcename_, str);

  //------------------------------------------------------------
  // equatorial positions (RA/Dec)
  //------------------------------------------------------------

  int intData[3];
  GET_SZA_DATA(rightAscension_, intData);
  angle.setMas((double)intData[0]);
  SET_CARMA_DOUBLE_VALUE(rightAscension_, angle.radians());
  angle.setMas((double)intData[1]);
  SET_CARMA_DOUBLE_VALUE(declination_, angle.radians());

  //------------------------------------------------------------
  // Error on the sky
  //------------------------------------------------------------

  int actual[3];
  int requested[3];
  GET_SZA_DATA(actualAzimuth_, actual);
  GET_SZA_DATA(requestedAzimuth_, requested);

  double caza, saza, cazr, sazr;
  double cela, sela, celr, selr;

  angle.setMas(actual[0]);
  caza = cos(angle.radians());
  saza = sin(angle.radians());

  angle.setMas(requested[0]);
  cazr = cos(angle.radians());
  sazr = sin(angle.radians());

  angle.setMas(actual[1]);
  cela = cos(angle.radians());
  sela = sin(angle.radians());

  angle.setMas(requested[1]);
  celr = cos(angle.radians());
  selr = sin(angle.radians());

  // Error on sky is acos() of the dot product of the expected and actual vectors:

  double dp = (celr*sazr)*(cela*saza) + (celr*cazr)*(cela*caza) + selr*sela;

  // Numerical precision can cause the above calculation to be > 1 if
  // the positions are very close to identical (zero error).  acos(1 +
  // delta) will give NaN, so we protect against this explicitly:

  if(dp > 1.0) {
    dp =  1.0;
  } else if(dp < -1.0) {
    dp = -1.0;
  }

  angle.setRadians(acos(dp));
  SET_CARMA_FLOAT_VALUE(errorSky_, angle.arcsec());

  //------------------------------------------------------------
  // Default values for safe monitor points
  //------------------------------------------------------------

  SET_CARMA_ENUM_VALUE(safeState_,    
		       carma::monitor::AntennaCommon::
		       SafeStateMonitorPointEnum::SAFE);
  SET_CARMA_FLOAT_VALUE(safeAzLow_,    0.0);
  SET_CARMA_FLOAT_VALUE(safeAzHigh_, 360.0);
  SET_CARMA_FLOAT_VALUE(safeElLow_,    0.0);
  SET_CARMA_FLOAT_VALUE(safeElHigh_,  90.0);

  writeTrackingStateMonitorPoints();

  //============================================================
  // Track monitor points
  //============================================================

  //------------------------------------------------------------
  // Requested and actual position
  //------------------------------------------------------------

  GET_SZA_DATA(requestedAzimuth_, intData);
  angle.setMas((double)intData[0]);
  SET_CARMA_DOUBLE_VALUE(requestedAzimuth_, angle.degrees());
  angle.setMas((double)intData[1]);
  SET_CARMA_DOUBLE_VALUE(requestedElevation_, angle.degrees());

  GET_SZA_DATA(actualAzimuth_, intData);
  angle.setMas((double)intData[0]);
  SET_CARMA_DOUBLE_VALUE(actualAzimuth_, angle.degrees());
  angle.setMas((double)intData[1]);
  SET_CARMA_DOUBLE_VALUE(actualElevation_, angle.degrees());

  sza::util::Angle actualElevation = angle;

  //------------------------------------------------------------
  // Tracking errors
  //------------------------------------------------------------

  GET_SZA_DATA(errorAzimuth_, intData);
  angle.setMas((double)intData[0]);
  SET_CARMA_FLOAT_VALUE(errorAzimuth_,    angle.arcsec());

  // errorAzimuthSky is defined to be the arc from the actual position
  // to the same elevation at the requested position.  This differs
  // from errorAzimuth by cosine of the elevation

  SET_CARMA_FLOAT_VALUE(errorAzimuthSky_, angle.arcsec() * cos(actualElevation.radians()));

  angle.setMas((double)intData[1]);
  SET_CARMA_FLOAT_VALUE(errorElevation_, angle.arcsec());

  // Default these to close the real value (about 1 deg/sec = 60 deg/min)

  SET_CARMA_FLOAT_VALUE(azimuthRate_,   60.0);
  SET_CARMA_FLOAT_VALUE(elevationRate_, 60.0);
  SET_CARMA_ENUM_VALUE(wrapLogic_, AntennaCommon::WrapLogicMonitorPointEnum::ZERO);

  // E-stop logic requires parsing bits of the pmac.axis_stat registere

  unsigned int uintData[3];
  GET_SZA_DATA(emergencyOff_, uintData);
  bool estopsClosed   = (uintData[0]>>22 & 0x1)==0;
  estopsClosed = estopsClosed && (uintData[0]>>23 & 0x1)==0;

  SET_CARMA_ENUM_VALUE(emergencyOff_, estopsClosed ? 
		       AntennaCommon::EmergencyOffMonitorPointEnum::OK : 
		       AntennaCommon::EmergencyOffMonitorPointEnum::OFF);

  bool remoteControl = (uintData[0]>>9 & 0x1)==0;
  SET_CARMA_ENUM_VALUE(manualSwitch_, remoteControl ? 
		       AntennaCommon::ManualSwitchMonitorPointEnum::OK : 
		       AntennaCommon::ManualSwitchMonitorPointEnum::MANUAL);

  // Default to 5 arcsec

  SET_CARMA_FLOAT_VALUE(trackTolerance_, 5.0);

  //------------------------------------------------------------
  // Point monitor points
  //------------------------------------------------------------

  angle = share_->getCarmaAzOffset();
  SET_CARMA_DOUBLE_VALUE(offsetAz_, angle.arcmin());

  angle = share_->getCarmaElOffset();
  SET_CARMA_DOUBLE_VALUE(offsetEl_, angle.arcmin());

  angle = share_->getCarmaMountAzOffset();
  SET_CARMA_DOUBLE_VALUE(mountOffsetAz_, angle.arcmin());

  angle = share_->getCarmaMountElOffset();
  SET_CARMA_DOUBLE_VALUE(mountOffsetEl_, angle.arcmin());

  GET_SZA_DATA(refraction_, intData);
  angle.setMas(intData[2]);
  SET_CARMA_FLOAT_VALUE(refraction_, angle.arcmin());

  GET_SZA_DATA(refractionModel_, uintData);
  SET_CARMA_ENUM_VALUE(refractionModel_, uintData[0]==1 ? 
		       AntennaCommon::RefractionModelMonitorPointEnum::RADIO :
		       AntennaCommon::RefractionModelMonitorPointEnum::OPTICAL);

  //  GET_MP(magnitude_,          "tracker", "tilts",        antennaCommon().drive().point().magnitude());
  //  GET_MP(direction_,          "tracker", "tilts",        antennaCommon().drive().point().direction());

  //------------------------------------------------------------
  // Constants monitor points
  //------------------------------------------------------------

  SET_CARMA_DOUBLE_VALUE(coefChange_, share_->getLastMjdCoefficientsChanged());

  GET_SZA_DATA(selectedApert_, uintData);

  if(uintData[0]==0) {
    SET_CARMA_ENUM_VALUE(selectedApert_, AntennaCommon::SelectedApertMonitorPointEnum::OPTICAL);
  } else {

    switch(rxId) {
    case sza::util::Rx::RX1MM:
      SET_CARMA_ENUM_VALUE(selectedApert_, AntennaCommon::SelectedApertMonitorPointEnum::RADIO1MM);
      break;
    case sza::util::Rx::RX3MM:
      SET_CARMA_ENUM_VALUE(selectedApert_, AntennaCommon::SelectedApertMonitorPointEnum::RADIO3MM);
      break;
    default:
      SET_CARMA_ENUM_VALUE(selectedApert_, AntennaCommon::SelectedApertMonitorPointEnum::RADIO1CM);
      break;
    }

  }

  SET_CARMA_DOUBLE_VALUE(optElCollErr_, 0.0);
  SET_CARMA_DOUBLE_VALUE(optCrossElCollErr_, 0.0);
  SET_CARMA_DOUBLE_VALUE(optSag_, 0.0);

  Angle sFlex, cFlex;

  SET_CARMA_DOUBLE_VALUE(rx1cmElCollErr_,      share_->getCarmaApertureElOffset(sza::util::Rx::RX1CM).arcmin());
  SET_CARMA_DOUBLE_VALUE(rx1cmCrossElCollErr_, share_->getCarmaApertureAzOffset(sza::util::Rx::RX1CM).arcmin());
  share_->getCarmaApertureFlexure(sza::util::Rx::RX1CM, sFlex, cFlex);
  SET_CARMA_DOUBLE_VALUE(rx1cmSag_, cFlex.arcmin());

  SET_CARMA_DOUBLE_VALUE(rx3mmElCollErr_,      share_->getCarmaApertureElOffset(sza::util::Rx::RX3MM).arcmin());
  SET_CARMA_DOUBLE_VALUE(rx3mmCrossElCollErr_, share_->getCarmaApertureAzOffset(sza::util::Rx::RX3MM).arcmin());
  share_->getCarmaApertureFlexure(sza::util::Rx::RX3MM, sFlex, cFlex);
  SET_CARMA_DOUBLE_VALUE(rx3mmSag_, cFlex.arcmin());

  SET_CARMA_DOUBLE_VALUE(rx1mmElCollErr_,      share_->getCarmaApertureElOffset(sza::util::Rx::RX1MM).arcmin());
  SET_CARMA_DOUBLE_VALUE(rx1mmCrossElCollErr_, share_->getCarmaApertureAzOffset(sza::util::Rx::RX1MM).arcmin());
  share_->getCarmaApertureFlexure(sza::util::Rx::RX1MM, sFlex, cFlex);
  SET_CARMA_DOUBLE_VALUE(rx1mmSag_, cFlex.arcmin());

  //------------------------------------------------------------
  // Limit monitor points
  //------------------------------------------------------------

  GET_SZA_DATA(azSwLimit_, uintData);

  bool lowAzLimitSet = (uintData[0]>>0) & 0x1;
  bool hiAzLimitSet  = (uintData[0]>>1) & 0x1;
  bool lowElLimitSet = (uintData[0]>>2) & 0x1;
  bool hiElLimitSet  = (uintData[0]>>3) & 0x1;

  if(lowAzLimitSet) {
    SET_CARMA_ENUM_VALUE(azSwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::MINUSLIM);
    SET_CARMA_ENUM_VALUE(azHwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::MINUSLIM);
  } else if(hiAzLimitSet) {
    SET_CARMA_ENUM_VALUE(azSwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::PLUSLIM);
    SET_CARMA_ENUM_VALUE(azHwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::PLUSLIM);
  } else {
    SET_CARMA_ENUM_VALUE(azSwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::OK);
    SET_CARMA_ENUM_VALUE(azHwLimit_, AntennaCommon::AzSwLimitMonitorPointEnum::OK);
  }

  if(lowElLimitSet) {
    SET_CARMA_ENUM_VALUE(elSwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::MINUSLIM);
    SET_CARMA_ENUM_VALUE(elHwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::MINUSLIM);
  } else if(hiElLimitSet) {
    SET_CARMA_ENUM_VALUE(elSwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::PLUSLIM);
    SET_CARMA_ENUM_VALUE(elHwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::PLUSLIM);
  } else {
    SET_CARMA_ENUM_VALUE(elSwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::OK);
    SET_CARMA_ENUM_VALUE(elHwLimit_, AntennaCommon::ElSwLimitMonitorPointEnum::OK);
  }

  // Write the software limits to both the software and hardware limit
  // registers, since CARMA's move command apparently checks both
  // before commanding a position (why isn't it sufficient to check
  // the software limits, since they must always be more conservative
  // than the hw limits?  And if someone mis-enters the software
  // limits, they can just as easily mis-enter the hw limits too, if
  // they are not readable from the hardware directly)

  GET_SZA_DATA(azLowSwLimitVal_, intData);
  angle.setMas(intData[0]);
  SET_CARMA_FLOAT_VALUE(azLowSwLimitVal_, angle.degrees());
  SET_CARMA_FLOAT_VALUE(azLowHwLimitVal_, angle.degrees());

  angle.setMas(intData[1]);
  SET_CARMA_FLOAT_VALUE(azHighSwLimitVal_, angle.degrees());
  SET_CARMA_FLOAT_VALUE(azHighHwLimitVal_, angle.degrees());

  GET_SZA_DATA(elLowSwLimitVal_, intData);
  angle.setMas(intData[0]);
  SET_CARMA_FLOAT_VALUE(elLowSwLimitVal_, angle.degrees());
  SET_CARMA_FLOAT_VALUE(elLowHwLimitVal_, angle.degrees());

  angle.setMas(intData[1]);
  SET_CARMA_FLOAT_VALUE(elHighSwLimitVal_, angle.degrees());
  SET_CARMA_FLOAT_VALUE(elHighHwLimitVal_, angle.degrees());

  //------------------------------------------------------------
  // LO monitor points
  //------------------------------------------------------------

  Frequency freq;

  freq.setMHz(10.0);
  SET_CARMA_DOUBLE_VALUE(oscFreq_, freq.GHz());

  unsigned short ushortData[3];
  GET_SZA_DATA(yigFreq_, ushortData);  
  freq.setMHz(ushortData[0]);
  SET_CARMA_DOUBLE_VALUE(yigFreq_, freq.GHz());

  short shortData[3];
  GET_SZA_DATA(yigIFLevel_, shortData);
  SET_CARMA_DOUBLE_VALUE(yigIFLevel_, (double)(shortData[0])/1000); // Convert from mV to V

  GET_SZA_DATA(yigError_, shortData);
  SET_CARMA_DOUBLE_VALUE(yigError_, (double)(shortData[0])/1000);  // Convert from mV to V

  // Write the yig state

  GET_SZA_DATA(yigState_, ucharData);

  switch (ucharData[0]) {
  case sza::antenna::canbus::Yig::UNLOCKED:
    SET_CARMA_ENUM_VALUE(yigState_, AntennaCommon::YigStateMonitorPointEnum::FAILED);
    SET_CARMA_ENUM_VALUE(loState_,  AntennaCommon::LoStateMonitorPointEnum::FAILED);
    break;
  case sza::antenna::canbus::Yig::SEARCHING:
    SET_CARMA_ENUM_VALUE(yigState_, AntennaCommon::YigStateMonitorPointEnum::SEARCH);
    SET_CARMA_ENUM_VALUE(loState_,  AntennaCommon::LoStateMonitorPointEnum::SEARCH);
    break;
  case sza::antenna::canbus::Yig::REFINING:
    SET_CARMA_ENUM_VALUE(yigState_, AntennaCommon::YigStateMonitorPointEnum::OPTIMIZING);
    SET_CARMA_ENUM_VALUE(loState_,  AntennaCommon::LoStateMonitorPointEnum::OPTIMIZING);
    break;
  default:
    {
      SET_CARMA_ENUM_VALUE(yigState_, AntennaCommon::YigStateMonitorPointEnum::LOCK);
      
      // If the Yig is locked, we can dependably set the YIG state to
      // locked, but we need to separately check if the high-frequency
      // oscillator is also locked.  Which one we need to check
      // depends on the frequency.

      switch(rxId) {
      case sza::util::Rx::RX1CM:
	{
	  unsigned char uChar;
	  GET_SZA_DATA(varLockStatus_, &uChar);
	  bool locked = (bool)uChar;
	  SET_CARMA_ENUM_VALUE(loState_,  locked ? AntennaCommon::LoStateMonitorPointEnum::LOCK :
			       AntennaCommon::LoStateMonitorPointEnum::FAILED);
	}
	break;
      case sza::util::Rx::RX3MM:
	{
	  unsigned char uChar;
	  GET_SZA_DATA(biasLockStatus_, &uChar);
	  unsigned int state = (int)uChar;
	  switch(state) {
	  case 0:
	    SET_CARMA_ENUM_VALUE(loState_, AntennaCommon::LoStateMonitorPointEnum::FAILED);
	    break;
	  case 5:
	  case 6:
	    SET_CARMA_ENUM_VALUE(loState_, AntennaCommon::LoStateMonitorPointEnum::OPTIMIZING);
	    break;
	  case 7:
	    SET_CARMA_ENUM_VALUE(loState_, AntennaCommon::LoStateMonitorPointEnum::LOCK);
	    break;
	  default:
	    SET_CARMA_ENUM_VALUE(loState_, AntennaCommon::LoStateMonitorPointEnum::SEARCH);
	    break;
	  }
	}
	break;
      default:
	SET_CARMA_ENUM_VALUE(loState_, AntennaCommon::LoStateMonitorPointEnum::FAILED);
	break;
      }
    }      
    break;
  }

  // State of the LO sweep 

  GET_SZA_DATA(yigSweep_, ucharData);
  SET_CARMA_ENUM_VALUE(yigSweep_, ucharData[0]==0 ? 
		       AntennaCommon::YigSweepMonitorPointEnum::OFF : 
		       AntennaCommon::YigSweepMonitorPointEnum::ON);

  // Check the LO sweep status differently depending on frequency

  switch(rxId) {
  case sza::util::Rx::RX1CM:
    {
      unsigned char uChar;
      GET_SZA_DATA(varSweepEnabled_, &uChar);
      bool sweepOn = (bool)uChar;
      SET_CARMA_ENUM_VALUE(loSweep_, sweepOn ? 
			   AntennaCommon::LoSweepMonitorPointEnum::ON : 
			   AntennaCommon::LoSweepMonitorPointEnum::OFF);
    }
    break;
  case sza::util::Rx::RX3MM:
    {
      unsigned char uChar;
      GET_SZA_DATA(biasSweepStatus_, &uChar);
      bool sweepOn = (uChar>>0 & 0x1);
      SET_CARMA_ENUM_VALUE(loSweep_, sweepOn ? 
			   AntennaCommon::LoSweepMonitorPointEnum::ON : 
			   AntennaCommon::LoSweepMonitorPointEnum::OFF);
    }
    break;
  default:
    SET_CARMA_ENUM_VALUE(loSweep_, AntennaCommon::LoSweepMonitorPointEnum::OFF);
    break;
  }
 
  //------------------------------------------------------------
  // Receiver monitor points
  //------------------------------------------------------------

  freq = sza::util::Rx::getSkyFrequency(rxId);
  SET_CARMA_DOUBLE_VALUE(loFreq_, freq.GHz());

  switch(rxId) {
  case sza::util::Rx::RX1MM:
    SET_CARMA_ENUM_VALUE(currentRx_, 
			 AntennaCommon::CurrentRxMonitorPointEnum::RX1MM);
    break;
  case sza::util::Rx::RX3MM:
    SET_CARMA_ENUM_VALUE(currentRx_, 
			 AntennaCommon::CurrentRxMonitorPointEnum::RX3MM);
    break;
  default:
    SET_CARMA_ENUM_VALUE(currentRx_, 
			 AntennaCommon::CurrentRxMonitorPointEnum::RX1CM);
    break;
  }

  writeRxStateMonitorPoint();

  //  GET_MP(rxTsysState_,     "",        "",            antennaCommon().receivers().rxTsysState()); // No correspondent for SZA
  //  GET_MP(rxTsys_,          "",        "",            antennaCommon().receivers().rxTsys());      // No correspondent for SZA

  SET_CARMA_DOUBLE_VALUE(rxOffsetAz_, 0.0);
  SET_CARMA_DOUBLE_VALUE(rxOffsetEl_, 0.0);

  //  GET_MP(compressorState_, "",        "",            antennaCommon().receivers().compressorState()); // Not recorded for SZA

  GET_SZA_DATA(dewarTemp_, shortData);
  double tempInKelvin = ((double)shortData[1])/100;

  // Use whichever sensor is not returning bogus readings

  if(tempInKelvin < 3)
    tempInKelvin = ((double)shortData[2])/100;

  SET_CARMA_DOUBLE_VALUE(dewarTemp_, tempInKelvin);

  //------------------------------------------------------------
  // Location monitor points
  //------------------------------------------------------------

  GET_SZA_DATA(latitude_, intData);

  angle.setMas((double)intData[0]);
  SET_CARMA_DOUBLE_VALUE(longitude_, angle.degrees());

  angle.setMas((double)intData[1]);
  SET_CARMA_DOUBLE_VALUE(latitude_, angle.degrees());

  SET_CARMA_DOUBLE_VALUE(altitude_, ((double)intData[2])/1000); // Convert from mm to m

  //------------------------------------------------------------
  // Optical telescope monitor points
  //------------------------------------------------------------

  // GET_MP(sizeX_,          "", "", antennaCommon().opticalTel().sizeX());     // N/A for SZA     
  // GET_MP(sizeY_,          "", "", antennaCommon().opticalTel().sizeY());
  // GET_MP(offsetX_,        "", "", antennaCommon().opticalTel().offsetX());
  // GET_MP(offsetY_,        "", "", antennaCommon().opticalTel().offsetY());
  // GET_MP(azFov_,          "", "", antennaCommon().opticalTel().azFov());
  // GET_MP(elFov_,          "", "", antennaCommon().opticalTel().elFov());
  // GET_MP(imageRotation_,  "", "", antennaCommon().opticalTel().imageRotation());
  // GET_MP(cover_,          "", "", antennaCommon().opticalTel().cover());

  //------------------------------------------------------------
  // Optics monitor points
  //------------------------------------------------------------

  SET_CARMA_ENUM_VALUE(polarization_, AntennaCommon::PolarizationMonitorPointEnum::LCP);

  //------------------------------------------------------------
  // Calibrator monitor points
  //------------------------------------------------------------

  GET_SZA_DATA(calState_, ucharData);
  unsigned char calReq = ucharData[0];

  // Get the state of the load

  unsigned char calState;
  share_->readReg("caltert", "calibState", &calState);

  // Get the data validity flag

  share_->readReg("caltert", "dataValid", ucharData);
  bool calDataValid = (bool)ucharData[0];

  // If using new CalTert API (March 2011)

  if(newCaltert_) {

    // If the data are valid, this means (according to the API anyway)
    // that the reported state reflects the state for the last full
    // half-second frame.  If the calState is reporting SKY or AMB,
    // but dataValid is false, we will assume the load was moving.

    switch ((CalTertNew::TertState)calState) {
	
    case CalTertNew::CS_SKY:
      SET_CARMA_ENUM_VALUE(calState_, calDataValid ? 
			   AntennaCommon::CalStateMonitorPointEnum::SKY : 
			   AntennaCommon::CalStateMonitorPointEnum::MOVING);
      break;
    case CalTertNew::CS_AMBIENT:
      SET_CARMA_ENUM_VALUE(calState_, calDataValid ? 
			   AntennaCommon::CalStateMonitorPointEnum::AMB : 
			   AntennaCommon::CalStateMonitorPointEnum::MOVING);
      break;
    case CalTertNew::CS_MOVING:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::MOVING);
      break;
    default:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::ERROR);
      break;
    }

    // Else if using old CalTert API (March 2011)
    
  } else {
    switch ((CalTertOld::TertState)calState) {

    case CalTertOld::IN_POSITION:

    switch ((unsigned)calReq) {
    case 0:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::SKY);
      break;
    case 1:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::AMB);
      break;
    default:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::ERROR);
      break;
    }

    break;
    case CalTertOld::MOVING:
    case CalTertOld::HOMING:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::MOVING);
      break;
    default:
      SET_CARMA_ENUM_VALUE(calState_, AntennaCommon::CalStateMonitorPointEnum::ERROR);
      break;
    }
  }

  //------------------------------------------------------------
  // Set temperatures related to Tsys calculation
  //------------------------------------------------------------

  Temperature temp = share_->getAmbientTemperature();
  SET_CARMA_FLOAT_VALUE(skyTemp_, temp.K()*0.95);
  
  GET_SZA_DATA(ambTemp_, shortData);
  temp.setMilliKelvin(((double)shortData[0])*10);
  SET_CARMA_FLOAT_VALUE(ambTemp_, temp.K());

  temp.setK(15.0);
  SET_CARMA_FLOAT_VALUE(fixedTemp_,    temp.K()); // Fixed temp not set for SZA -- default to what?

  SET_CARMA_FLOAT_VALUE(partialTrans_, 50.0);     // No correspondent for SZA   -- default to 50%

  temp.setK(5.0);
  SET_CARMA_FLOAT_VALUE(spillOver_,    temp.K());  // Not measured online by SZA -- default to something realistic?

  //------------------------------------------------------------
  // Secondary monitor points
  //------------------------------------------------------------

  SET_CARMA_ENUM_VALUE(focusState_, AntennaCommon::FocusStateMonitorPointEnum::ACQUIRED);
  SET_CARMA_FLOAT_VALUE(focusZ_, 0.0); // Not recorded or settable with SZA

  // Now that we've buffered all our data, write the monitor points

  antennaMonitor_->antennaCommon().drive().driveSeqNum().setValue(42);
  antennaMonitor_->antennaCommon().drive().driveSeqNumSuccess().setValue(true);

  // Write sequence numbers that were staged earlier

  antennaMonitor_->antennaCommon().drive().driveSeqNum().setValue(driveSeqNo_.getSeq());
  antennaMonitor_->antennaCommon().drive().driveSeqNumSuccess().setValue(driveSeqNo_.getSuccess());

  antennaMonitor_->antennaCommon().receivers().tuneSeqNum().setValue(tuneSeqNo_.getSeq());

  antennaMonitor_->antennaCommon().calibrator().calSeqNum().setValue(calSeqNo_.getSeq());

  antennaMonitor_->antennaCommon().optics().opticsSeqNum().setValue(opticsSeqNo_.getSeq());

  antennaMonitor_->antennaCommon().opticalTel().centroidSeqNum().setValue(opticalTelSeqNo_.getSeq());

#endif
}

/**.......................................................................
 * Write values to the SZA-specific monitor points
 */
void CarmaMonitorPointHandler::stageSpecificMonitorPoints()
{
  // Do nothing if not initialized

  if(!initialized_)
    return;

#if DIR_USE_ANT_CORBA

  //============================================================
  // Antenna IF monitor points
  //============================================================
	  
  float atten;
  GET_SZA_DATA(totalPamAtten_, &atten);
  SET_CARMA_FLOAT_VALUE(totalPamAtten_, atten);

  GET_SZA_DATA(inputPamAtten_, &atten);
  SET_CARMA_FLOAT_VALUE(inputPamAtten_, atten);

  GET_SZA_DATA(outputPamAtten_, &atten);
  SET_CARMA_FLOAT_VALUE(outputPamAtten_, atten);

  float power;
  GET_SZA_DATA(ifTotalPower_, &power);
  SET_CARMA_FLOAT_VALUE(ifTotalPower_, power);

  //============================================================
  // Varactor monitor points
  //============================================================

  unsigned char uChar;
  GET_SZA_DATA(varLockStatus_, &uChar);
  bool locked = (uChar>>0 & 0x1);
  SET_CARMA_ENUM_VALUE(varLockStatus_, (locked ? carma::monitor::VaractorModule::LockStatusMonitorPointEnum::LOCKED : 
					carma::monitor::VaractorModule::LockStatusMonitorPointEnum::UNLOCKED));
  bool sweepOn = (uChar>>2 & 0x1);
  SET_CARMA_BOOL_VALUE(varSweepEnabled_, sweepOn);

  bool gunnOn = (uChar>>3 & 0x1);
  SET_CARMA_BOOL_VALUE(varGunnEnabled_, gunnOn);

  short sVal;
  unsigned short usVal;
  float fVal;

  GET_SZA_DATA(varNoiseMeter_,  &sVal);
  fVal = ((float)sVal)/1000;
  SET_CARMA_FLOAT_VALUE(varNoiseMeter_, fVal);

  GET_SZA_DATA(varIfLevel_,     &sVal);
  fVal = ((float)sVal)/1000;
  SET_CARMA_FLOAT_VALUE(varIfLevel_, fVal);

  GET_SZA_DATA(varLoopGain_,    &usVal);
  fVal = (float)usVal;
  SET_CARMA_FLOAT_VALUE(varLoopGain_, fVal);

  GET_SZA_DATA(varGunnCurrent_, &sVal);
  fVal = (float)sVal;
  SET_CARMA_FLOAT_VALUE(varGunnCurrent_, fVal);

#endif
}

/**.......................................................................
 * Set the value of all sza monitor points in the monitor stream (but
 * don't write them)
 */
void CarmaMonitorPointHandler::
stageSzaAllMonitorPoints()
{
#if 1
  TimeVal time;
  time.setToCurrentTime();
  RegDate date(time);
  share_->writeReg("tracker", "utc", date.data());
#endif

  //------------------------------------------------------------
  // Read the values of any controlling monitor points first
  //------------------------------------------------------------

  for(unsigned i=0; i < szaCanRcvdMonitorPoints_.size(); i++) {
    szaCanRcvdMonitorPoints_[i].read(share_);
  }

  //------------------------------------------------------------
  // Now iterate over staging all monitor points
  //------------------------------------------------------------

  for(unsigned i=0; i < szaSpecificMonitorPoints_.size(); i++) {
    CarmaMonitorPoint& mp = szaSpecificMonitorPoints_[i];

    // If there is no controlling monitor point, just pack this
    // monitor point

    if(!mp.szaControlMp_) {
      mp.packData();

      // Else only pack this monitor point if the control monitor
      // point allows it

    } else if(mp.szaControlMp_->isValid()) {
      mp.packData();
    }
  }
}

/**.......................................................................
 * Set the value of a carma sequence number in the monitor stream
 */
void CarmaMonitorPointHandler::
stageCarmaSeqNo(unsigned long seq, 
		sza::util::GenericTaskMsg::CarmaSeqNoType type, 
		bool success)
{
  // Do nothing if not initialized

  if(!initialized_)
    return;

#if DIR_USE_ANT_CORBA

  // Write to the monitor system

  if(antennaMonitor_) {

    switch (type) {
    case sza::util::GenericTaskMsg::DRIVE:
      driveSeqNo_.setSeq(seq, success);
      break;
    case sza::util::GenericTaskMsg::RX:
      CTOUT("Got a tuning sequence number message: seq = " << seq << " success = " << success);
      tuneSeqNo_.setSeq(seq, success);

      // And mark whatever tuning command led to this point as complete

      share_->setTuningPending(false);
      break;
    case sza::util::GenericTaskMsg::CAL:
      calSeqNo_.setSeq(seq, success);
      break;
    case sza::util::GenericTaskMsg::OPTICS:
      opticsSeqNo_.setSeq(seq, success);
      break;
    case sza::util::GenericTaskMsg::OPTICALTEL:
      opticalTelSeqNo_.setSeq(seq, success);
      break;
    default:
      break;
    }
  }
#endif  
}

/**.......................................................................
 * Write staged monitor points to the monitor stream
 */
void CarmaMonitorPointHandler::writeMonitorPoints()
{
  antennaMonitor_->writeWithoutResettingValidities();
}

/**.......................................................................
 * Populate the map of SZA blocks to CARMA monitor points
 */
void CarmaMonitorPointHandler::createMonitorPointMap()
{
#if DIR_USE_ANT_CORBA

  RegMap* regMap = share_->getRegMap();

  for(unsigned iBoard=0; iBoard < (unsigned)regMap->nboard_; iBoard++) {

    RegMapBoard* board = regMap->boards_[iBoard];

    for(unsigned iBlock=0; iBlock < (unsigned)board->nblock; iBlock++) {
      RegMapBlock* block = board->blocks[iBlock];
      monitorMap_[block] = CarmaMonitorMap::getMonitorPoint(antennaMonitor_, board->name, block->name_);
    }
  }

#endif
}

void CarmaMonitorPointHandler::
packData(RegMapBlock* blk, void* data, 
	 sza::util::CoordRange* coordRange, sza::util::DataType::Type type)
{
#if DIR_USE_ANT_CORBA

  std::map<RegMapBlock*,carma::monitor::MonitorPoint*>::iterator iter = monitorMap_.find(blk);

  // If a CARMA monitor point exists for this register, write it now

  if(iter != monitorMap_.end()) {

    carma::monitor::MonitorPoint* mp = iter->second;
  
    if(mp != 0) {

      // If the number of bytes was passed as 0, use the default from the
      // block descriptor
      
      AxisRange range(blk->axes_, coordRange);
      
      switch(type) {

      case DataType::UCHAR:
	{
	  unsigned char* dptr = (unsigned char*)data;
	  for(range.reset(); !range.isEnd(); ++range) 
	    ((carma::monitor::MonitorPointByte*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::CHAR:
	{
	  char* dptr = (char*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointChar*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::BOOL:
	{
	  bool* dptr = (bool*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointBool*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;

	// Unsigned short gets upcast to an int, since CARMA doesn't support unsigned types

      case DataType::USHORT:
	{
	  unsigned short* dptr = (unsigned short*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointInt*)mp)->setValue((int)dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::SHORT:
	{
	  short* dptr = (short*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointShort*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;

	// Unsigned int gets upcast to a double, since CARMA doesn't
	// support unsigned types. 

      case DataType::UINT:
	{
	  unsigned int* dptr = (unsigned int*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::INT:
	{
	  int* dptr = (int*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointInt*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;

	// Unsigned int gets upcast to a double, since CARMA doesn't
	// support unsigned types. 

      case DataType::ULONG:
	{
	  unsigned long* dptr = (unsigned long*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)dptr[range.currentIterator()], range.currentElement());
	}
	break;

	// Long gets upcast to a double, since CARMA doesn't support longs

      case DataType::LONG:
	{
	  long* dptr = (long*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::FLOAT:
	{
	  float* dptr = (float*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointFloat*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::DOUBLE:
	{
	  double* dptr = (double*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue(dptr[range.currentIterator()], range.currentElement());
	}
	break;
      case DataType::DATE:
	{
	  RegDate date;
	  RegDate::Data* dptr = (RegDate::Data*)data;
	  for(range.reset(); !range.isEnd(); ++range) {
	    date = dptr[range.currentIterator()];
	    ((carma::monitor::MonitorPointAbstime*)mp)->setValue(date.mjd(), range.currentElement());
	  }
	}
	break;
      default:
	break;
      }
    }
  }
#endif
}

void CarmaMonitorPointHandler::
packValue(RegMapBlock* blk, void* data, 
	  sza::util::CoordRange* coordRange, sza::util::DataType::Type type)
{
#if DIR_USE_ANT_CORBA

  std::map<RegMapBlock*,carma::monitor::MonitorPoint*>::iterator iter = monitorMap_.find(blk);

  // If a CARMA monitor point exists for this register, write it now

  if(iter != monitorMap_.end()) {

    carma::monitor::MonitorPoint* mp = iter->second;
  
    if(mp != 0) {

      // If the number of bytes was passed as 0, use the default from the
      // block descriptor
      
      AxisRange range(blk->axes_, coordRange);
      
      switch(type) {

      case DataType::UCHAR:
	{
	  unsigned char* dptr = (unsigned char*)data;
	  for(range.reset(); !range.isEnd(); ++range) 
	    ((carma::monitor::MonitorPointByte*)mp)->setValue(*dptr, range.currentElement());
	}
	break;
      case DataType::CHAR:
	{
	  char* dptr = (char*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointChar*)mp)->setValue(*dptr, range.currentElement());
	}
	break;
      case DataType::BOOL:
	{
	  bool* dptr = (bool*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointBool*)mp)->setValue(*dptr, range.currentElement());
	}
	break;

	// Unsigned short gets upcast to an int, since CARMA doesn't support unsigned types

      case DataType::USHORT:
	{
	  unsigned short* dptr = (unsigned short*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointInt*)mp)->setValue((int)*dptr, range.currentElement());
	}
	break;
      case DataType::SHORT:
	{
	  short* dptr = (short*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointShort*)mp)->setValue(*dptr, range.currentElement());
	}
	break;

	// Unsigned int gets upcast to a long, since CARMA doesn't
	// support unsigned types.  If sizeof(long)==sizeof(int), oh
	// well.

      case DataType::UINT:
	{
	  unsigned int* dptr = (unsigned int*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)*dptr, range.currentElement());
	}
	break;
      case DataType::INT:
	{
	  int* dptr = (int*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointInt*)mp)->setValue(*dptr, range.currentElement());
	}
	break;
      case DataType::ULONG:
	{
	  unsigned long* dptr = (unsigned long*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)*dptr, range.currentElement());
	}
	break;
      case DataType::LONG:
	{
	  long* dptr = (long*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue((double)*dptr, range.currentElement());
	}
	break;
      case DataType::FLOAT:
	{
	  float* dptr = (float*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointFloat*)mp)->setValue(*dptr, range.currentElement());
	}
	break;
      case DataType::DOUBLE:
	{
	  double* dptr = (double*)data;
	  for(range.reset(); !range.isEnd(); ++range)
	    ((carma::monitor::MonitorPointDouble*)mp)->setValue(*dptr, range.currentElement());
	}
	break;
      case DataType::DATE:
	{
	  RegDate date;
	  RegDate::Data* dptr = (RegDate::Data*)data;
	  for(range.reset(); !range.isEnd(); ++range) {
	    date = *dptr;
	    ((carma::monitor::MonitorPointAbstime*)mp)->setValue(date.mjd(), range.currentElement());
	  }
	}
	break;
      default:
	break;
      }
    }
  }
#endif
}

/**.......................................................................
 * Write tracking state monitor points.  This requires some screwing
 * around to transform from SZA monitor points to CARMA monitor
 * points.
 */
void CarmaMonitorPointHandler::writeTrackingStateMonitorPoints()
{
  // Get all the registers we need from the pmac and tracker boards

  unsigned int pmacDriveStatus;
  share_->readReg("pmac", "drive_status", &pmacDriveStatus);

  bool acquired = (pmacDriveStatus>>2 & 0x1);
  bool running  = (pmacDriveStatus>>0 & 0x1);
  bool pmacError= (pmacDriveStatus>>1 & 0x1);

  unsigned int pmacAxisStat;
  share_->readReg("pmac", "axis_stat", &pmacAxisStat);

  bool local    = (pmacAxisStat>>9 & 0x1);
  bool limitSet = (pmacAxisStat & 0xf000);
  bool fatalErr = (pmacAxisStat>>6 & 0x1);

  // Needed to defeat the bogus reading on the platform E-stop for ant7.
  // If the E-stop is actually engaged, the PMAC program will report
  // that it is stopped.

  bool eStopOn  = ((pmacAxisStat>>22 & 0x1) || (pmacAxisStat>>23 & 0x1)) && !running;

  unsigned char trackerState;
  share_->readReg("tracker", "state", &trackerState);

  bool lacking  = (trackerState==0);
  bool timeErr  = (trackerState==1);
  bool updating = (trackerState==2);
  bool tracking = (trackerState==5);
  bool tooLow   = (trackerState==6);
  bool tooHigh  = (trackerState==7);

  // See if the telescope is in an error condition

  bool error = pmacError || timeErr;

  // Increment the number of frames 

  share_->incrementFrame(acquired);
  
  // Extract the needed information about the last requested position
  
  SzaShareCorba::Position position;
  unsigned nFrame;
  bool pending;
  
  share_->getPosition(position, nFrame, pending);

  // Here we go.  First check for exceptional conditions

  if(fatalErr) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::FATAL);
  } else if(error) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::ERROR);
  } else if(limitSet) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::HWLIMIT);
  } else if(local) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::LOCAL);
  } else if(eStopOn) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::DISABLE);
  } else if(tooHigh || tooLow) {
    SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::SWLIMIT);

    // If the telescope is slewing

  } else if(updating) {

    // then if the last position is still pending, we are just slewing to position

    if(pending) {
      SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::SLEW);

      // Else we were tracking, but have gone into slew mode for some reason
    } else {
      SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::CLOSE);
    }

  } else if(tracking) {

    // If we are tracking, but the number of frames for which
    // acquired==true is less than 2, then we are close

    if(nFrame < 2) {
      SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::CLOSE);

      // Else we are tracking. 

    } else {

      // Are we tracking a known position?

      switch (position) {
      case SzaShareCorba::SERVICE:
	SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::SERVICE);
	break;
      case SzaShareCorba::STOW:
	SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::STOW);
	break;
      case SzaShareCorba::SNOW:
	SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::SNOW);
	break;
      case SzaShareCorba::AZEL:
	SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::STOP);
	break;
      default:
	SET_CARMA_ENUM_VALUE(state_, carma::monitor::AntennaCommon::StateMonitorPointEnum::TRACK);
	break;
      }

    }
  }

  //-----------------------------------------------------------------------
  // Lastly, set the mode, which is independent of the state monitor point
  //-----------------------------------------------------------------------

  switch (position) {
  case SzaShareCorba::AZEL:
  case SzaShareCorba::SERVICE:
    SET_CARMA_ENUM_VALUE(mode_, carma::monitor::AntennaCommon::ModeMonitorPointEnum::AZEL);
    break;
  case SzaShareCorba::STOW:
    SET_CARMA_ENUM_VALUE(mode_, carma::monitor::AntennaCommon::ModeMonitorPointEnum::STOW);
    break;
  case SzaShareCorba::SNOW:
    SET_CARMA_ENUM_VALUE(mode_, carma::monitor::AntennaCommon::ModeMonitorPointEnum::SNOW);
    break;
  case SzaShareCorba::EQUAT:
    SET_CARMA_ENUM_VALUE(mode_, carma::monitor::AntennaCommon::ModeMonitorPointEnum::EQUAT);
    break;
  default:
    break;
  }
}

void CarmaMonitorPointHandler::writeRxStateMonitorPoint()
{
  bool tuningPending;
  bool yigLocked;
  bool varactorError;
  bool btgError;
  bool gunnError;
  bool rxBiased;
  bool tertiaryInPosition;
  bool tertPosError;

  sza::util::Rx::Id rxId;
  share_->getRx(rxId);

  tuningPending = share_->getTuningPending();

  unsigned char yigState;
  GET_SZA_DATA(yigState_, &yigState);
  yigLocked = (yigState==sza::antenna::canbus::Yig::LOCKED);

  share_->readReg("varactor", "lockStatusError", &varactorError);
  share_->readReg("bias",     "lockStatusError", &btgError);

  gunnError = varactorError || btgError;
  
  short drainCurrent30GHz;
  share_->readReg("rx", "drainCurrent30GHz", &drainCurrent30GHz);

  if(rxId == sza::util::Rx::RX1CM)
    rxBiased = (drainCurrent30GHz > 1);
  else
    rxBiased = true;

  unsigned char tertState;

  // If this is the new caltert, all positional information is already
  // contained in tertPosError below

  if(newCaltert_) {
    tertiaryInPosition = true;
  } else {
    share_->readReg("caltert", "tertState", &tertState);
    tertiaryInPosition = (tertState==0);
  }

  share_->readReg("caltert", "tertPosError", &tertPosError);

  // If tuning is pending, we are still tuning

  if(tuningPending) {
    SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::TUNE);
  } else {

    // If the YIG is unlocked, flag it as YIG_BAD
      
    if(!yigLocked) {

      SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::YIG_BAD);

      // Else if the varactor or btg is unlocked, flag it as GUNN_BAD

    } else if(gunnError) {

      SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::GUNN_BAD);

      // Else if the receiver failed to bias

    } else if(!rxBiased) {

      SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::RX_BAD);

      // Finally check the tertiary

    } else if(!tertiaryInPosition || tertPosError) {

      SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::TERTIARY_BAD);

      // Else tuning is done, and everything looks good

    } else {
      SET_CARMA_ENUM_VALUE(rxState_, carma::monitor::AntennaCommon::RxStateMonitorPointEnum::GOOD);
    }
  }
}

#if DIR_USE_ANT_CORBA
void CarmaMonitorPointHandler::CarmaMonitorPoint::packData()
{
  if(packFn_) {
    (*packFn_)(*this);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packBool)
{
  unsigned n = cmp.szaBlock_->nEl();
  bool bval[n];

  GET_SZA_DATA_EXT(cmp, bval);

  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_BOOL_VALUE_IDX(cmp, bval[i], i);
    }
  } else {
    SET_CARMA_BOOL_VALUE(cmp, bval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packChar)
{
  unsigned n = cmp.szaBlock_->nEl();
  signed char cval[n];

  GET_SZA_DATA_EXT(cmp, (signed char*)cval);
  
  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_CHAR_VALUE_IDX(cmp, cval[i], i);
    } 
  } else {
    SET_CARMA_CHAR_VALUE(cmp, cval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packUchar)
{
  unsigned n = cmp.szaBlock_->nEl();
  unsigned char ucval[n];

  GET_SZA_DATA_EXT(cmp, (unsigned char*)ucval);

  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_SHORT_VALUE_IDX(cmp, (short)ucval[i], i);
    }
  } else {
    SET_CARMA_SHORT_VALUE(cmp, (short)ucval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packShort)
{
  unsigned n = cmp.szaBlock_->nEl();
  short sval[n];

  GET_SZA_DATA_EXT(cmp, sval);

  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_SHORT_VALUE_IDX(cmp, sval[i], i);
    }
  } else {
    SET_CARMA_SHORT_VALUE(cmp, sval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packUshort)
{
  unsigned n = cmp.szaBlock_->nEl();
  unsigned short usval[n];

  GET_SZA_DATA_EXT(cmp, usval);

  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_INT_VALUE_IDX(cmp, (int)usval[i], i);
    }
  } else {
    SET_CARMA_INT_VALUE(cmp, (int)usval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packInt)
{
  unsigned n = cmp.szaBlock_->nEl();
  int ival[n];

  GET_SZA_DATA_EXT(cmp, ival);
  
  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_INT_VALUE_IDX(cmp, ival[i], i);
    }
  } else {
    SET_CARMA_INT_VALUE(cmp, ival[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packUint)
{
  unsigned n = cmp.szaBlock_->nEl();
  unsigned int uival[n];

  GET_SZA_DATA_EXT(cmp, uival);
  
  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_INT_VALUE_IDX(cmp, (int)uival[i], i);
    }
  } else {
    SET_CARMA_INT_VALUE(cmp, (int)uival[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packFloat)
{
  unsigned n = cmp.szaBlock_->nEl();
  float fval[n];

  GET_SZA_DATA_EXT(cmp, fval);
  
  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_FLOAT_VALUE_IDX(cmp, fval[i], i);
    }
  } else {
    SET_CARMA_FLOAT_VALUE(cmp, fval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packDouble)
{
  unsigned n = cmp.szaBlock_->nEl();
  double dval[n];

  GET_SZA_DATA_EXT(cmp, dval);
  
  if(n > 1) {
    for(unsigned i=0; i < n; i++) {
      SET_CARMA_DOUBLE_VALUE_IDX(cmp, dval[i], i);
    }
  } else {
    SET_CARMA_DOUBLE_VALUE(cmp, dval[0]);
  }
}

MP_PACK_FN(CarmaMonitorPointHandler::packString)
{
  std::string str;
  str.resize(cmp.szaBlock_->nEl() + 1);

  GET_SZA_DATA_EXT(cmp, (unsigned char*)&str[0]);
  SET_CARMA_STRING_VALUE(cmp, str);
}

MP_PACK_FN(CarmaMonitorPointHandler::packUtc)
{
  RegDate date;
  GET_SZA_DATA_EXT(cmp, date.data());
  SET_CARMA_DOUBLE_VALUE(cmp, date.mjd());
}

void CarmaMonitorPointHandler::extractSzaCanRcvdMonitorPoints()
{
  RegMapBlock* block = share_->getReg("bias",      "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "bias"));

  block = share_->getReg("caltert",   "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "caltert"));

  block = share_->getReg("ifmod",     "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "ifmod"));

  block = share_->getReg("intmod",    "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "intmod"));

  block = share_->getReg("rx",        "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "rx"));

  block = share_->getReg("thermal",   "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "thermal"));

  block = share_->getReg("tiltmeter", "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "tiltmeter"));

  block = share_->getReg("varactor",  "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "varactor"));

  block = share_->getReg("yig",       "received");
  szaCanRcvdMonitorPoints_.push_back(SzaCanReceivedMonitorPoint(block, "yig"));
}

void CarmaMonitorPointHandler::extractAllSzaMonitorPoints()
{
  // Instantiate the object that will return the correct CARMA monitor
  // point for each SZA register block

  CarmaMonitorPointTranslator mapper(antennaMonitor_);

  // Now iterate through the whole register map, extracting SZA
  // register blocks from the shared memory map, and the carma regs
  // that correspond to them

  SzaRegMap*  regmap = 0;
  regmap = new_SzaAntRegMap();

  if(regmap == 0) {
    ThrowError("Unable to allocate antenna register map");
  }

  szaSpecificMonitorPoints_.resize(regmap->nreg_);

  for(unsigned iReg=0, iBoard=0; iBoard < regmap->nboard_; iBoard++) {
    RegMapBoard* board = regmap->boards_[iBoard];

    for(unsigned iBlock=0; iBlock < board->nblock; iBlock++, iReg++) {
      RegMapBlock* block = board->blocks[iBlock];

      CarmaMonitorPoint& mp = szaSpecificMonitorPoints_[iReg];

      mp.szaBlock_ = share_->getReg(board->name, block->name_);

      mp.szaControlMp_ = getControlMp(board->name, block->name_);

      unsigned nEl = block->nEl();

      if(nEl > 1 && !block->isString()) {
	mp.carmaMpVec_.resize(nEl);
	for(unsigned iEl=0; iEl < nEl; iEl++) {
	  mp.carmaMpVec_[iEl] = &mapper.getMonitorPoint(board->name, block->name_, iEl);
	}
	
      } else {
	mp.carmaMp_ = &mapper.getMonitorPoint(board->name, block->name_);
      }

      mp.share_    = share_;

      assignMpPackFn(mp);

    }
  }

  if(regmap) {
    regmap = del_SzaAntRegMap(regmap);
  }

}

void CarmaMonitorPointHandler::assignMpPackFn(CarmaMonitorPoint& mp)
{
  if(mp.szaBlock_->isString()) {
    mp.packFn_ = CarmaMonitorPointHandler::packString;
  } else if(mp.szaBlock_->isBool()) {
    mp.packFn_ = CarmaMonitorPointHandler::packBool;
  } else if(mp.szaBlock_->isUchar()) {
    mp.packFn_ = CarmaMonitorPointHandler::packUchar;
  } else if(mp.szaBlock_->isChar()) {
    mp.packFn_ = CarmaMonitorPointHandler::packChar;
  } else if(mp.szaBlock_->isUshort()) {
    mp.packFn_ = CarmaMonitorPointHandler::packUshort;
  } else if(mp.szaBlock_->isShort()) {
    mp.packFn_ = CarmaMonitorPointHandler::packShort;
  } else if(mp.szaBlock_->isUint()) {
    mp.packFn_ = CarmaMonitorPointHandler::packUint;
  } else if(mp.szaBlock_->isInt()) {
    mp.packFn_ = CarmaMonitorPointHandler::packInt;
  } else if(mp.szaBlock_->isFloat()) {
    mp.packFn_ = CarmaMonitorPointHandler::packFloat;
  } else if(mp.szaBlock_->isDouble()) {
    mp.packFn_ = CarmaMonitorPointHandler::packDouble;
  } else if(mp.szaBlock_->isUtc()) {
    mp.packFn_ = CarmaMonitorPointHandler::packUtc;
  }
}

/**.......................................................................
 * Return the control monitor point corresponding to the named board
 */
SzaCanReceivedMonitorPoint* CarmaMonitorPointHandler::getControlMp(std::string boardName, std::string blockName)
{
  for(unsigned iReg=0; iReg < szaCanRcvdMonitorPoints_.size(); iReg++) {
    SzaCanReceivedMonitorPoint& mp = szaCanRcvdMonitorPoints_[iReg];

    // Don't control the writing of the "received" monitor point; we
    // want the "received" monitor point always to be written, to
    // correctly reflect the state of the device, indpendent of the
    // RTD ? mechanism.

    if(boardName == mp.boardName_ && blockName != "received") {
      return &mp;
    }
  }

  // No match found?  Return null (no controlling monitor point)

  return 0;
}

void SzaCanReceivedMonitorPoint::read(SzaShareCorba* share) 
{
  if(block_) {
    unsigned short rcvd;
    share->readReg(block_, &rcvd);
    valid_ = (rcvd != 0x0);
  }
}

void SzaCanReceivedMonitorPoint::invalidate(SzaShareCorba* share)
{
  if(block_) {
    unsigned short rcvd = 0x0;
    share->writeReg(block_, &rcvd);
    valid_ = false;
  }
}

#endif
