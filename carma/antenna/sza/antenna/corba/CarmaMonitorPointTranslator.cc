#include "carma/antenna/sza/antenna/corba/CarmaMonitorPointTranslator.h"
#include "carma/szautil/Exception.h"

using namespace sza::antenna::corba;
using namespace sza::util;

#define FRAME_SUBSYSTEM(mpFn) base_->frame().mpFn()
#define FRAME_SUBSYSTEM_IDX(mpFn, iReg) base_->frame().mpFn(iReg)
#define BIAS_SUBSYSTEM(mpFn) base_->bias().mpFn()
#define BIAS_SUBSYSTEM_IDX(mpFn, iReg) base_->bias().mpFn(iReg)
#define CALTERT_SUBSYSTEM(mpFn) base_->caltert().mpFn()
#define CALTERT_SUBSYSTEM_IDX(mpFn, iReg) base_->caltert().mpFn(iReg)
#define IFMOD_SUBSYSTEM(mpFn) base_->ifmod().mpFn()
#define IFMOD_SUBSYSTEM_IDX(mpFn, iReg) base_->ifmod().mpFn(iReg)
#define INTMOD_SUBSYSTEM(mpFn) base_->intmod().mpFn()
#define INTMOD_SUBSYSTEM_IDX(mpFn, iReg) base_->intmod().mpFn(iReg)
#define PMAC_SUBSYSTEM(mpFn) base_->pmac().mpFn()
#define PMAC_SUBSYSTEM_IDX(mpFn, iReg) base_->pmac().mpFn(iReg)
#define RX_SUBSYSTEM(mpFn) base_->rx().mpFn()
#define RX_SUBSYSTEM_IDX(mpFn, iReg) base_->rx().mpFn(iReg)
#define THERMAL_SUBSYSTEM(mpFn) base_->thermal().mpFn()
#define THERMAL_SUBSYSTEM_IDX(mpFn, iReg) base_->thermal().mpFn(iReg)
#define TILTMETER_SUBSYSTEM(mpFn) base_->tiltmeter().mpFn()
#define TILTMETER_SUBSYSTEM_IDX(mpFn, iReg) base_->tiltmeter().mpFn(iReg)
#define TRACKER_SUBSYSTEM(mpFn) base_->tracker().mpFn()
#define TRACKER_SUBSYSTEM_IDX(mpFn, iReg) base_->tracker().mpFn(iReg)
#define VARACTOR_SUBSYSTEM(mpFn) base_->varactor().mpFn()
#define VARACTOR_SUBSYSTEM_IDX(mpFn, iReg) base_->varactor().mpFn(iReg)
#define YIG_SUBSYSTEM(mpFn) base_->yig().mpFn()
#define YIG_SUBSYSTEM_IDX(mpFn, iReg) base_->yig().mpFn(iReg)

CarmaMonitorPointTranslator::CarmaMonitorPointTranslator(carma::monitor::SzaSubsystem* base)
{
  base_  = base;
}

carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getMonitorPoint(std::string boardName, std::string regName, int iReg)
{
  if(boardName=="frame") {
    return getFrameMonitorPoint(regName, iReg);
  } else if(boardName=="bias") {
    return getBiasMonitorPoint(regName, iReg);
  } else if(boardName=="caltert") {
    return getCaltertMonitorPoint(regName, iReg);
  } else if(boardName=="ifmod") {
    return getIfmodMonitorPoint(regName, iReg);
  } else if(boardName=="intmod") {
    return getIntmodMonitorPoint(regName, iReg);
  } else if(boardName=="pmac") {
    return getPmacMonitorPoint(regName, iReg);
  } else if(boardName=="rx") {
    return getRxMonitorPoint(regName, iReg);
  } else if(boardName=="thermal") {
    return getThermalMonitorPoint(regName, iReg);
  } else if(boardName=="tiltmeter") {
    return getTiltmeterMonitorPoint(regName, iReg);
  } else if(boardName=="tracker") {
    return getTrackerMonitorPoint(regName, iReg);
  } else if(boardName=="varactor") {
    return getVaractorMonitorPoint(regName, iReg);
  } else if(boardName=="yig") {
    return getYigMonitorPoint(regName, iReg);
  } else {
    ThrowError("Unrecognized board name: " << boardName);
  }
}

carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getFrameMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return FRAME_SUBSYSTEM(status);
  } else if(regName == "received") {
    return FRAME_SUBSYSTEM(received);
  } else if(regName == "nsnap") {
    return FRAME_SUBSYSTEM(nsnap);
  } else if(regName == "record") {
    return FRAME_SUBSYSTEM(record);
  } else if(regName == "utc") {
    return FRAME_SUBSYSTEM(utc);
  } else if(regName == "lst") {
    return FRAME_SUBSYSTEM(lst);
  } else if(regName == "features") {
    return FRAME_SUBSYSTEM(features);
  } else if(regName == "markSeq") {
    return FRAME_SUBSYSTEM(markSeq);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getBiasMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return BIAS_SUBSYSTEM(status);
  } else if(regName == "received") {
    return BIAS_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return BIAS_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return BIAS_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return BIAS_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return BIAS_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return BIAS_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return BIAS_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return BIAS_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return BIAS_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return BIAS_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return BIAS_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return BIAS_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return BIAS_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return BIAS_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return BIAS_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return BIAS_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return BIAS_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return BIAS_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return BIAS_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return BIAS_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return BIAS_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return BIAS_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return BIAS_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return BIAS_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return BIAS_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return BIAS_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return BIAS_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return BIAS_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return BIAS_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return BIAS_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return BIAS_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return BIAS_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return BIAS_SUBSYSTEM(buildTime);
  } else if(regName == "phaseLockState") {
    return BIAS_SUBSYSTEM(phaseLockState);
  } else if(regName == "hwLockStatus") {
    return BIAS_SUBSYSTEM(hwLockStatus);
  } else if(regName == "refLockStatus") {
    return BIAS_SUBSYSTEM(refLockStatus);
  } else if(regName == "sweepStatus") {
    return BIAS_SUBSYSTEM(sweepStatus);
  } else if(regName == "gunnStatus") {
    return BIAS_SUBSYSTEM(gunnStatus);
  } else if(regName == "dataValid") {
    return BIAS_SUBSYSTEM(dataValid);
  } else if(regName == "autoRelock") {
    return BIAS_SUBSYSTEM(autoRelock);
  } else if(regName == "relockCount") {
    return BIAS_SUBSYSTEM(relockCount);
  } else if(regName == "gunnId") {
    return BIAS_SUBSYSTEM(gunnId);
  } else if(regName == "gunnVoltage") {
    return BIAS_SUBSYSTEM(gunnVoltage);
  } else if(regName == "multiplier") {
    return BIAS_SUBSYSTEM(multiplier);
  } else if(regName == "freqRangeCheck") {
    return BIAS_SUBSYSTEM(freqRangeCheck);
  } else if(regName == "ifMonState") {
    return BIAS_SUBSYSTEM(ifMonState);
  } else if(regName == "calTableState") {
    return BIAS_SUBSYSTEM(calTableState);
  } else if(regName == "calMonth") {
    return BIAS_SUBSYSTEM(calMonth);
  } else if(regName == "calDay") {
    return BIAS_SUBSYSTEM(calDay);
  } else if(regName == "calYear") {
    return BIAS_SUBSYSTEM(calYear);
  } else if(regName == "calDate") {
    return BIAS_SUBSYSTEM(calDate);
  } else if(regName == "numZabers") {
    return BIAS_SUBSYSTEM(numZabers);
  } else if(regName == "allZabers") {
    return BIAS_SUBSYSTEM(allZabers);
  } else if(regName == "gunnFrequency") {
    return BIAS_SUBSYSTEM(gunnFrequency);
  } else if(regName == "loopGain") {
    return BIAS_SUBSYSTEM(loopGain);
  } else if(regName == "tunerPosition") {
    return BIAS_SUBSYSTEM(tunerPosition);
  } else if(regName == "backShortPosition") {
    return BIAS_SUBSYSTEM(backShortPosition);
  } else if(regName == "attenuatorPosition") {
    return BIAS_SUBSYSTEM(attenuatorPosition);
  } else if(regName == "ifLevel") {
    return BIAS_SUBSYSTEM(ifLevel);
  } else if(regName == "maxChnl") {
    return BIAS_SUBSYSTEM_IDX(maxChnl, iReg);
  } else if(regName == "errorVoltage") {
    return BIAS_SUBSYSTEM(errorVoltage);
  } else if(regName == "gunnCurrent") {
    return BIAS_SUBSYSTEM(gunnCurrent);
  } else if(regName == "noiseMeterVoltage") {
    return BIAS_SUBSYSTEM(noiseMeterVoltage);
  } else if(regName == "boardTemperature") {
    return BIAS_SUBSYSTEM(boardTemperature);
  } else if(regName == "pos24VAnalogVoltage") {
    return BIAS_SUBSYSTEM(pos24VAnalogVoltage);
  } else if(regName == "pos5VDigitalVoltage") {
    return BIAS_SUBSYSTEM(pos5VDigitalVoltage);
  } else if(regName == "pos15VAnalogVoltage") {
    return BIAS_SUBSYSTEM(pos15VAnalogVoltage);
  } else if(regName == "pos12VAnalogVoltage") {
    return BIAS_SUBSYSTEM(pos12VAnalogVoltage);
  } else if(regName == "pos5VAnalogVoltage") {
    return BIAS_SUBSYSTEM(pos5VAnalogVoltage);
  } else if(regName == "neg12VAnalogVoltage") {
    return BIAS_SUBSYSTEM(neg12VAnalogVoltage);
  } else if(regName == "pos6VAnalogVoltage") {
    return BIAS_SUBSYSTEM(pos6VAnalogVoltage);
  } else if(regName == "crowbarState") {
    return BIAS_SUBSYSTEM(crowbarState);
  } else if(regName == "crowbarCount") {
    return BIAS_SUBSYSTEM(crowbarCount);
  } else if(regName == "lockStatusError") {
    return BIAS_SUBSYSTEM(lockStatusError);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getCaltertMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return CALTERT_SUBSYSTEM(status);
  } else if(regName == "received") {
    return CALTERT_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return CALTERT_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return CALTERT_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return CALTERT_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return CALTERT_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return CALTERT_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return CALTERT_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return CALTERT_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return CALTERT_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return CALTERT_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return CALTERT_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return CALTERT_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return CALTERT_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return CALTERT_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return CALTERT_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return CALTERT_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return CALTERT_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return CALTERT_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return CALTERT_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return CALTERT_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return CALTERT_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return CALTERT_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return CALTERT_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return CALTERT_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return CALTERT_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return CALTERT_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return CALTERT_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return CALTERT_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return CALTERT_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return CALTERT_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return CALTERT_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return CALTERT_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return CALTERT_SUBSYSTEM(buildTime);
  } else if(regName == "tertState") {
    return CALTERT_SUBSYSTEM(tertState);
  } else if(regName == "moveMirOk") {
    return CALTERT_SUBSYSTEM(moveMirOk);
  } else if(regName == "posnCode") {
    return CALTERT_SUBSYSTEM(posnCode);
  } else if(regName == "encPos") {
    return CALTERT_SUBSYSTEM(encPos);
  } else if(regName == "calibState") {
    return CALTERT_SUBSYSTEM(calibState);
  } else if(regName == "calibPosReq") {
    return CALTERT_SUBSYSTEM(calibPosReq);
  } else if(regName == "calibMoveOk") {
    return CALTERT_SUBSYSTEM(calibMoveOk);
  } else if(regName == "inPowSup24V") {
    return CALTERT_SUBSYSTEM(inPowSup24V);
  } else if(regName == "outPowSup24V") {
    return CALTERT_SUBSYSTEM(outPowSup24V);
  } else if(regName == "powSup5V") {
    return CALTERT_SUBSYSTEM(powSup5V);
  } else if(regName == "calibTemp") {
    return CALTERT_SUBSYSTEM(calibTemp);
  } else if(regName == "modTemp") {
    return CALTERT_SUBSYSTEM(modTemp);
  } else if(regName == "mirStable") {
    return CALTERT_SUBSYSTEM(mirStable);
  } else if(regName == "calibStable") {
    return CALTERT_SUBSYSTEM(calibStable);
  } else if(regName == "calibTempStable") {
    return CALTERT_SUBSYSTEM(calibTempStable);
  } else if(regName == "calFault") {
    return CALTERT_SUBSYSTEM(calFault);
  } else if(regName == "stepFault") {
    return CALTERT_SUBSYSTEM(stepFault);
  } else if(regName == "encFault") {
    return CALTERT_SUBSYSTEM(encFault);
  } else if(regName == "tertStateMask") {
    return CALTERT_SUBSYSTEM(tertStateMask);
  } else if(regName == "tertPosError") {
    return CALTERT_SUBSYSTEM(tertPosError);
  } else if(regName == "ccwDirLim") {
    return CALTERT_SUBSYSTEM(ccwDirLim);
  } else if(regName == "cwDirLim") {
    return CALTERT_SUBSYSTEM(cwDirLim);
  } else if(regName == "ccwUltLim") {
    return CALTERT_SUBSYSTEM(ccwUltLim);
  } else if(regName == "cwUltLim") {
    return CALTERT_SUBSYSTEM(cwUltLim);
  } else if(regName == "stepDisabled") {
    return CALTERT_SUBSYSTEM(stepDisabled);
  } else if(regName == "dataValid") {
    return CALTERT_SUBSYSTEM(dataValid);
  } else if(regName == "tertPos") {
    return CALTERT_SUBSYSTEM(tertPos);
  } else if(regName == "encIndex") {
    return CALTERT_SUBSYSTEM(encIndex);
  } else if(regName == "calId") {
    return CALTERT_SUBSYSTEM(calId);
  } else if(regName == "calOut") {
    return CALTERT_SUBSYSTEM(calOut);
  } else if(regName == "calIn") {
    return CALTERT_SUBSYSTEM(calIn);
  } else if(regName == "calOutLim") {
    return CALTERT_SUBSYSTEM(calOutLim);
  } else if(regName == "calInLim") {
    return CALTERT_SUBSYSTEM(calInLim);
  } else if(regName == "calDriveFault") {
    return CALTERT_SUBSYSTEM(calDriveFault);
  } else if(regName == "calDisable") {
    return CALTERT_SUBSYSTEM(calDisable);
  } else if(regName == "calDone") {
    return CALTERT_SUBSYSTEM(calDone);
  } else if(regName == "tertPosErr") {
    return CALTERT_SUBSYSTEM(tertPosErr);
  } else if(regName == "fpgaMaj") {
    return CALTERT_SUBSYSTEM(fpgaMaj);
  } else if(regName == "fpgaMin") {
    return CALTERT_SUBSYSTEM(fpgaMin);
  } else if(regName == "fpgaVer") {
    return CALTERT_SUBSYSTEM(fpgaVer);
  } else if(regName == "ctrlReg") {
    return CALTERT_SUBSYSTEM(ctrlReg);
  } else if(regName == "calHardLimFault") {
    return CALTERT_SUBSYSTEM(calHardLimFault);
  } else if(regName == "calStepFault") {
    return CALTERT_SUBSYSTEM(calStepFault);
  } else if(regName == "calDisableFault") {
    return CALTERT_SUBSYSTEM(calDisableFault);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getIfmodMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return IFMOD_SUBSYSTEM(status);
  } else if(regName == "received") {
    return IFMOD_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return IFMOD_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return IFMOD_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return IFMOD_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return IFMOD_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return IFMOD_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return IFMOD_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return IFMOD_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return IFMOD_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return IFMOD_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return IFMOD_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return IFMOD_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return IFMOD_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return IFMOD_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return IFMOD_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return IFMOD_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return IFMOD_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return IFMOD_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return IFMOD_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return IFMOD_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return IFMOD_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return IFMOD_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return IFMOD_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return IFMOD_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return IFMOD_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return IFMOD_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return IFMOD_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return IFMOD_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return IFMOD_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return IFMOD_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return IFMOD_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return IFMOD_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return IFMOD_SUBSYSTEM(buildTime);
  } else if(regName == "ifTotalPower") {
    return IFMOD_SUBSYSTEM(ifTotalPower);
  } else if(regName == "pamTemperature") {
    return IFMOD_SUBSYSTEM(pamTemperature);
  } else if(regName == "totalAtten") {
    return IFMOD_SUBSYSTEM(totalAtten);
  } else if(regName == "pamStatus") {
    return IFMOD_SUBSYSTEM(pamStatus);
  } else if(regName == "ifSwitchState") {
    return IFMOD_SUBSYSTEM(ifSwitchState);
  } else if(regName == "laserStatus") {
    return IFMOD_SUBSYSTEM(laserStatus);
  } else if(regName == "laserPower") {
    return IFMOD_SUBSYSTEM(laserPower);
  } else if(regName == "laserRegError") {
    return IFMOD_SUBSYSTEM(laserRegError);
  } else if(regName == "inputAtten") {
    return IFMOD_SUBSYSTEM(inputAtten);
  } else if(regName == "outputAtten") {
    return IFMOD_SUBSYSTEM(outputAtten);
  } else if(regName == "laserId") {
    return IFMOD_SUBSYSTEM_IDX(laserId, iReg);
  } else if(regName == "errorCount") {
    return IFMOD_SUBSYSTEM(errorCount);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getIntmodMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return INTMOD_SUBSYSTEM(status);
  } else if(regName == "received") {
    return INTMOD_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return INTMOD_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return INTMOD_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return INTMOD_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return INTMOD_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return INTMOD_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return INTMOD_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return INTMOD_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return INTMOD_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return INTMOD_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return INTMOD_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return INTMOD_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return INTMOD_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return INTMOD_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return INTMOD_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return INTMOD_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return INTMOD_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return INTMOD_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return INTMOD_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return INTMOD_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return INTMOD_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return INTMOD_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return INTMOD_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return INTMOD_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return INTMOD_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return INTMOD_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return INTMOD_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return INTMOD_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return INTMOD_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return INTMOD_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return INTMOD_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return INTMOD_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return INTMOD_SUBSYSTEM(buildTime);
  } else if(regName == "photoLev10MHz") {
    return INTMOD_SUBSYSTEM(photoLev10MHz);
  } else if(regName == "photoLev50MHz") {
    return INTMOD_SUBSYSTEM(photoLev50MHz);
  } else if(regName == "photoLevLOTerm") {
    return INTMOD_SUBSYSTEM(photoLevLOTerm);
  } else if(regName == "loRFInLev") {
    return INTMOD_SUBSYSTEM(loRFInLev);
  } else if(regName == "loRFOutLev") {
    return INTMOD_SUBSYSTEM(loRFOutLev);
  } else if(regName == "loTempLev") {
    return INTMOD_SUBSYSTEM(loTempLev);
  } else if(regName == "pamAtten") {
    return INTMOD_SUBSYSTEM(pamAtten);
  } else if(regName == "modTemp") {
    return INTMOD_SUBSYSTEM(modTemp);
  } else if(regName == "powSupPos24V") {
    return INTMOD_SUBSYSTEM(powSupPos24V);
  } else if(regName == "powSupNeg28V") {
    return INTMOD_SUBSYSTEM(powSupNeg28V);
  } else if(regName == "powSupPosDig5V") {
    return INTMOD_SUBSYSTEM(powSupPosDig5V);
  } else if(regName == "powSupPos15V") {
    return INTMOD_SUBSYSTEM(powSupPos15V);
  } else if(regName == "powSupNeg9V") {
    return INTMOD_SUBSYSTEM(powSupNeg9V);
  } else if(regName == "powSupNeg5V") {
    return INTMOD_SUBSYSTEM(powSupNeg5V);
  } else if(regName == "powSupNeg15V") {
    return INTMOD_SUBSYSTEM(powSupNeg15V);
  } else if(regName == "powSupPos5V") {
    return INTMOD_SUBSYSTEM(powSupPos5V);
  } else if(regName == "powSupPos9V") {
    return INTMOD_SUBSYSTEM(powSupPos9V);
  } else if(regName == "statusRegister") {
    return INTMOD_SUBSYSTEM_IDX(statusRegister, iReg);
  } else if(regName == "lockStatus10MHz") {
    return INTMOD_SUBSYSTEM(lockStatus10MHz);
  } else if(regName == "photoStatus10MHz") {
    return INTMOD_SUBSYSTEM(photoStatus10MHz);
  } else if(regName == "photoStatus50MHz") {
    return INTMOD_SUBSYSTEM(photoStatus50MHz);
  } else if(regName == "loTermPowerState") {
    return INTMOD_SUBSYSTEM(loTermPowerState);
  } else if(regName == "sn10MHzModule") {
    return INTMOD_SUBSYSTEM(sn10MHzModule);
  } else if(regName == "sn50MHzModule") {
    return INTMOD_SUBSYSTEM(sn50MHzModule);
  } else if(regName == "snLoTermModule") {
    return INTMOD_SUBSYSTEM(snLoTermModule);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getPmacMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return PMAC_SUBSYSTEM(status);
  } else if(regName == "new_position") {
    return PMAC_SUBSYSTEM(new_position);
  } else if(regName == "new_mode") {
    return PMAC_SUBSYSTEM(new_mode);
  } else if(regName == "new_az") {
    return PMAC_SUBSYSTEM(new_az);
  } else if(regName == "new_el") {
    return PMAC_SUBSYSTEM(new_el);
  } else if(regName == "new_az_rate") {
    return PMAC_SUBSYSTEM(new_az_rate);
  } else if(regName == "new_el_rate") {
    return PMAC_SUBSYSTEM(new_el_rate);
  } else if(regName == "host_read") {
    return PMAC_SUBSYSTEM(host_read);
  } else if(regName == "pmac_write") {
    return PMAC_SUBSYSTEM(pmac_write);
  } else if(regName == "position_fault") {
    return PMAC_SUBSYSTEM(position_fault);
  } else if(regName == "mtr_stat") {
    return PMAC_SUBSYSTEM(mtr_stat);
  } else if(regName == "mtr_pos") {
    return PMAC_SUBSYSTEM_IDX(mtr_pos, iReg);
  } else if(regName == "mtr_com_i") {
    return PMAC_SUBSYSTEM_IDX(mtr_com_i, iReg);
  } else if(regName == "mtr_com_i_phase_az") {
    return PMAC_SUBSYSTEM_IDX(mtr_com_i_phase_az, iReg);
  } else if(regName == "mtr_com_i_phase_el") {
    return PMAC_SUBSYSTEM_IDX(mtr_com_i_phase_el, iReg);
  } else if(regName == "mtr_i_mon_phase_az") {
    return PMAC_SUBSYSTEM_IDX(mtr_i_mon_phase_az, iReg);
  } else if(regName == "mtr_i_mon_phase_el") {
    return PMAC_SUBSYSTEM_IDX(mtr_i_mon_phase_el, iReg);
  } else if(regName == "enc_conv_tab") {
    return PMAC_SUBSYSTEM_IDX(enc_conv_tab, iReg);
  } else if(regName == "res_inc_cnt_err") {
    return PMAC_SUBSYSTEM(res_inc_cnt_err);
  } else if(regName == "count") {
    return PMAC_SUBSYSTEM(count);
  } else if(regName == "res_abs") {
    return PMAC_SUBSYSTEM_IDX(res_abs, iReg);
  } else if(regName == "res_inc_raw") {
    return PMAC_SUBSYSTEM_IDX(res_inc_raw, iReg);
  } else if(regName == "axis_stat") {
    return PMAC_SUBSYSTEM(axis_stat);
  } else if(regName == "az_pos") {
    return PMAC_SUBSYSTEM(az_pos);
  } else if(regName == "el_pos") {
    return PMAC_SUBSYSTEM(el_pos);
  } else if(regName == "az_err") {
    return PMAC_SUBSYSTEM(az_err);
  } else if(regName == "el_err") {
    return PMAC_SUBSYSTEM(el_err);
  } else if(regName == "az_rms_err") {
    return PMAC_SUBSYSTEM(az_rms_err);
  } else if(regName == "el_rms_err") {
    return PMAC_SUBSYSTEM(el_rms_err);
  } else if(regName == "x_tilt") {
    return PMAC_SUBSYSTEM(x_tilt);
  } else if(regName == "y_tilt") {
    return PMAC_SUBSYSTEM(y_tilt);
  } else if(regName == "t_tilt") {
    return PMAC_SUBSYSTEM(t_tilt);
  } else if(regName == "tb_offset") {
    return PMAC_SUBSYSTEM(tb_offset);
  } else if(regName == "tb_raw_count") {
    return PMAC_SUBSYSTEM(tb_raw_count);
  } else if(regName == "drive_status") {
    return PMAC_SUBSYSTEM(drive_status);
  } else if(regName == "pvt_move_time") {
    return PMAC_SUBSYSTEM(pvt_move_time);
  } else if(regName == "track_time") {
    return PMAC_SUBSYSTEM(track_time);
  } else if(regName == "mtr_mon_i") {
    return PMAC_SUBSYSTEM_IDX(mtr_mon_i, iReg);
  } else if(regName == "amp_err_code") {
    return PMAC_SUBSYSTEM_IDX(amp_err_code, iReg);
  } else if(regName == "amp_temp") {
    return PMAC_SUBSYSTEM_IDX(amp_temp, iReg);
  } else if(regName == "new_dk") {
    return PMAC_SUBSYSTEM(new_dk);
  } else if(regName == "new_dk_rate") {
    return PMAC_SUBSYSTEM(new_dk_rate);
  } else if(regName == "dk_pos") {
    return PMAC_SUBSYSTEM(dk_pos);
  } else if(regName == "statusMask") {
    return PMAC_SUBSYSTEM(statusMask);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getRxMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return RX_SUBSYSTEM(status);
  } else if(regName == "received") {
    return RX_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return RX_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return RX_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return RX_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return RX_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return RX_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return RX_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return RX_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return RX_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return RX_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return RX_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return RX_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return RX_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return RX_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return RX_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return RX_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return RX_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return RX_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return RX_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return RX_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return RX_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return RX_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return RX_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return RX_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return RX_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return RX_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return RX_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return RX_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return RX_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return RX_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return RX_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return RX_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return RX_SUBSYSTEM(buildTime);
  } else if(regName == "boardTemperature") {
    return RX_SUBSYSTEM(boardTemperature);
  } else if(regName == "neg15VAnalogVoltage") {
    return RX_SUBSYSTEM(neg15VAnalogVoltage);
  } else if(regName == "pos5VAnalogVoltage") {
    return RX_SUBSYSTEM(pos5VAnalogVoltage);
  } else if(regName == "neg5VDigitalVoltage") {
    return RX_SUBSYSTEM(neg5VDigitalVoltage);
  } else if(regName == "pos15VAnalogVoltage") {
    return RX_SUBSYSTEM(pos15VAnalogVoltage);
  } else if(regName == "pos5VDigitalVoltage") {
    return RX_SUBSYSTEM(pos5VDigitalVoltage);
  } else if(regName == "pos28VDigitalVoltage") {
    return RX_SUBSYSTEM(pos28VDigitalVoltage);
  } else if(regName == "bias90GHz") {
    return RX_SUBSYSTEM(bias90GHz);
  } else if(regName == "drainCurrent30GHz") {
    return RX_SUBSYSTEM_IDX(drainCurrent30GHz, iReg);
  } else if(regName == "gateVoltage30GHz") {
    return RX_SUBSYSTEM_IDX(gateVoltage30GHz, iReg);
  } else if(regName == "gateCurrent30GHz") {
    return RX_SUBSYSTEM_IDX(gateCurrent30GHz, iReg);
  } else if(regName == "ifAmpVoltage30GHz") {
    return RX_SUBSYSTEM(ifAmpVoltage30GHz);
  } else if(regName == "ifAmpCurrent30GHz") {
    return RX_SUBSYSTEM(ifAmpCurrent30GHz);
  } else if(regName == "mixerCurrent30GHz") {
    return RX_SUBSYSTEM(mixerCurrent30GHz);
  } else if(regName == "ledCurrent30GHz") {
    return RX_SUBSYSTEM(ledCurrent30GHz);
  } else if(regName == "gateCurrent90GHz") {
    return RX_SUBSYSTEM_IDX(gateCurrent90GHz, iReg);
  } else if(regName == "drainCurrent90GHz") {
    return RX_SUBSYSTEM_IDX(drainCurrent90GHz, iReg);
  } else if(regName == "ifAmpDrainCurrent90GHz") {
    return RX_SUBSYSTEM(ifAmpDrainCurrent90GHz);
  } else if(regName == "ifAmpGateCurrent90GHz") {
    return RX_SUBSYSTEM(ifAmpGateCurrent90GHz);
  } else if(regName == "tempSensor") {
    return RX_SUBSYSTEM_IDX(tempSensor, iReg);
  } else if(regName == "pos24VAnalogVoltage") {
    return RX_SUBSYSTEM(pos24VAnalogVoltage);
  } else if(regName == "tempRadShield") {
    return RX_SUBSYSTEM(tempRadShield);
  } else if(regName == "tempStage2ColdHead") {
    return RX_SUBSYSTEM(tempStage2ColdHead);
  } else if(regName == "temp90GHzIsolator") {
    return RX_SUBSYSTEM(temp90GHzIsolator);
  } else if(regName == "temp4") {
    return RX_SUBSYSTEM(temp4);
  } else if(regName == "drainSetVoltage30GHz") {
    return RX_SUBSYSTEM_IDX(drainSetVoltage30GHz, iReg);
  } else if(regName == "drainSetVoltage90GHz") {
    return RX_SUBSYSTEM_IDX(drainSetVoltage90GHz, iReg);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getThermalMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return THERMAL_SUBSYSTEM(status);
  } else if(regName == "received") {
    return THERMAL_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return THERMAL_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return THERMAL_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return THERMAL_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return THERMAL_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return THERMAL_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return THERMAL_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return THERMAL_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return THERMAL_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return THERMAL_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return THERMAL_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return THERMAL_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return THERMAL_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return THERMAL_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return THERMAL_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return THERMAL_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return THERMAL_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return THERMAL_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return THERMAL_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return THERMAL_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return THERMAL_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return THERMAL_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return THERMAL_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return THERMAL_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return THERMAL_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return THERMAL_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return THERMAL_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return THERMAL_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return THERMAL_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return THERMAL_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return THERMAL_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return THERMAL_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return THERMAL_SUBSYSTEM(buildTime);
  } else if(regName == "moduleTemperature") {
    return THERMAL_SUBSYSTEM(moduleTemperature);
  } else if(regName == "rboxTopTemperature") {
    return THERMAL_SUBSYSTEM(rboxTopTemperature);
  } else if(regName == "eboxTemperature") {
    return THERMAL_SUBSYSTEM(eboxTemperature);
  } else if(regName == "powSup24V") {
    return THERMAL_SUBSYSTEM(powSup24V);
  } else if(regName == "rboxLoopState") {
    return THERMAL_SUBSYSTEM(rboxLoopState);
  } else if(regName == "rboxPwmFraction") {
    return THERMAL_SUBSYSTEM(rboxPwmFraction);
  } else if(regName == "rboxTemperatureError") {
    return THERMAL_SUBSYSTEM(rboxTemperatureError);
  } else if(regName == "rboxIntTemperatureError") {
    return THERMAL_SUBSYSTEM(rboxIntTemperatureError);
  } else if(regName == "rboxLoopGain") {
    return THERMAL_SUBSYSTEM(rboxLoopGain);
  } else if(regName == "rboxIntGainConstant") {
    return THERMAL_SUBSYSTEM(rboxIntGainConstant);
  } else if(regName == "rboxLoopRateConstant") {
    return THERMAL_SUBSYSTEM(rboxLoopRateConstant);
  } else if(regName == "rboxLoopBandwidth") {
    return THERMAL_SUBSYSTEM(rboxLoopBandwidth);
  } else if(regName == "eboxLoopState") {
    return THERMAL_SUBSYSTEM(eboxLoopState);
  } else if(regName == "eboxVoltage") {
    return THERMAL_SUBSYSTEM(eboxVoltage);
  } else if(regName == "eboxTemperatureError") {
    return THERMAL_SUBSYSTEM(eboxTemperatureError);
  } else if(regName == "eboxIntTemperatureError") {
    return THERMAL_SUBSYSTEM(eboxIntTemperatureError);
  } else if(regName == "eboxLoopGain") {
    return THERMAL_SUBSYSTEM(eboxLoopGain);
  } else if(regName == "eboxIntGainConstant") {
    return THERMAL_SUBSYSTEM(eboxIntGainConstant);
  } else if(regName == "eboxLoopRateConstant") {
    return THERMAL_SUBSYSTEM(eboxLoopRateConstant);
  } else if(regName == "eboxLoopBandwidth") {
    return THERMAL_SUBSYSTEM(eboxLoopBandwidth);
  } else if(regName == "rboxBottomTemperature") {
    return THERMAL_SUBSYSTEM(rboxBottomTemperature);
  } else if(regName == "rboxSetTemperature") {
    return THERMAL_SUBSYSTEM(rboxSetTemperature);
  } else if(regName == "eboxSetTemperature") {
    return THERMAL_SUBSYSTEM(eboxSetTemperature);
  } else if(regName == "circPropConst") {
    return THERMAL_SUBSYSTEM(circPropConst);
  } else if(regName == "powSupPos12V") {
    return THERMAL_SUBSYSTEM(powSupPos12V);
  } else if(regName == "powSupNeg12V") {
    return THERMAL_SUBSYSTEM(powSupNeg12V);
  } else if(regName == "powSupPos5V") {
    return THERMAL_SUBSYSTEM(powSupPos5V);
  } else if(regName == "powSupVoltage") {
    return THERMAL_SUBSYSTEM(powSupVoltage);
  } else if(regName == "powSupCurrent") {
    return THERMAL_SUBSYSTEM(powSupCurrent);
  } else if(regName == "powSupError") {
    return THERMAL_SUBSYSTEM(powSupError);
  } else if(regName == "voltageOffset") {
    return THERMAL_SUBSYSTEM(voltageOffset);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getTiltmeterMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return TILTMETER_SUBSYSTEM(status);
  } else if(regName == "received") {
    return TILTMETER_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return TILTMETER_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return TILTMETER_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return TILTMETER_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return TILTMETER_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return TILTMETER_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return TILTMETER_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return TILTMETER_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return TILTMETER_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return TILTMETER_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return TILTMETER_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return TILTMETER_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return TILTMETER_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return TILTMETER_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return TILTMETER_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return TILTMETER_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return TILTMETER_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return TILTMETER_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return TILTMETER_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return TILTMETER_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return TILTMETER_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return TILTMETER_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return TILTMETER_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return TILTMETER_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return TILTMETER_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return TILTMETER_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return TILTMETER_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return TILTMETER_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return TILTMETER_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return TILTMETER_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return TILTMETER_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return TILTMETER_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return TILTMETER_SUBSYSTEM(buildTime);
  } else if(regName == "lrTilt") {
    return TILTMETER_SUBSYSTEM(lrTilt);
  } else if(regName == "afTilt") {
    return TILTMETER_SUBSYSTEM(afTilt);
  } else if(regName == "boardTemperature") {
    return TILTMETER_SUBSYSTEM(boardTemperature);
  } else if(regName == "tiltTemp") {
    return TILTMETER_SUBSYSTEM(tiltTemp);
  } else if(regName == "structTemp") {
    return TILTMETER_SUBSYSTEM(structTemp);
  } else if(regName == "heaterVoltage") {
    return TILTMETER_SUBSYSTEM(heaterVoltage);
  } else if(regName == "heaterCurrent") {
    return TILTMETER_SUBSYSTEM(heaterCurrent);
  } else if(regName == "loopState") {
    return TILTMETER_SUBSYSTEM(loopState);
  } else if(regName == "pwrFract") {
    return TILTMETER_SUBSYSTEM(pwrFract);
  } else if(regName == "tempDiff") {
    return TILTMETER_SUBSYSTEM(tempDiff);
  } else if(regName == "integDiff") {
    return TILTMETER_SUBSYSTEM(integDiff);
  } else if(regName == "loopGain") {
    return TILTMETER_SUBSYSTEM(loopGain);
  } else if(regName == "loopIntegration") {
    return TILTMETER_SUBSYSTEM(loopIntegration);
  } else if(regName == "loopDiffGain") {
    return TILTMETER_SUBSYSTEM(loopDiffGain);
  } else if(regName == "loopBw") {
    return TILTMETER_SUBSYSTEM(loopBw);
  } else if(regName == "pos24VPsVoltage") {
    return TILTMETER_SUBSYSTEM(pos24VPsVoltage);
  } else if(regName == "pos12VTiltPsVoltage") {
    return TILTMETER_SUBSYSTEM(pos12VTiltPsVoltage);
  } else if(regName == "neg15VTiltPsVoltage") {
    return TILTMETER_SUBSYSTEM(neg15VTiltPsVoltage);
  } else if(regName == "pos5VTiltPsVoltage") {
    return TILTMETER_SUBSYSTEM(pos5VTiltPsVoltage);
  } else if(regName == "pos12VThermalPsVoltage") {
    return TILTMETER_SUBSYSTEM(pos12VThermalPsVoltage);
  } else if(regName == "neg12VThermalPsVoltage") {
    return TILTMETER_SUBSYSTEM(neg12VThermalPsVoltage);
  } else if(regName == "pos5VThermalPsVoltage") {
    return TILTMETER_SUBSYSTEM(pos5VThermalPsVoltage);
  } else if(regName == "teePeeTemp") {
    return TILTMETER_SUBSYSTEM(teePeeTemp);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getTrackerMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return TRACKER_SUBSYSTEM(status);
  } else if(regName == "lacking") {
    return TRACKER_SUBSYSTEM(lacking);
  } else if(regName == "utc") {
    return TRACKER_SUBSYSTEM(utc);
  } else if(regName == "lst") {
    return TRACKER_SUBSYSTEM(lst);
  } else if(regName == "ut1utc") {
    return TRACKER_SUBSYSTEM(ut1utc);
  } else if(regName == "eqneqx") {
    return TRACKER_SUBSYSTEM(eqneqx);
  } else if(regName == "mode") {
    return TRACKER_SUBSYSTEM(mode);
  } else if(regName == "deck_mode") {
    return TRACKER_SUBSYSTEM(deck_mode);
  } else if(regName == "refraction") {
    return TRACKER_SUBSYSTEM_IDX(refraction, iReg);
  } else if(regName == "encoder_off") {
    return TRACKER_SUBSYSTEM_IDX(encoder_off, iReg);
  } else if(regName == "encoder_mul") {
    return TRACKER_SUBSYSTEM_IDX(encoder_mul, iReg);
  } else if(regName == "az_limits") {
    return TRACKER_SUBSYSTEM_IDX(az_limits, iReg);
  } else if(regName == "el_limits") {
    return TRACKER_SUBSYSTEM_IDX(el_limits, iReg);
  } else if(regName == "dk_limits") {
    return TRACKER_SUBSYSTEM_IDX(dk_limits, iReg);
  } else if(regName == "tilts") {
    return TRACKER_SUBSYSTEM_IDX(tilts, iReg);
  } else if(regName == "flexure") {
    return TRACKER_SUBSYSTEM_IDX(flexure, iReg);
  } else if(regName == "axis") {
    return TRACKER_SUBSYSTEM(axis);
  } else if(regName == "collimation") {
    return TRACKER_SUBSYSTEM_IDX(collimation, iReg);
  } else if(regName == "siteActual") {
    return TRACKER_SUBSYSTEM_IDX(siteActual, iReg);
  } else if(regName == "siteFiducial") {
    return TRACKER_SUBSYSTEM_IDX(siteFiducial, iReg);
  } else if(regName == "location") {
    return TRACKER_SUBSYSTEM_IDX(location, iReg);
  } else if(regName == "source") {
    return TRACKER_SUBSYSTEM(source);
  } else if(regName == "equat_geoc") {
    return TRACKER_SUBSYSTEM_IDX(equat_geoc, iReg);
  } else if(regName == "equat_off") {
    return TRACKER_SUBSYSTEM_IDX(equat_off, iReg);
  } else if(regName == "horiz_geoc") {
    return TRACKER_SUBSYSTEM_IDX(horiz_geoc, iReg);
  } else if(regName == "horiz_topo") {
    return TRACKER_SUBSYSTEM_IDX(horiz_topo, iReg);
  } else if(regName == "horiz_mount") {
    return TRACKER_SUBSYSTEM_IDX(horiz_mount, iReg);
  } else if(regName == "horiz_off") {
    return TRACKER_SUBSYSTEM_IDX(horiz_off, iReg);
  } else if(regName == "sky_xy_off") {
    return TRACKER_SUBSYSTEM_IDX(sky_xy_off, iReg);
  } else if(regName == "counts") {
    return TRACKER_SUBSYSTEM_IDX(counts, iReg);
  } else if(regName == "rates") {
    return TRACKER_SUBSYSTEM_IDX(rates, iReg);
  } else if(regName == "actual") {
    return TRACKER_SUBSYSTEM_IDX(actual, iReg);
  } else if(regName == "expected") {
    return TRACKER_SUBSYSTEM_IDX(expected, iReg);
  } else if(regName == "errors") {
    return TRACKER_SUBSYSTEM_IDX(errors, iReg);
  } else if(regName == "state") {
    return TRACKER_SUBSYSTEM(state);
  } else if(regName == "stateMask") {
    return TRACKER_SUBSYSTEM(stateMask);
  } else if(regName == "off_source") {
    return TRACKER_SUBSYSTEM(off_source);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getVaractorMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return VARACTOR_SUBSYSTEM(status);
  } else if(regName == "received") {
    return VARACTOR_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return VARACTOR_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return VARACTOR_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return VARACTOR_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return VARACTOR_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return VARACTOR_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return VARACTOR_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return VARACTOR_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return VARACTOR_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return VARACTOR_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return VARACTOR_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return VARACTOR_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return VARACTOR_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return VARACTOR_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return VARACTOR_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return VARACTOR_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return VARACTOR_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return VARACTOR_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return VARACTOR_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return VARACTOR_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return VARACTOR_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return VARACTOR_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return VARACTOR_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return VARACTOR_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return VARACTOR_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return VARACTOR_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return VARACTOR_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return VARACTOR_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return VARACTOR_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return VARACTOR_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return VARACTOR_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return VARACTOR_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return VARACTOR_SUBSYSTEM(buildTime);
  } else if(regName == "lockStatus") {
    return VARACTOR_SUBSYSTEM(lockStatus);
  } else if(regName == "refStatus") {
    return VARACTOR_SUBSYSTEM(refStatus);
  } else if(regName == "sweepStatus") {
    return VARACTOR_SUBSYSTEM(sweepStatus);
  } else if(regName == "gunnStatus") {
    return VARACTOR_SUBSYSTEM(gunnStatus);
  } else if(regName == "ifMonStatus") {
    return VARACTOR_SUBSYSTEM(ifMonStatus);
  } else if(regName == "dataValid") {
    return VARACTOR_SUBSYSTEM(dataValid);
  } else if(regName == "validityMask") {
    return VARACTOR_SUBSYSTEM(validityMask);
  } else if(regName == "powSupPos24V") {
    return VARACTOR_SUBSYSTEM(powSupPos24V);
  } else if(regName == "powSupPosDig5V") {
    return VARACTOR_SUBSYSTEM(powSupPosDig5V);
  } else if(regName == "powSupPosDig15V") {
    return VARACTOR_SUBSYSTEM(powSupPosDig15V);
  } else if(regName == "powSupPos12V") {
    return VARACTOR_SUBSYSTEM(powSupPos12V);
  } else if(regName == "powSupPos6V") {
    return VARACTOR_SUBSYSTEM(powSupPos6V);
  } else if(regName == "powSupNeg15V") {
    return VARACTOR_SUBSYSTEM(powSupNeg15V);
  } else if(regName == "powSupPos5V") {
    return VARACTOR_SUBSYSTEM(powSupPos5V);
  } else if(regName == "powSupPos9V") {
    return VARACTOR_SUBSYSTEM(powSupPos9V);
  } else if(regName == "statusRegister") {
    return VARACTOR_SUBSYSTEM(statusRegister);
  } else if(regName == "loopGainResistance") {
    return VARACTOR_SUBSYSTEM(loopGainResistance);
  } else if(regName == "boardTemperature") {
    return VARACTOR_SUBSYSTEM(boardTemperature);
  } else if(regName == "noiseMeterVoltage") {
    return VARACTOR_SUBSYSTEM(noiseMeterVoltage);
  } else if(regName == "ifLevel") {
    return VARACTOR_SUBSYSTEM(ifLevel);
  } else if(regName == "errorVoltage") {
    return VARACTOR_SUBSYSTEM(errorVoltage);
  } else if(regName == "biasCurrent") {
    return VARACTOR_SUBSYSTEM(biasCurrent);
  } else if(regName == "maxChnl") {
    return VARACTOR_SUBSYSTEM_IDX(maxChnl, iReg);
  } else if(regName == "statusRegisterMask") {
    return VARACTOR_SUBSYSTEM(statusRegisterMask);
  } else if(regName == "lockStatusError") {
    return VARACTOR_SUBSYSTEM(lockStatusError);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
carma::monitor::MonitorPoint& CarmaMonitorPointTranslator::getYigMonitorPoint(std::string regName, int iReg)
{
  if(regName == "status") {
    return YIG_SUBSYSTEM(status);
  } else if(regName == "received") {
    return YIG_SUBSYSTEM(received);
  } else if(regName == "apiNo") {
    return YIG_SUBSYSTEM(apiNo);
  } else if(regName == "swVersionStr") {
    return YIG_SUBSYSTEM(swVersionStr);
  } else if(regName == "swVersion") {
    return YIG_SUBSYSTEM_IDX(swVersion, iReg);
  } else if(regName == "dongleId") {
    return YIG_SUBSYSTEM(dongleId);
  } else if(regName == "serialNo") {
    return YIG_SUBSYSTEM(serialNo);
  } else if(regName == "moduleType") {
    return YIG_SUBSYSTEM(moduleType);
  } else if(regName == "initRequest") {
    return YIG_SUBSYSTEM(initRequest);
  } else if(regName == "rxErrors") {
    return YIG_SUBSYSTEM(rxErrors);
  } else if(regName == "txErrors") {
    return YIG_SUBSYSTEM(txErrors);
  } else if(regName == "memoryErrors") {
    return YIG_SUBSYSTEM(memoryErrors);
  } else if(regName == "systemErrors") {
    return YIG_SUBSYSTEM(systemErrors);
  } else if(regName == "schOverflowCnt") {
    return YIG_SUBSYSTEM(schOverflowCnt);
  } else if(regName == "tSchOverflowCnt") {
    return YIG_SUBSYSTEM(tSchOverflowCnt);
  } else if(regName == "swVerMaj") {
    return YIG_SUBSYSTEM(swVerMaj);
  } else if(regName == "swVerMin") {
    return YIG_SUBSYSTEM(swVerMin);
  } else if(regName == "swVerTst") {
    return YIG_SUBSYSTEM(swVerTst);
  } else if(regName == "testMode") {
    return YIG_SUBSYSTEM(testMode);
  } else if(regName == "commErrCnt") {
    return YIG_SUBSYSTEM(commErrCnt);
  } else if(regName == "timeErrCnt") {
    return YIG_SUBSYSTEM(timeErrCnt);
  } else if(regName == "swErrCnt") {
    return YIG_SUBSYSTEM(swErrCnt);
  } else if(regName == "hwErrCnt") {
    return YIG_SUBSYSTEM(hwErrCnt);
  } else if(regName == "timeJitter") {
    return YIG_SUBSYSTEM(timeJitter);
  } else if(regName == "sinceLastTs") {
    return YIG_SUBSYSTEM(sinceLastTs);
  } else if(regName == "tsDelta") {
    return YIG_SUBSYSTEM(tsDelta);
  } else if(regName == "apiVer") {
    return YIG_SUBSYSTEM(apiVer);
  } else if(regName == "timeOffset") {
    return YIG_SUBSYSTEM(timeOffset);
  } else if(regName == "timeStampInt") {
    return YIG_SUBSYSTEM(timeStampInt);
  } else if(regName == "timeStampDelta") {
    return YIG_SUBSYSTEM(timeStampDelta);
  } else if(regName == "uptime") {
    return YIG_SUBSYSTEM(uptime);
  } else if(regName == "bootLoader") {
    return YIG_SUBSYSTEM(bootLoader);
  } else if(regName == "buildDate") {
    return YIG_SUBSYSTEM(buildDate);
  } else if(regName == "buildTime") {
    return YIG_SUBSYSTEM(buildTime);
  } else if(regName == "lockState") {
    return YIG_SUBSYSTEM(lockState);
  } else if(regName == "dataValid") {
    return YIG_SUBSYSTEM(dataValid);
  } else if(regName == "frequency") {
    return YIG_SUBSYSTEM(frequency);
  } else if(regName == "loopGainResistance") {
    return YIG_SUBSYSTEM(loopGainResistance);
  } else if(regName == "dampingResistance") {
    return YIG_SUBSYSTEM(dampingResistance);
  } else if(regName == "ifLevel") {
    return YIG_SUBSYSTEM(ifLevel);
  } else if(regName == "errorVoltage") {
    return YIG_SUBSYSTEM(errorVoltage);
  } else if(regName == "current") {
    return YIG_SUBSYSTEM(current);
  } else if(regName == "noiseMeterVoltage") {
    return YIG_SUBSYSTEM(noiseMeterVoltage);
  } else if(regName == "boardTemperature") {
    return YIG_SUBSYSTEM(boardTemperature);
  } else if(regName == "maxChnl") {
    return YIG_SUBSYSTEM_IDX(maxChnl, iReg);
  } else if(regName == "id") {
    return YIG_SUBSYSTEM(id);
  } else if(regName == "calDate") {
    return YIG_SUBSYSTEM_IDX(calDate, iReg);
  } else if(regName == "sweepStatus") {
    return YIG_SUBSYSTEM(sweepStatus);
  } else if(regName == "refStatus") {
    return YIG_SUBSYSTEM(refStatus);
  } else if(regName == "autoRelock") {
    return YIG_SUBSYSTEM(autoRelock);
  } else if(regName == "relockCount") {
    return YIG_SUBSYSTEM(relockCount);
  } else if(regName == "lockStateMask") {
    return YIG_SUBSYSTEM(lockStateMask);
  } else if(regName == "lockBit") {
    return YIG_SUBSYSTEM(lockBit);
  } else if(regName == "lockBitMask") {
    return YIG_SUBSYSTEM(lockBitMask);
  } else {
    ThrowError("Unrecognized register name: " << regName);
  }
}
