//$Id: SignalPathMapperHandle.cc,v 1.28 2014/06/04 17:09:16 mpound Exp $
#include "carma/corba/corba.h"

#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/SignalPathMapperHandle.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/SeqTypedefs.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iostream>
#include <sstream>
#include <set>

using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::signalpath;
using namespace carma::switchyard;
using namespace carma::util;
using namespace std;

#define SPM_FN(argList, tryBlock) \
  {\
    ostringstream os;                       \
    os << "SignalPathMapperHandle::" << __FUNCTION__;       \
    ScopedLogNdc ndc(os.str().c_str());             \
    if ( isObjReachable( ) ) {                  \
      os.str("");                       \
      os << "(" << argList << ")";              \
      /*programLogNoticeIfPossible(os.str());*/ \
      try {                         \
    const double sendTime = Time::MJD( );           \
    tryBlock;                       \
    logSentCommandIfNeeded(os.str(), sendTime );        \
      }  catch (...) {                      \
    logCaughtAsErrorAndRethrowAsUser();         \
      }                             \
    } else {                                \
      programLogInfoIfPossible("SignalPathMapper is not reachable");    \
    }\
  }


SignalPathMapperHandle::SignalPathMapperHandle( 
  MonitorSystem                   & carmaMonitor,
  ControlSubsystemBase::Reachable & reachable ) :
  SignalPathMapperRemoteObjHandle(
      SIGNALPATHMAPPERCONTROL_NAME,
      &( reachable.signalPath() ),
      &( carmaMonitor.signalPath() ),
      &carmaMonitor,
      true,
      false) // change to true for method logging
{
    ostringstream os;
    os << "SignalPathMapperHandle c'tor: " << SIGNALPATHMAPPERCONTROL_NAME;
    const string msg = os.str();
    CPTRACE( Trace::TRACE3, msg );
}


SignalPathMapperHandle::~SignalPathMapperHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void 
SignalPathMapperHandle::initializeCableMap( const string & filename )
{
  SPM_FN(filename,
     remoteObj()->initializeCableMap(filename.c_str()));
}

void 
SignalPathMapperHandle::loadConfiguration( 
                      const string & filename,
                      const string & confName, 
                      const string & astroBandConfName )
{
  SPM_FN(filename << " , " << confName << " , " << astroBandConfName,
     remoteObj()->loadConfiguration(filename.c_str(), confName.c_str(), astroBandConfName.c_str()));
}

void 
SignalPathMapperHandle::configureAstroBand( 
                            unsigned short bandNo,
                            const string & confName, 
                unsigned short subarrayNo,
                            ControlCorrelatorDesignation type)
{
  SPM_FN(bandNo  << " , " << confName << " , " << getStringForCorrType( type ),
     remoteObj()->configureAstroBand( bandNo, confName.c_str(), subarrayNo, type));
}

void SignalPathMapperHandle::
checkConfigurationValidity(unsigned short bandNo,
               const string & confName, 
               unsigned short subarrayNo,
               ControlCorrelatorDesignation type )
{
  SPM_FN(bandNo << ", " << confName << ", " << getStringForCorrType(type),
     remoteObj()->checkConfigurationValidity( bandNo, confName.c_str(), subarrayNo, type ));
}

void SignalPathMapperHandle::
checkConfigurationSuccess(unsigned short bandNo)
{
  SPM_FN(bandNo,
     remoteObj()->checkConfigurationSuccess(bandNo));
}

void SignalPathMapperHandle::
clearAstroBandConfiguration(unsigned short bandNo, unsigned short subarrayNo, ControlCorrelatorDesignation type)
{
  SPM_FN("bandNo = " << bandNo << " , type = " << getStringForCorrType( type ),
     remoteObj()->clearAstroBandConfiguration(bandNo, subarrayNo, type));
}

vector<SignalPathMapperControl::CorrelatorBandInput>
SignalPathMapperHandle::getCorrelatorBandInputMap(const SignalPathMapperControl::CorrelatorBand & band )
{
  vector<SignalPathMapperControl::CorrelatorBandInput> bandVec;

  SPM_FN("( [ bandNo = " << band.bandNo << " , crateNo = " << band.crate.crateNo << " , type = " << getStringForCorrType(band.crate.type) << " ] )",
     SignalPathMapperControl::CorrelatorBandInputSeq_var cbSeq;
     cbSeq.out() = remoteObj( )->getCorrelatorBandInputMap( band );
     bandVec = convertSequenceToVector<SignalPathMapperControl::CorrelatorBandInput>(cbSeq.in()));
  
  return bandVec;
}

vector<SignalPathMapperControl::Antenna>
SignalPathMapperHandle::getAntennas(unsigned short astroBandNo)
{
  vector<SignalPathMapperControl::Antenna> antVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ",
     SignalPathMapperControl::AntennaSeq_var antSeq;
     antSeq.out() = remoteObj( )->getAntennas( astroBandNo );
  antVec = convertSequenceToVector<SignalPathMapperControl::Antenna>(antSeq.in()));

  return antVec;
}

/**.......................................................................
 * Query switch settings
 */
std::vector<carma::switchyard::SwitchPosition>
SignalPathMapperHandle::getIFSwitchSettings(unsigned short astroBandNo)
{
  vector<carma::switchyard::SwitchPosition> swVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ", 
     carma::switchyard::SwitchPositionSeq_var swSeq;
     swSeq.out() = remoteObj( )->getIFSwitchSettings( astroBandNo );
     swVec = convertSequenceToVector<carma::switchyard::SwitchPosition>(swSeq.in()));

  return swVec;
}

std::vector<carma::switchyard::SwitchPosition>
SignalPathMapperHandle::getDCLOSwitchSettings(unsigned short astroBandNo)
{
  vector<carma::switchyard::SwitchPosition> swVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ", 
     carma::switchyard::SwitchPositionSeq_var swSeq;
     swSeq.out() = remoteObj( )->getDCLOSwitchSettings( astroBandNo );
     swVec = convertSequenceToVector<carma::switchyard::SwitchPosition>(swSeq.in()));

  return swVec;
}

std::vector<carma::switchyard::SwitchPosition>
SignalPathMapperHandle::getLOSwitchSettings(unsigned short astroBandNo)
{
  vector<carma::switchyard::SwitchPosition> swVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ", 
     carma::switchyard::SwitchPositionSeq_var swSeq;
     swSeq.out() = remoteObj( )->getLOSwitchSettings( astroBandNo );
     swVec = convertSequenceToVector<carma::switchyard::SwitchPosition>(swSeq.in()));

  return swVec;
}

std::vector<carma::switchyard::SwitchPosition>
SignalPathMapperHandle::getLLSwitchSettings(unsigned short astroBandNo)
{
  vector<carma::switchyard::SwitchPosition> swVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ", 
     carma::switchyard::SwitchPositionSeq_var swSeq;
     swSeq.out() = remoteObj( )->getLLSwitchSettings( astroBandNo );
     swVec = convertSequenceToVector<carma::switchyard::SwitchPosition>(swSeq.in()));

  return swVec;
}

vector<SignalPathMapperControl::BlockDownconverterSetting>
SignalPathMapperHandle::getBdcSettings(unsigned short astroBandNo)
{
  vector<SignalPathMapperControl::BlockDownconverterSetting> bdcVec;

  SPM_FN(" [ astroBandNo = " << astroBandNo << " ] ",
     SignalPathMapperControl::BlockDownconverterSettingSeq_var bdcSeq;
     bdcSeq.out() = remoteObj( )->getBdcSettings( astroBandNo );
     bdcVec = convertSequenceToVector<SignalPathMapperControl::BlockDownconverterSetting>(bdcSeq.in()));

  return bdcVec;
}

vector<SignalPathMapperControl::AstroBand>
SignalPathMapperHandle::getActiveAstroBands( ControlCorrelatorDesignation type )
{
  vector<SignalPathMapperControl::AstroBand> bandVec;

  SPM_FN(getStringForCorrType(type),
     SignalPathMapperControl::AstroBandSeq_var abSeq;
     abSeq.out() = remoteObj( )->getActiveAstroBands( type );
     bandVec = convertSequenceToVector<SignalPathMapperControl::AstroBand>(abSeq.in()));

  return bandVec;
}

vector<SignalPathMapperControl::AstroBand>
SignalPathMapperHandle::getAstroBandsForConfiguration( const ::std::string & confName, 
                               unsigned short subarrayNo,
                               ControlCorrelatorDesignation type)
{
  vector<SignalPathMapperControl::AstroBand> bandVec;


  SPM_FN(confName << " , " << subarrayNo << ", " << getStringForCorrType( type ),
     SignalPathMapperControl::AstroBandSeq_var abSeq;
     abSeq.out() = remoteObj()->getAstroBandsForConfiguration(confName.c_str(), subarrayNo, type);
     bandVec = convertSequenceToVector<SignalPathMapperControl::AstroBand>(abSeq.in()));

  return bandVec;
}


vector<SignalPathMapperControl::CorrelatorBand>
SignalPathMapperHandle::getActiveCorrelatorBands( ControlCorrelatorDesignation type )
{
  vector<SignalPathMapperControl::CorrelatorBand> bandVec;

  SPM_FN(getStringForCorrType(type),
     SignalPathMapperControl::CorrelatorBandSeq_var cbSeq;
     cbSeq.out() = remoteObj( )->getActiveCorrelatorBands( type );
     bandVec = convertSequenceToVector<SignalPathMapperControl::CorrelatorBand>(cbSeq.in()));
  return bandVec;
}

vector<SignalPathMapperControl::CorrelatorBand>
SignalPathMapperHandle::getCorrelatorBands(unsigned astroBandNo)
{
    vector<SignalPathMapperControl::CorrelatorBand> bandVec;

    SPM_FN( "astroBandNo = " << astroBandNo,
       SignalPathMapperControl::CorrelatorBandSeq_var cbSeq;
       cbSeq.out() = remoteObj( )->getCorrelatorBands( astroBandNo );
       if ( cbSeq.in().length() > 0 ) {
           bandVec = convertSequenceToVector<SignalPathMapperControl::CorrelatorBand>( cbSeq.in() );
       } 
    );

    return bandVec;
}

carma::util::CorrelatorFpgaModeType 
SignalPathMapperHandle::getFpgaMode(unsigned astroBandNo)
{
  carma::util::CorrelatorFpgaModeType fpgaMode = carma::util::CORR_SINGLEPOL;

  SPM_FN(astroBandNo,
     SignalPathMapperControl::CorrelatorBandSeq_var cbSeq;
     fpgaMode = remoteObj( )->getFpgaMode( astroBandNo));

  return fpgaMode;
}

void 
SignalPathMapperHandle::assignWalshColumn(SignalPathMapperControl::WalshColumnAssignment wca)
{
  SPM_FN(" [" << wca.antNo << ", " << wca.walshColNo  <<"] ",
     remoteObj()->assignWalshColumn(wca));
}

void
SignalPathMapperHandle::clearWalshColumnAssignment(unsigned short antNo )
{
  SPM_FN(antNo,
     remoteObj()->clearWalshColumnAssignment( antNo));
}

vector<SignalPathMapperControl::WalshColumnAssignment>
SignalPathMapperHandle::getWalshColumnAssignment(unsigned short antNo )
{
  vector<SignalPathMapperControl::WalshColumnAssignment> wcaVec;

  SPM_FN(antNo,
     SignalPathMapperControl::WalshColumnAssignmentSeq_var wcaSeq;
     wcaSeq.out() = remoteObj( )->getWalshColumnAssignment( antNo );
     wcaVec = convertSequenceToVector<SignalPathMapperControl::WalshColumnAssignment>( wcaSeq.in())
  );

  return wcaVec;
}

string 
SignalPathMapperHandle::queryConfiguration()
{
  string config("");

  SPM_FN("",
     config = remoteObj( )->queryConfiguration()
  );

  return config;
}

void SignalPathMapperHandle::addAntenna(unsigned short antNo, 
                                        unsigned short subarrayNo)
{
  SPM_FN(antNo << ", " << subarrayNo,
     remoteObj()->addAntenna(antNo, subarrayNo)
  );
}

void SignalPathMapperHandle::removeAntenna(unsigned short antNo, 
                                           unsigned short subarrayNo)
{
  SPM_FN(antNo << ", " << subarrayNo,
     remoteObj()->removeAntenna(antNo, subarrayNo)
  );
}

void SignalPathMapperHandle::addCorrelator(ControlCorrelatorDesignation type,
                       unsigned short subarrayNo)
{
  SPM_FN(type << ", " << subarrayNo,
     remoteObj()->addCorrelator(type, subarrayNo)
  );
}

void SignalPathMapperHandle::removeCorrelator(ControlCorrelatorDesignation type,
                          unsigned short subarrayNo)
{
  SPM_FN(type << ", " << subarrayNo,
     remoteObj()->removeCorrelator(type, subarrayNo)
  );
}

SeqShort *
SignalPathMapperHandle::getCorrelatorBandNoSeq(unsigned astroBandNo)
{

  SeqShort_var cbnSeq;
  SPM_FN( astroBandNo, 
     cbnSeq.out() = remoteObj( )->getCorrelatorBandNoSeq( astroBandNo );
  );

  return cbnSeq._retn();
}

SeqShort *
SignalPathMapperHandle::getActiveAstroBandNoSeq( ControlCorrelatorDesignation type )
{

  SeqShort_var cbnSeq;
  SPM_FN( "type = " << type,
     cbnSeq.out() = remoteObj( )->getActiveAstroBandNoSeq( type );
  );
  return cbnSeq._retn();
}

vector<short>
SignalPathMapperHandle::getActiveAstroBandNoVec( ControlCorrelatorDesignation type )
{
    ScopedLogNdc ndc("SignalPathMapperHandle::getActiveAstroBandNoVec");
  vector<short> cbnVec;
  SPM_FN( "type = " << type,
     SeqShort_var cbnSeq;
     cbnSeq.out() = remoteObj( )->getActiveAstroBandNoSeq( type );
     cbnVec = convertSequenceToVector<short>(cbnSeq.in());
  );
  return cbnVec;
}

bool SignalPathMapperHandle::isValidAstroBand( unsigned astroBandNo, 
                           ControlCorrelatorDesignation type )
{
      SeqShort_var cbnSeq;
      SPM_FN( "ABno = " << astroBandNo << " , type = " << type,
         cbnSeq.out() = remoteObj( )->getActiveAstroBandNoSeq( type );
      );
      std::set<unsigned> cbnSet = convertSequenceToSet<unsigned>( cbnSeq.in() );
      return ( cbnSet.find( astroBandNo ) != cbnSet.end() );

}

string 
SignalPathMapperHandle::getConfname( unsigned short astroBandNo ) 
{
  string confName("");

  SPM_FN("",
     confName = remoteObj( )->getConfName( astroBandNo )
  );

  return confName;
}

int SignalPathMapperHandle::getAstroBandForCorrelatorBand( unsigned short corrBandNo,
                                 ControlCorrelatorDesignation type )
{
    int abNo = -1;
      SPM_FN( "corrBandNo = " << corrBandNo,
          abNo = remoteObj( )->getAstroBandForCorrelatorBand( corrBandNo, type );
      );
      return abNo;
}

ControlCorrelatorDesignation
SignalPathMapperHandle::getCorrTypeForAstroBand( unsigned short astroBandNo )
{
  ControlCorrelatorDesignation cType = util::CORR_NONE;
    SPM_FN( "astroBandNo = " << astroBandNo,
          cType = remoteObj( )->getCorrTypeForAstroBand( astroBandNo );
      );
    return cType;
}
