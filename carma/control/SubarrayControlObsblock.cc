/**
 *
 * Carma control interface server implementation for project & obsblock
 * commands
 *
 * @author: Marc Pound
 *
 * $Id: SubarrayControlObsblock.cc,v 1.38 2014/07/28 16:50:03 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <iomanip>
#include <fstream>
#include <sstream>

#include "carma/control/IntentInfo.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/control/stringUtils.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/corba/corba.h"
#include "carma/util/StringUtils.h"
#include "carma/services/stringConstants.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;

namespace { // < anonymous >
    static const string kVALID_PURPOSES = "ABFGPRSO" ;
    static const string kUNUSED         = "UNUSED" ;

    static const string PROJECT_NAMES[] =
    {
        "BASE",   // baseline
        "DDT",    // Director's discretionary time.
        "FRINGE", // fringe test
        "FLUX",   // flux observations
         MAINTENANCE, // throwaway integrations, maintenance
         NO_PROJECT,  // throwaway integrations, the default project
        "OPNT",    // optical pointing
        "RPNT",    // radio pointing
        "TEST",    // generic test
        "TILT",    // tilts
         WEATHER   // throwaway integrations, weather
    };

    static const IntentInfo DEFAULT_INTENTS[] =
    {
        IntentInfo( "GF", true , false ),  // base
        IntentInfo( "O" , false, false ),  // discretionary
        IntentInfo( "O" , false, false ),  // fringe
        IntentInfo( "F" , false, false ),  // flux
        IntentInfo( "O" , false, false ),  // maintenance
        IntentInfo( "O" , false, false ),  // none
        IntentInfo( "O" , false, false ),  // opnt
        IntentInfo( "R" , false, false ),  // rpnt
        IntentInfo( "O" , false, false ),  // test
        IntentInfo( "O" , false, false ),  // tilts
        IntentInfo( "O" , false, false )   // weather
    };

    static const unsigned short NUM_INTENTS = 11;
    // see http://cedarflat.mmarray.org/statistics/test_projects.txt
    static const int MAX_COMMISSIONING_NO   = 17;
    static const size_t PROJECT_NUMERIC_CHARS = 3;
}

std::string SubarrayControlImpl::getProject(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(getCorrelatorDesignation());
  carmaMonitor_.readNewestConditionalCopy();
  MonitorPointString& projectMp = getProjectMpForRead(corrSet.getFirstControlCorrelatorDesignation());
  return projectMp.getValue();
}

SubarrayControlImpl::ObsblockGroup SubarrayControlImpl::getObsblock(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet inputSet(ctype);

  if(inputSet.isEmpty()) {
    ThrowCarmaError("The getObsblock() method cannot be called with CORR_NONE as an argument");
  }

  ObsblockGroup obGroup;
  CorrelatorSet ownedSet(getCorrelatorDesignation());

  carmaMonitor_.readNewestConditionalCopy();

  std::string fullObsblock;
  bool haveit = false;

  if(!inputSet.isSingleCorrelator()) {

    // If we currently own multiple correlators, use the first one for
    // the obsblock name

    if(!ownedSet.isSingleCorrelator()) {

      fullObsblock = getObsblockIdMpForRead(ownedSet.getFirstControlCorrelatorDesignation()).getValue();
      obGroup.corrType = ownedSet.getControlCorrelatorDesignation();
      haveit = true;

      // Else if empty, return an empty obsblock object

    } else if(ownedSet.isEmpty()) {

      obGroup.corrType = carma::util::CORR_NONE;
      return obGroup;

      // Else return the appropriate name

    } else if(inputSet.includes(ownedSet)) {
     
      fullObsblock = getObsblockIdMpForRead(ownedSet.getControlCorrelatorDesignation()).getValue();
      obGroup.corrType = ownedSet.getControlCorrelatorDesignation();
      haveit = true;
      
    }
  } else {

    if(ownedSet.isEmpty()) {

      obGroup.corrType = carma::util::CORR_NONE;
      return obGroup;

    } else if(ownedSet.includes(inputSet)) {
     
      fullObsblock = getObsblockIdMpForRead(inputSet.getControlCorrelatorDesignation()).getValue();
      obGroup.corrType = inputSet.getControlCorrelatorDesignation();
      haveit = true;
      
    }
  }

  if(!haveit) {
    ThrowCarmaError("Subarray " << subarrayNo_ << " is not associated with " << inputSet
                << ", and cannot obtain the obsBlockId.");
  }

  const string DELIMITER(".");
  vector<string> token = StringUtils::tokenize(fullObsblock,DELIMITER);
  const unsigned numTokens = token.size();
  switch( numTokens ) {
      case 3:
          obGroup.project  = token[0];
          obGroup.obsblock = token[1];
          obGroup.trial    = atoi(token[2].c_str());
          break;
      case 4:
          obGroup.project     = token[0];
          obGroup.obsblock    = token[1];
          obGroup.subObsblock = token[2];
          obGroup.trial       = atoi(token[3].c_str());
          break;
      default:
          ThrowCarmaError("Oh noes! Unexpected number of substrings ("<<
                  numTokens << ") in obsblockId " << fullObsblock);
          break;
  }
  /*
  std::string::size_type pos,epos;
  pos = fullObsblock.find(".",0);
  obGroup.project = fullObsblock.substr(0,pos);
  epos = fullObsblock.find(".",pos + 1);
  obGroup.obsblock = fullObsblock.substr(pos + 1, epos - pos - 1);
  pos = epos;
  epos = fullObsblock.rfind(".");
  if(epos == pos) {
    obGroup.subObsblock = "";
    obGroup.trial = atoi(fullObsblock.substr(pos + 1).c_str());
  }
  else{
    obGroup.subObsblock = fullObsblock.substr(pos + 1, epos - pos - 1);
    obGroup.trial = atoi(fullObsblock.substr(epos + 1).c_str());
  }
  */
  return obGroup;
}

void
SubarrayControlImpl::setObsblock(const char* project, const char* obsblock,
                     const char* subObsblock, CORBA::Long trial)
try{

    cmdlog() << "setObsblock(" << project << "," << obsblock << ","
             << subObsblock << "," << trial << ")";
    if ( subarrayIsIntegrating() ) {
        string m ;
        m = "You cannot change obsblocks while the subarray is integrating!";
        cmdlog() << "Exception: " << m;
        throw CARMA_EXCEPTION( carma::util::UserException, m.c_str());
    }

    if ( subarrayIsIntegrating() ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
            "You cannot use setObsblock while the subarray is integrating!"
        );
    }

    // just return if not a science subarray
    if (subarrayNo_ > 2) {
        cmdlog() << "setObsblock command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }

    string p(project);
    string o(obsblock);
    string so(subObsblock);

    ObsblockGroup obGroup = getObsblock(getCorrelatorDesignation());

    checkStringValidity(p,  "Project");
    checkStringValidity(o,  "Obsblock");
    checkStringValidity(so, "SubObsblock", false);

    obGroup.project     = p;
    obGroup.obsblock    = o;
    obGroup.subObsblock = so;
    obGroup.trial       = trial;

    if(setObsblockIdItem(obGroup, true, true, true, true)) {
      invalidateIntent();
      invalidateConstraints();
      if ( hasDefaultIntent( p ) ) {
        const IntentInfo iinfo = getDefaultIntent( p );
        setDefaultIntent( iinfo );
        setDefaultObsObject();
      }

      // EML: Every other set method calls sleep but this one.  I
      // suspect it's an error, and I'm adding it in

      // sleep for just over 1 frame.

      usleep(700000UL);
    }

} catch (...) {
        rethrowCaughtAsUser( );
}



// @TODO FIX ME FOR USE WITH C3G.
// 1. Need a generic method that takes a sequence of Obsblock structs.
// 2. Then PDBMH:projectRun to be generically multi-correlator and not
//    assume only NCORR=1|2 and C1=SPECTRAL and C2=WIDEBAND.
void
SubarrayControlImpl::setAllObsblocks(const char* slproject, const char* slobsblock,
                     const char* slsubObsblock, CORBA::Long sltrial,
                     const char* wbproject, const char* wbobsblock,
                     const char* wbsubObsblock, CORBA::Long wbtrial )
try{
    cmdlog() << "setAllObsblocks(" << slproject << "," << slobsblock << ","
             << slsubObsblock << "," << sltrial << "," << wbproject << "," 
             << wbobsblock << "," << wbsubObsblock << "," << wbtrial <<")";
    if ( anyScienceSubarrayIsIntegrating() ) {
        string m ;
        m = "You cannot change obsblocks while a subarray is integrating!";
        cmdlog() << "Exception: " << m;
        throw CARMA_EXCEPTION( carma::util::UserException, m.c_str());
    }

    if ( anyScienceSubarrayIsIntegrating() ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
            "You cannot use setAllObsblocks while a subarray is integrating!"
        );
    }

    string p1(slproject);
    string o1(slobsblock);
    string so1(slsubObsblock);
    string p2(wbproject);
    string o2(wbobsblock);
    string so2(wbsubObsblock);

    ObsblockGroup obGroupS = getObsblock(carma::util::CORR_SPECTRAL);
    ObsblockGroup obGroupW = getObsblock(carma::util::CORR_WIDEBAND);
    
    // ugh this should be done with ObsblockGpoup ==operator overloading
    if ( StringUtils::equalsIgnoreCase( p1, obGroupS.project ) &&
         StringUtils::equalsIgnoreCase( o1, obGroupS.obsblock ) &&
         StringUtils::equalsIgnoreCase( so1, obGroupS.subObsblock ) &&
         sltrial == (obGroupS.trial)  &&
         StringUtils::equalsIgnoreCase( p2, obGroupW.project ) &&
         StringUtils::equalsIgnoreCase( o2, obGroupW.obsblock ) &&
         StringUtils::equalsIgnoreCase( so2, obGroupW.subObsblock ) &&
         wbtrial == (obGroupW.trial) ) {

        return;
    }

    checkStringValidity(p1, "Project");
    checkStringValidity(p2, "Project");

    checkStringValidity(o1, "Obsblock");
    checkStringValidity(o2, "Obsblock");

    checkStringValidity(so1, "SubObsblock", false);
    checkStringValidity(so2, "SubObsblock", false);

    if ( sltrial < 1 || wbtrial < 1) {
      throw CARMA_EXCEPTION( carma::util::UserException,
                     "Trial number must be positive");
    }

    obGroupS.project = p1;
    obGroupS.obsblock = o1;
    obGroupS.subObsblock = so1;
    obGroupS.trial = sltrial;

    obGroupW.project = p2;
    obGroupW.obsblock = o2;
    obGroupW.subObsblock = so2;
    obGroupW.trial = wbtrial;


    
    controlSubsystem_.spectralLineCorrelator().project().setValue( obGroupS.project );
    controlSubsystem_.spectralLineCorrelator().obsBlockId().setValue( makeObsBlockId(obGroupS) );
    controlSubsystem_.widebandCorrelator().project().setValue( obGroupW.project );
    controlSubsystem_.widebandCorrelator().obsBlockId().setValue( makeObsBlockId(obGroupW) );

    //ThrowCarmaError("setAllObsblocks() hasn't been defined for new correlators");

    controlSubsystem_.subarray(subarrayNo_ - 1).project().setValue( obGroupS.project );
    controlSubsystem_.subarray(subarrayNo_ - 1).obsBlockId().setValue( makeObsBlockId(obGroupS) );

    invalidateIntent();
    invalidateConstraints();
    if ( hasDefaultIntent( p1 ) ) {
        const IntentInfo iinfo = getDefaultIntent( p1 );
        setDefaultIntent( iinfo );
        setDefaultObsObject();
    }

} catch (...) {
        rethrowCaughtAsUser( );
}

void
SubarrayControlImpl::project(const char* project)
try {
    cmdlog() << "project(" << project << ")";
    if (subarrayIsIntegrating()) {
        string m ;
        m = "You cannot change projects while the subarray is integrating!";
        cmdlog() << "Exception: " << m;
        throw CARMA_EXCEPTION(carma::util::UserException, m.c_str());
    }

    // is this really needed? the subarrays greater>2 have
    // project etc MPs.  issuing this command for those subarrays
    // won't affect other subarrays....
    // Besides, might we have pure engineering projects that
    // don't produce visibility data on the far end, but
    // never the less have an obsblock/script?
    if (subarrayNo_ > 2) {
        cmdlog() << "Project command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }

    string check(project) ;

    carmaMonitor_.readNewestConditionalCopy();
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    // if project name has not actually changed, just return.

    if(StringUtils::equalsIgnoreCase(check, ob.project)) return;

    checkStringValidity(check, "Project");

    ob.project = check;
    if(setObsblockIdProject(ob)) {

      invalidateIntent();
      invalidateConstraints();
      if ( hasDefaultIntent( check ) ) {
        const IntentInfo iinfo = getDefaultIntent( check );
        setDefaultIntent( iinfo );
        setDefaultObsObject();
      }

      // sleep for just over 1 frame.

      usleep(700000UL);
    }

 } catch (...) {
  rethrowCaughtAsUser( );
 }

void
SubarrayControlImpl::obsblock(const char* obsblock)
try {

    if ( subarrayIsIntegrating() ) {
        string m ;
        m = "You cannot change obsblocks while the subarray is integrating!";
        cmdlog() << "Exception: " << m;
        throw CARMA_EXCEPTION( carma::util::UserException, m.c_str());
    }

    if (subarrayNo_ > 2) {
        cmdlog() << "Obsblock command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }
    cmdlog() << "obsblock(" << obsblock << ")";

    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    string check(obsblock) ;

    checkStringValidity(check, "Obsblock");

    ob.obsblock = check;
    if(setObsblockIdObsblock(ob)) {

      // Sleep for just over 1 frame.

      usleep(700000UL);
    }

} catch ( ... ) {
    rethrowCaughtAsUser( );
 }



void
SubarrayControlImpl::subObsblock(const char* subObsblock)
try {

    if ( subarrayIsIntegrating() ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
            "You cannot change sub-obsblocks while the subarray is integrating!"
        );
    }

    if (subarrayNo_ > 2) {
        cmdlog() << "subObsblock command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }
    cmdlog() << "subObsblock(" << subObsblock << ")";

    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    string check(subObsblock) ;

    checkStringValidity(check, "SubObsblock", false);

    ob.subObsblock = check;
    if(setObsblockIdSubobsblock(ob)) {
      // sleep for just over 1 frame.
      usleep(700000UL);
    }

} catch ( ... ) {
    rethrowCaughtAsUser( );
}

//@todo need to add a method incrementTrialNo() to query the
//PJDB and return the next trial available for a given project/obsblock
void
SubarrayControlImpl::trial(CORBA::Long number)
try {

    if ( subarrayIsIntegrating() ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
        "You cannot change trials while the subarray is integrating!"
        );
    }

    if (subarrayNo_ > 2) {
        cmdlog() << "Trial command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }
    cmdlog() << "trial(" << number << ")";
    if ( number < 1 ) {
        throw CARMA_EXCEPTION( carma::util::UserException,
            "Trial number must be positive");
    }

    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    ob.trial = number;
    if(setObsblockIdTrial(ob)) {
      // sleep for just over 1 frame.
      usleep(700000UL);
    }

} catch ( ... ) {
    rethrowCaughtAsUser( );
}

string SubarrayControlImpl::makeObsBlockId(ObsblockGroup& obGroup)
{
  /* ostringstream o;
    o << project_  << "." ;
    o << obsblock_ << "." ;
    if ( ! subObsblock_.empty() ) {
        o << subObsblock_ << ".";
    }
    o << trial_;
    string obid ( o.str() );
    return obid;*/
    ostringstream o;
    o << obGroup.project << ".";
    o << obGroup.obsblock << "."; 
    if(!obGroup.subObsblock.empty()){
      o << obGroup.subObsblock << ".";
    }
    o << obGroup.trial;
    string obid (o.str());
    return obid;
}

void
SubarrayControlImpl::setIntent ( const char* sourcename,
                     const char* purpose,
                     bool selfcal,
                     bool fastSwitch
                       )
try {
    if (subarrayNo_ > 2) {
        cmdlog() << "setIntent command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }
    cmdlog() << "setIntent( src=" << sourcename
             << " , purpose=" << purpose
             << " , selfcal=" << boolalpha << selfcal
             << " , fastSwitch=" << boolalpha << fastSwitch
             <<  ")";

    const string supper
        = StringUtils::lowASCIIAlphaNumericToUpper( sourcename );
    const string pupper
        = StringUtils::lowASCIIAlphaNumericToUpper( purpose );

    if ( ! isValidPurpose( pupper ) ) {
        ostringstream o;
        o << "Invalid purpose " << pupper
          << " for source "
          << supper
          << ". Valid purposes are A, B, G, F, P, R, S, O and combinations thereof with no spaces.";
        throw CARMA_EXCEPTION( UserException,  o.str().c_str());
    }

    // make sure we have no repeated chars, adjacent or otherwise
    const string puniq = StringUtils::reallyUniq( pupper );

    // sort the string by ascending characters so that MIRIAD
    // will always have a sorted string for selection.
    const string sortedPurpose = StringUtils::sort( puniq,
                                              StringUtils::ASCENDING_SORT
                              );
    unsigned short i;
    
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    if(ob.corrType == carma::util::CORR_NONE)
      return;
    
    try {
        // check first that the source is not already in this obsblock.
        i = getObsblockIndexOf( supper );
            cmdlog() << "Found " << supper << " in obsblock " << makeObsBlockId(ob)
                 << " at index " << i;
    } catch ( const NotFoundException & nfe ){
        // source is not in obsblock. add it.
        i = getNextAvailableObsblockIndex( );
            cmdlog() << "Adding " << supper << " to obsblock " << makeObsBlockId(ob)
                 << " at index " << i;
    }

    CorrelatorSet corrSet(ob.corrType);
    std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

    for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
      carma::monitor::Obsblock::ObsObject & s = getObsblockMp(corrs[iCorr]).obsObject( i );
      s.setValidity( MonitorPoint::VALID_GOOD );
      s.name().setValue ( supper );
      s.name().setValidity( MonitorPoint::VALID_GOOD );
      s.purpose().setValue ( sortedPurpose );
      s.purpose().setValidity( MonitorPoint::VALID_GOOD );
      s.selfCalibratable().setValue( selfcal );
      s.selfCalibratable().setValidity( MonitorPoint::VALID_GOOD );
      s.fastSwitch().setValue( fastSwitch );
      s.fastSwitch().setValidity( MonitorPoint::VALID_GOOD );
    }
    return;

} catch ( ... ) {
    rethrowCaughtAsUser();
}


//@todo move this to IntentInfo class
bool
SubarrayControlImpl::isValidPurpose( const string & purpose )
{
    // check for empty, since it is a valid subset of any string
    // and we need to weed it out before the call to containsOnly().
    if ( purpose.empty() ) return false;
    return StringUtils::containsOnly( purpose, kVALID_PURPOSES );
}

void
SubarrayControlImpl::setIntent( const string & sourceName,
                                const IntentInfo & intent )
{
    setIntent( sourceName.c_str(),
               intent.purpose.c_str(),
               intent.selfcal,
               intent.fastSwitch
             );
}


void setSpecificDefaultIntent(carma::monitor::Obsblock & ob, 
                              const string supper, 
                              const string sortedPurpose, 
                              const IntentInfo& intent)
{
    ob.setValidity( MonitorPoint::VALID_GOOD );
    ob.defaultName().setValue( supper );
    ob.defaultName().setValidity( MonitorPoint::VALID_GOOD );
    ob.defaultPurpose().setValue( sortedPurpose );
    ob.defaultPurpose().setValidity( MonitorPoint::VALID_GOOD );
    ob.defaultSelfcal().setValue( intent.selfcal );
    ob.defaultSelfcal().setValidity( MonitorPoint::VALID_GOOD );
    ob.defaultFastswitch().setValue( intent.fastSwitch );
    ob.defaultFastswitch().setValidity( MonitorPoint::VALID_GOOD );
}

void
SubarrayControlImpl::setDefaultIntent( const IntentInfo & intent )
try {
    const string supper
        = StringUtils::lowASCIIAlphaNumericToUpper( sourceName() );
    const string pupper
        = StringUtils::lowASCIIAlphaNumericToUpper( intent.purpose );

    // We really shouldn't have to do these tests, since
    // default intents are completely under programmer control.
    // But what the hey.
    if ( ! isValidPurpose( pupper ) ) {
        ostringstream o;
        o << "Invalid default purpose [" << pupper
          << "]. This is a stupid programmer error: Contact Marc Pound!";
        throw CARMA_EXCEPTION( UserException,  o.str().c_str());
    }

    // make sure we have no repeated chars, adjacent or otherwise
    const string puniq = StringUtils::reallyUniq( pupper );

    // sort the string by ascending characters so that MIRIAD
    // will always have a sorted string for selection.
    const string sortedPurpose 
        = StringUtils::sort( puniq, StringUtils::ASCENDING_SORT);
    ControlCorrelatorDesignation corr = getCorrelatorDesignation();

    CorrelatorSet corrSet(corr);
    std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();
    for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
      carma::monitor::Obsblock& obs = getObsblockMp(corrs[iCorr]);
      setSpecificDefaultIntent(obs, supper, sortedPurpose, intent);
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
SubarrayControlImpl::setConstraints(
        ImagingType imgVsSnr,
            const CORBA::Short minAnts,
        const float calMaxTime,
        const float calMaxRms,
        const float maxTsys,
        const float maxRmsPathLength,
        const float maxTau,
        const float maxDecor,
        const float requiredRms
        )
try {
    if (subarrayNo_ > 2) {
        cmdlog() << "setConstraints command issued in subarray#" << subarrayNo_
                 << " and is being ignored";
        return;
    }
    cmdlog() << "setConstraints( ImgVsSNR =" << imgVsSnr
             << " , minAnts =" << minAnts
             << " , calMaxTime (minutes) =" << calMaxTime
             << " , calMaxRms (Jy) =" << calMaxRms
             << " , maxTsys (K) =" << maxTsys
             << " , maxRmsPath (micron) =" << maxRmsPathLength
             << " , maxTau (225GHz) =" << maxTau
             << " , maxDecor =" << maxDecor
             << " , reqRMS (Jy) =" << requiredRms
             <<  ")";

    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    CorrelatorSet corrSet(ob.corrType);
    std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

    for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
      carma::monitor::Obsblock::Constraints& cs = getObsblockMp(corrs[iCorr]).constraints();

      switch ( imgVsSnr ) {
      case IMG:
        cs.imgVsSnr().setValue(
                       carma::monitor::Obsblock::ImgVsSnrMonitorPointEnum::IMG
                       );
        break;
      case SNR:
        cs.imgVsSnr().setValue(
                       carma::monitor::Obsblock::ImgVsSnrMonitorPointEnum::SNR
                       );
        break;
      }
      
      cs.minAnts().setValue( minAnts );
      cs.gainCalMaxTime().setValue( calMaxTime );
      cs.gainCalMaxRms().setValue( calMaxRms );
      cs.maxTsys().setValue( maxTsys );
      cs.maxRmsPathLength().setValue( maxRmsPathLength );
      cs.maxTau230().setValue( maxTau );
      cs.maxDecorrelationRatio().setValue( maxDecor );
      cs.requiredSourceRms().setValue( requiredRms );
    }

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
SubarrayControlImpl::setDefaultConstraints( void )
try {
   // These values are meant to be extremes such that anything
   // will meet the criteria, unless overridden by the observing script.
   // They were chosen by Doug F.
    setConstraints( SNR, 3, 100.0, 100.0, 2.0E5, 1.0E5, 1.0E5, 1.0, 1.0E-5 );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void SubarrayControlImpl::initializeObsblock( void )
{
    if (subarrayNo_ > 2) {
        cmdlog() << "initializeObsblock command issued in subarray#"
                 << subarrayNo_
                 << " and is being ignored";
        return;
    }

    return;
}

void
SubarrayControlImpl::resetProjectAndObsblock( void )
try {
    if (subarrayNo_ > 2) {
        cmdlog() << "resetProjectAndObsblock command issued in subarray#"
                 << subarrayNo_
                 << " and is being ignored";
        return;
    }
    setObsblock(NO_PROJECT.c_str(),STANDBY.c_str(),"",1);

    invalidateIntent();
    invalidateConstraints();
} catch ( ... ) {
        rethrowCaughtAsUser();
}

bool
SubarrayControlImpl::isDefaultProjectOrObsblock()
{
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());
    if ( StringUtils::equalsIgnoreCase( ob.project, services::NO_PROJECT ) )
        return true;
    if ( StringUtils::equalsIgnoreCase( ob.obsblock, STANDBY ) )
        return true;
    // let's not allow sub-obsblock to be standby either
    if ( StringUtils::equalsIgnoreCase( ob.subObsblock, STANDBY ) )
        return true;

    return false;
}

bool
SubarrayControlImpl::isThrowawayObsblock()
{
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());
    bool standby = StringUtils::equalsIgnoreCase( ob.obsblock, STANDBY ) ;
    bool okproj = false;
    if ( StringUtils::equalsIgnoreCase( ob.project, WEATHER )
       ||
         StringUtils::equalsIgnoreCase( ob.project, MAINTENANCE )
       ||
         StringUtils::equalsIgnoreCase( ob.project, NO_PROJECT ) )
     okproj = true;

    return  ( okproj && standby && ob.subObsblock.empty() );
}

bool
SubarrayControlImpl::hasDefaultIntent( const string & project )
{
    const string pupper = StringUtils::lowASCIIAlphaNumericToUpper( project );
    return (   projectDefaultIntentMap_.find( pupper )
        != projectDefaultIntentMap_.end()
        );
}

bool
SubarrayControlImpl::isCommissioning( const char * project )
try {
    const string p( project );
    if ( hasDefaultIntent(p) )
        return true;

    const string CS("CS"); // summer school projects
    const string projectAlpha = p.substr(0,2); // first two chars
    const string projectNumeric = p.substr(2);   // char 3 to end of string
    // a valid numeric part of the project must be
    // exactly PROJECT_NUMERIC_CHARS in length
    const size_t length = projectNumeric.length();
    bool startsWithCS = StringUtils::equalsIgnoreCase( projectAlpha, CS );
    // must be csNNN with reasonable numeric value.
    if ( startsWithCS ) {
        // stringToInt will throw if bad conversion.
        int numericCode = StringUtils::stringToInt( projectNumeric );
        bool isValidNumeric = ( numericCode > 0 )
                          && ( length == PROJECT_NUMERIC_CHARS );
        return ( isValidNumeric );
    }

    // must be ctNNN with restricted numeric range.
    const string CT("CT"); // commissioning projects
    bool startsWithCT = StringUtils::equalsIgnoreCase( projectAlpha, CT );
    if ( startsWithCT ) {
        // stringToInt will throw if bad conversion.
        int numericCode = StringUtils::stringToInt( projectNumeric );
        bool isCorrectNumeric =
        ( numericCode <= MAX_COMMISSIONING_NO && numericCode > 0 );
        bool isValidNumeric = isCorrectNumeric
                          && ( length == PROJECT_NUMERIC_CHARS );
        return isValidNumeric;
    }

    //@ToDO add CX projects for 1cm once we know the range.

    return false;
} catch ( ... ) {
    return false;
}

const IntentInfo &
SubarrayControlImpl::getDefaultIntent( const string & project )
{
    //ScopedLogNdc ndc("SacI::getDefaultIntent");
    if ( hasDefaultIntent( project ) ) {
    // note, keys are upper case.  If you use lower case here
    // a key/value will be pair will be *inserted* with the
    // default constructor of IntentInfo.
        const string pupper
            = StringUtils::lowASCIIAlphaNumericToUpper( project );
        return projectDefaultIntentMap_[pupper];
    }

    ostringstream os;
    os << "Could not find default intent for project " << project ;
    throw CARMA_EXCEPTION( UserException, os.str().c_str() );
}

ControlCorrelatorDesignation SubarrayControlImpl::getCorrelatorDesignation( void )
{
  carmaMonitor_.readNewestConditionalCopy();
  return static_cast<ControlCorrelatorDesignation>(carmaMonitor_.signalPath().mapping().subarray(subarrayNo_ - 1).CORRELATOR_DESIGNATION_MP().getValue());
}

void
SubarrayControlImpl::invalidateSpecificIntent(carma::monitor::Obsblock & ob)
{
    unsigned short count = ob.obsObjectCount();
    for (unsigned short i = 0; i < count ; i++) {
        ob.obsObject( i ).name().setValue( kUNUSED );
    ob.obsObject( i ).purpose().setValue( invalidIntent_.purpose );
        ob.obsObject( i ).selfCalibratable().setValue( invalidIntent_.selfcal );
    ob.obsObject( i ).fastSwitch().setValue( invalidIntent_.fastSwitch );
        ob.obsObject( i ).setValidity( MonitorPoint::INVALID_NO_DATA );
    }

    ob.defaultName().setValue( kUNUSED );
    ob.defaultPurpose().setValue( invalidIntent_.purpose );
    ob.defaultSelfcal().setValue( invalidIntent_.selfcal );
    ob.defaultFastswitch().setValue( invalidIntent_.fastSwitch );
    ob.setValidity( MonitorPoint::INVALID_NO_DATA );
}

void
SubarrayControlImpl::invalidateIntent( void )
{
    ControlCorrelatorDesignation corr = getCorrelatorDesignation();

    CorrelatorSet corrSet(corr);
    std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();
    for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
      carma::monitor::Obsblock& obs = getObsblockMp(corrs[iCorr]);
      invalidateSpecificIntent(obs);
    }
    return;
}

void
SubarrayControlImpl::invalidateConstraints( void )
{
    ControlCorrelatorDesignation corr = getCorrelatorDesignation();

    CorrelatorSet corrSet(corr);
    std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

    for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
      getObsblockMp(corrs[iCorr]).constraints().setValidity(MonitorPoint::INVALID_NO_DATA);
    }
}

bool
SubarrayControlImpl::obsblockContains( const string & source )
{
    try {
        unsigned short i = getObsblockIndexOf( source );
        i++; // stifle the compiler warning
        return true;
        } catch ( const NotFoundException & nfe ) {
        return false;
    }
}

bool
SubarrayControlImpl::currentSourceHasIntent( void )
{
    return obsblockContains( sourceName() );
}

unsigned short
SubarrayControlImpl::getObsblockIndexOf( const string & source )
{
    unsigned short count = 0;
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    CorrelatorSet corrSet(ob.corrType);
    carma::monitor::Obsblock& obsblock = getObsblockMp(corrSet.getFirstControlCorrelatorDesignation());
    count = obsblock.obsObjectCount(); 

    for (unsigned short i = 0; i < count ; i++) { 
      const string name = obsblock.obsObject( i ).name().getValue();
      if ( StringUtils::equalsIgnoreCase( source , name ) ) 
        return i; 
    } 

    const string supper = StringUtils::lowASCIIAlphaNumericToUpper( source );
    ostringstream os;
    os << "Source " << supper << " not found in obsblock " << makeObsBlockId(ob);
    throw CARMA_EXCEPTION( util::NotFoundException, os.str().c_str() );

}

void
SubarrayControlImpl::setObsblockCurrentObsObject( void )
{

  //if ( setDefaultObsObject() ) return;

    const string localSourceName = sourceName();

    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    // First we check if the local source name exists in the
    // obsobject container.  If it does, then that entry takes precedence
    // over default intents.  If not, check if default intents are
    // allowed for this project before throwing an exception.
    try {
        unsigned short i = getObsblockIndexOf( localSourceName );

        CorrelatorSet corrSet(ob.corrType);
        std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

        for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
          getObsblockMp(corrs[iCorr]).currentObsObject().setValue(i);
          getObsblockMp(corrs[iCorr]).currentObsObject().setValidity(MonitorPoint::VALID_GOOD);
        }
    } catch ( const util::NotFoundException & nfe ) {

        if ( setDefaultObsObject() ) return;

        ostringstream o;
        o << "Unable to set current sky object in obsblock "
          << makeObsBlockId(ob)
          << " because " << localSourceName
          << " is not in the obsblock.  Fix this by calling "
          << "intent( '"<< localSourceName <<"', ... )";
        throw CARMA_EXCEPTION( UserException, o.str().c_str() );
    }
}

bool
SubarrayControlImpl::setDefaultObsObject( void )
try {
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());
    if ( hasDefaultIntent( ob.project ) ) {

      ControlCorrelatorDesignation corr = getCorrelatorDesignation();

      CorrelatorSet corrSet(corr);
      std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

      for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {
        carma::monitor::Obsblock& obs = getObsblockMp(corrs[iCorr]);
        obs.currentObsObject().setValue( -1 );
        obs.currentObsObject().setValidity( MonitorPoint::VALID_GOOD );
        obs.setValidity( MonitorPoint::VALID_GOOD );
      }
      return true;
    }
    return false;
} catch ( ... ) {
    programLogWarnIfPossible( "Problem setting CurrentObsObject index to default.");
    return false;
}



unsigned short
SubarrayControlImpl::getNextAvailableObsblockIndex( void )
{
    unsigned short count = 0;
    ObsblockGroup ob = getObsblock(getCorrelatorDesignation());

    CorrelatorSet corrSet(ob.corrType);
    carma::monitor::Obsblock& obs = getObsblockMp(corrSet.getFirstControlCorrelatorDesignation());

    count = obs.obsObjectCount();
    for (unsigned short i = 0; i < count ; i++) {
      string name = obs.obsObject( i ).name().getValue(); 
      if ( StringUtils::equalsIgnoreCase( kUNUSED , name) || name.empty()) 
        return i; 
    } 

    // if we've gotten here, they have tried to put more than
    // 32 sources into their obsblock, which is pretty extreme.
    ostringstream o;
    o << "No space left in obsblock "
      << makeObsBlockId(ob) << ". Limit is 32 sources.";
    throw CARMA_EXCEPTION( util::NotFoundException , o.str().c_str() );

}

void
SubarrayControlImpl::initializeDefaultProjectMap()
{

    //ScopedLogNdc ndc("SacI::initializeDefaultProjectMap()");
    // clear all the elements in case it has been previously
    // initialized at some point.
    projectDefaultIntentMap_.clear();
    for ( unsigned short i = 0 ; i < NUM_INTENTS; i++ ) {
        projectDefaultIntentMap_.insert(
            make_pair( PROJECT_NAMES[i], DEFAULT_INTENTS[i] )
            );
    }
}

/**.......................................................................
 * Return the appropriate project() monitor point for this correlator
 * designation
 */
carma::monitor::MonitorPointString& SubarrayControlImpl::getProjectMpForRead(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getProjectMp called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getProjectMp called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return carmaMonitor_.control().spectralLineCorrelator().project();
  }

  if(corrSet.isWideband()) {
    return carmaMonitor_.control().widebandCorrelator().project();
  }

  if(corrSet.isC3gMax8()) {
    return carmaMonitor_.control().c3gMax8Correlator().project();
  }

  if(corrSet.isC3gMax23()) {
    return carmaMonitor_.control().c3gMax23Correlator().project();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getProjectMpForRead called with invalid correlator designation");
  return carmaMonitor_.control().spectralLineCorrelator().project();
}

/**.......................................................................
 * Return the appropriate project() monitor point for this correlator
 * designation
 */
carma::monitor::MonitorPointString& SubarrayControlImpl::getProjectMp(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getProjectMp called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getProjectMp called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return controlSubsystem_.spectralLineCorrelator().project();
  }

  if(corrSet.isWideband()) {
    return controlSubsystem_.widebandCorrelator().project();
  }

  if(corrSet.isC3gMax8()) {
    return controlSubsystem_.c3gMax8Correlator().project();
  }

  if(corrSet.isC3gMax23()) {
    return controlSubsystem_.c3gMax23Correlator().project();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getProjectMp called with invalid correlator designation");
  return controlSubsystem_.spectralLineCorrelator().project();
}

/**.......................................................................
 * Return the appropriate obsBlockId() monitor point for this correlator
 * designation
 */
carma::monitor::MonitorPointString& SubarrayControlImpl::getObsblockIdMp(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getObsblockIdMp called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getObsblockIdMp called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return controlSubsystem_.spectralLineCorrelator().obsBlockId();
  }

  if(corrSet.isWideband()) {
    return controlSubsystem_.widebandCorrelator().obsBlockId();
  }

  if(corrSet.isC3gMax8()) {
    return controlSubsystem_.c3gMax8Correlator().obsBlockId();
  }

  if(corrSet.isC3gMax23()) {
    return controlSubsystem_.c3gMax23Correlator().obsBlockId();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getObsblockIdMp called with invalid correlator designation");
  return controlSubsystem_.spectralLineCorrelator().obsBlockId();
}

/**.......................................................................
 * Return the appropriate obsBlockId() monitor point for this correlator
 * designation
 */
carma::monitor::MonitorPointString& SubarrayControlImpl::getObsblockIdMpForRead(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getObsblockIdMpForRead called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getObsblockIdMpForRead called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return carmaMonitor_.control().spectralLineCorrelator().obsBlockId();
  }

  if(corrSet.isWideband()) {
    return carmaMonitor_.control().widebandCorrelator().obsBlockId();
  }

  if(corrSet.isC3gMax8()) {
    return carmaMonitor_.control().c3gMax8Correlator().obsBlockId();
  }

  if(corrSet.isC3gMax23()) {
    return carmaMonitor_.control().c3gMax23Correlator().obsBlockId();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getObsblockIdMp called with invalid correlator designation");
  return carmaMonitor_.control().spectralLineCorrelator().obsBlockId();
}

/**.......................................................................
 * Return the appropriate obsBlock() monitor point for this correlator
 * designation
 */
carma::monitor::Obsblock& SubarrayControlImpl::getObsblockMp(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getObsblockMp called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getObsblockMp called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return controlSubsystem_.spectralLineCorrelator().obsblock();
  }

  if(corrSet.isWideband()) {
    return controlSubsystem_.widebandCorrelator().obsblock();
  }

  if(corrSet.isC3gMax8()) {
    return controlSubsystem_.c3gMax8Correlator().obsblock();
  }

  if(corrSet.isC3gMax23()) {
    return controlSubsystem_.c3gMax23Correlator().obsblock();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getObsblockMp called with invalid correlator designation");
  return controlSubsystem_.spectralLineCorrelator().obsblock();
}

/**.......................................................................
 * Return the appropriate obsBlock() monitor point for this correlator
 * designation
 */
carma::monitor::MonitorPointString& SubarrayControlImpl::getModeDescMp(const ControlCorrelatorDesignation ctype)
{
  CorrelatorSet corrSet(ctype);

  if(corrSet.isEmpty()) {
    ThrowCarmaError("getModeDescMp called with CORR_NONE");
  }

  if(!corrSet.isSingleCorrelator()) {
    ThrowCarmaError("getModeDescMp called with multiple correlator designation");
  }

  if(corrSet.isSpectral()) {
    return controlSubsystem_.spectralLineCorrelator().modeDesc();
  }

  if(corrSet.isWideband()) {
    return controlSubsystem_.widebandCorrelator().modeDesc();
  }

  if(corrSet.isC3gMax8()) {
    return controlSubsystem_.c3gMax8Correlator().modeDesc();
  }

  if(corrSet.isC3gMax23()) {
    return controlSubsystem_.c3gMax23Correlator().modeDesc();
  }

  // We'll never get here -- just to shut up compiler warnings

  ThrowCarmaError("getModeDescMp called with invalid correlator designation");
  return controlSubsystem_.spectralLineCorrelator().modeDesc();
}

/**.......................................................................
 * Method to set the obsblock id so that we don't have huge duplicaed
 * blocks of unparsable code everywhere throughout this file
 */
bool SubarrayControlImpl::setObsblockIdProject(ObsblockGroup& ob)
{
  return setObsblockIdItem(ob, true, false, false, false);
}

bool SubarrayControlImpl::setObsblockIdObsblock(ObsblockGroup& ob)
{
  return setObsblockIdItem(ob, false, true, false, false);
}

bool SubarrayControlImpl::setObsblockIdSubobsblock(ObsblockGroup& ob)
{
  return setObsblockIdItem(ob, false, false, true, false);
}

bool SubarrayControlImpl::setObsblockIdTrial(ObsblockGroup& ob)
{
  return setObsblockIdItem(ob, false, false, false, true);
}

bool SubarrayControlImpl::setObsblockIdItem(ObsblockGroup& obSave, 
                        bool overwriteProject, 
                        bool overwriteObsblock, 
                        bool overwriteSubobsblock, 
                        bool overwriteTrial)
{

    ObsblockGroup ob = obSave;

    CorrelatorSet corrSet(ob.corrType);

    if(corrSet.isEmpty()) {
      return false;
    } else {
      
      std::vector<ControlCorrelatorDesignation> corrs = corrSet.getControlCorrelatorDesignations();

      for(unsigned iCorr=0; iCorr < corrs.size(); iCorr++) {

        ob = getObsblock(corrs[iCorr]);

        if (overwriteProject) {
          ob.project = obSave.project;
          getProjectMp(corrs[iCorr]).setValue(ob.project);
        } 

        if (overwriteObsblock) {
          ob.obsblock = obSave.obsblock;
        } 

        if (overwriteSubobsblock) {
          ob.subObsblock = obSave.subObsblock;
        } 

        if (overwriteTrial) {
          ob.trial = obSave.trial;
        }

        const string obid = makeObsBlockId(ob);
        getObsblockIdMp(corrs[iCorr]).setValue( obid );
      }

      // Set the subarray obsBlockId to the last value.

      if(overwriteProject) {
        controlSubsystem_.subarray(subarrayNo_ - 1).project().setValue( ob.project );
        controlSubsystem_.subarray(subarrayNo_ - 1).obsBlockId().setValue( makeObsBlockId(ob) );
      }
    }


    return true;
}

/**.......................................................................
 * Generic string validity check so that we don't have massively
 * duplicated blocks of code making the file unreadable
 */
void SubarrayControlImpl::checkStringValidity(std::string& str, std::string prefix, bool checkWhitespace)
{
  if(containsDot(str))
    ThrowCarmaError(prefix << " string may not contain a dot.");
 
  if(containsAsterisk(str))
    ThrowCarmaError(prefix << " string may not contain an asterisk.");

  if(containsQuote(str))
    ThrowCarmaError(prefix << " string may not contain quote marks.");
  
  if(checkWhitespace) {
    if ( StringUtils::isEmptyOrContainsWhiteSpace(str) )
      ThrowCarmaError(prefix << " string may not be empty or contain blanks.");
  }
  
  if(carma::control::containsReserveWords(StringUtils::lowASCIIAlphaNumericToLower(str))) {
    ThrowCarmaError(prefix << " string may not contain the reserved words: "
                << carma::control::listReserveWords());
  }
}
