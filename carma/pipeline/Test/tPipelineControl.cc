// $Id: tPipelineControl.cc,v 1.6 2014/05/21 15:25:18 mpound Exp $

#include "carma/corba/corba.h"
#include "carma/corba/Client.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/pipeline/pipelineControl.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Time.h"

#include <unistd.h>
#include <iostream>
#include <math.h>

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;
using namespace carma::pipeline;

static string progName("tPipelineControl");

/**
 *  @description
 *  Sends simple commands to the PipeControl DO
 *
 *  @usage Usage: tPipelineControl  [f=<Drive Control Config Filename>] ant=val [stop=true/false] [az=az(degrees)] [el=el(degrees)] [src=source name] [tiltZeroaf=aftForward] [tiltZerolr=leftRight] [aperture=aperture] [pcAzOff=azOffset] [pzElOff=elOffset] [pcSag=sag] [pcm1=m1] [pcm2=m2] [pcm3=m3] [pcm4=m4] [pcm5=m5]
 *
 *  @key f     correlator/correlator.conf  string  Correlator Config filename (install or build conf dir path will be prepended)
 *  @key sl      true                bool    Spectral Pipeline Control
 *  @key aall    true                bool    Activate All stages
 *  @key adec    false                bool    Activate Decimator stage
 *  @key atsys    false                bool    Activate Tsys stage
 *  @key abf    false                bool    Activate BlankFlag stage
 *  @key dall    false                bool    Deactivate All stages
 *  @key ddec    false                bool    Deactivate Decimator stage
 *  @key dtsys    false                bool    Deactivate Tsys stage
 *  @key dbf    false                bool    Deactivate BlankFlag stage
 *  @key decimate false              bool    Decimate data
 *  @key keepEndChannels true        bool    keep end channels
 *  @key stopint    false                bool  Stop Integration
 *  @key intTime 0.0                double integration Time (sec)
 *  @key numRec  0                  int    number of integration records
 *  @key startint false             bool   start integration
 *
 *  @logger DEFAULT_FACILITY carma.antenna.ovro.drives
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.6 $, $Date: 2014/05/21 15:25:18 $
 */
int Program::main() {
  CorrelatorConfigChecker* ccc =
    CorrelatorConfigChecker::getInstance(getConfFile(getStringParameter("f")));
  ccc->start();
  bool sl =           getBoolParameter("sl");
  bool aall =         getBoolParameter("aall");
  bool adec =         getBoolParameter("adec");
  bool atsys =         getBoolParameter("atsys");
  bool abf =         getBoolParameter("abf");
  bool dall =         getBoolParameter("dall");
  bool ddec =         getBoolParameter("ddec");
  bool dtsys =         getBoolParameter("dtsys");
  bool dbf =         getBoolParameter("dbf");

  bool keepEndChannels = getBoolParameter("keepEndChannels");
  bool decimate =          getBoolParameter("decimate");
  int numRec =       getIntParameter("numRec");
  double intTime =           getDoubleParameter("intTime");
  bool stopint =         getBoolParameter("stopint");
  bool startint =         getBoolParameter("startint");

  try {
    // get object pointer
    carma::pipeline::PipelineControl_var pc;
    corba::Client & client = getCorbaClient();
    if (sl)
      pc = client.resolveName<carma::pipeline::PipelineControl>("carma.slPipelineControl");
    else
      pc = client.resolveName<carma::pipeline::PipelineControl>("carma.wbPipelineControl");
    cerr << "acquired PipelineControl DO..." << endl;
    
    // chec activation of stages
    if (parameterWasSpecified("aall") && aall) { // activate all stages
      pc->activateDecimator();
      pc->activateTsys();
      pc->activateBlankFlag();
    }
    if (parameterWasSpecified("dall") && dall) { // deactivate all stages
      pc->deactivateDecimator();
      pc->deactivateTsys();
      pc->deactivateBlankFlag();
    }
    if (parameterWasSpecified("adec") && adec) {
      pc->activateDecimator();
    }
    if (parameterWasSpecified("atsys") && atsys) {
      pc->activateTsys();
    }
    if (parameterWasSpecified("abf") && abf) {
      pc->activateBlankFlag();
    }
    if (parameterWasSpecified("ddec") && ddec) {
      pc->deactivateDecimator();
    }
    if (parameterWasSpecified("dtsys") && dtsys) {
      pc->deactivateTsys();
    }
    if (parameterWasSpecified("dbf") && dbf) {
      pc->deactivateBlankFlag();
    }
    if (parameterWasSpecified("stopint") && stopint) { // stop integration
      pc->stopIntegration();
    }
    if (parameterWasSpecified("startint") && startint) {
      pc->startIntegration(intTime, 0, numRec, false, 0);
    }
    if (parameterWasSpecified("decimate")) {
      pc->decimate(decimate, 0);
    }
    if (parameterWasSpecified("keepEndChannels")) {
      pc->keepEndChannels(keepEndChannels, 0);
    }
  } catch(const IllegalArgumentException& err) {
    cerr << "IllegalArgumentException: " << err << endl;
  } catch(const ErrorException& err) {
    cerr << "ErrorException: " << err << endl;
  } catch(const CORBA::ORB::InvalidName& err) {
    cerr << "CORBAORBInvalidNameException: " << err << endl;
  } catch(const CORBA::SystemException& err) {
    cerr << "SystemException: " << err << endl;
  } catch(...) {
    cerr << "Exception" << endl;
  }
  return 0;
}
