#include <iostream>

#include "carma/corba/Client.h"
#include "carma/corba/Server.h"

#include "carma/util/UserException.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/String.h"

#include "carma/correlator/transport/CorrDataRemapper.h"
#include "carma/correlator/transport/CorrDataRemapperControl_skel.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/obsRecord2/CorbaCorrConsumer.h"

#include "carma/signalpath/SignalPathMapperControl_skel.h"

using namespace std;
using namespace sza::util;
using namespace carma::correlator;
using namespace carma::signalpath;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "printconf", "t",   "b", USAGE "True to print final configuration"},
  { "abtest",    "f",   "b", USAGE "True to send mapping for an astroband"},
  { "clear",     "f",   "b", USAGE "True to clear an astroband, if abtest=t"},
  { "band",      "1",   "i", USAGE "If abtest==true, the band to send the mapping for"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

/**.......................................................................
 * Update a single astroband input map
 */
void updateAstroBandInputMap(unsigned astroBandNo,     
			     carma::correlator::CorrDataRemapperControl_var cdrControl,
			     carma::signalpath::SignalPathMapperControl_var spmControl); 

int Program::main(void)
{
  COUT("Here 0");

  COUT("Here 1");
  try {

    //------------------------------------------------------------
    // Get DO references here
    //------------------------------------------------------------

    std::ostringstream os;
    os << "carma.correlatordataremapper";

  COUT("Here 2");
    carma::correlator::CorrDataRemapperControl_var cdrControl = 0;
    cdrControl = getCorbaClient().resolveName<carma::correlator::CorrDataRemapperControl>(os.str());

    os.str("");
    os << "carma.signalpathmapper";
    
  COUT("Here 3");
    carma::signalpath::SignalPathMapperControl_var spmControl = 0;
    spmControl = getCorbaClient().resolveName<carma::signalpath::SignalPathMapperControl>(os.str());
      
    //------------------------------------------------------------
    // If a band was specified, update its mapping now
    //------------------------------------------------------------

    if(Program::getBoolParameter("abtest")) {
      unsigned astroBandNo = Program::getIntParameter("band");

      if(Program::getBoolParameter("clear")) {
	cdrControl->clearAstroBandInputMap(astroBandNo);
      } else {
  COUT("Here 4");
	updateAstroBandInputMap(astroBandNo, cdrControl, spmControl);
      }
    }

  } catch(carma::util::UserException& err) {
    COUT(err.errorMsg);
  } catch(...) {
    COUT("Caught an unknown error");
  }

  COUT("Here 5");
  return 0;
}

/**.......................................................................
 * Update a single astroband input map
 */
void updateAstroBandInputMap(unsigned astroBandNo,     
			     carma::correlator::CorrDataRemapperControl_var cdrControl,
			     carma::signalpath::SignalPathMapperControl_var spmControl) 
{
  carma::signalpath::SignalPathMapperControl::CorrelatorBandSeq* cbs = 
    spmControl->getCorrelatorBands(astroBandNo);
      
  std::vector<CorrDataRemapperControl::AstroBandInput> abVec;
  for(unsigned iCorrBand=0; iCorrBand < cbs->length(); iCorrBand++) {

    carma::signalpath::SignalPathMapperControl::CorrelatorBandInputSeq* cbis = 
      spmControl->getCorrelatorBandInputMap((*cbs)[iCorrBand]);

    for(unsigned iInput = 0; iInput < cbis->length(); iInput++) {

      carma::signalpath::SignalPathMapperControl::CorrelatorBandInput& input = (*cbis)[iInput];

      CorrDataRemapperControl::AstroBandInput abInput;
      abInput.inputNo   = input.aBandInput.inputNo;
      abInput.antennaIF = input.antIF;
	  
      abVec.push_back(abInput);
    }
  }
      
  CorrDataRemapperControl::AstroBandInputSeq abseq;
  abseq.length(abVec.size());

  for(unsigned i=0; i < abVec.size(); i++) {
    abseq[i] = abVec[i];
  }

  cdrControl->updateAstroBandInputMap(astroBandNo, abseq);
}
