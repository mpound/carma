/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.5 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: BimaDewarRegulationImpl.cc,v 1.5 2012/02/21 21:06:58 abeard Exp $
 */


// CARMA includes
#include "carma/antenna/bima/control/BimaDewarRegulationImpl.h"
#include "carma/antenna/bima/control/BimaDewarRegulation.h"
#include "carma/antenna/bima/control/BimaDewarRegulation_skel.h"
#include "carma/antenna/bima/control/BimaDewarRegulation_skel_tie.h"

#include "carma/corba/Server.h"

#include <cmath>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

#define NMAX 20

BimaDewarRegulationImpl::BimaDewarRegulationImpl(
    DewarRegulation &dereg,
    bool emulate ) :
  _dereg( dereg ),
  _log( Program::getLogger() ),
  _server( Program::getProgram().getCorbaServer() )
{
  //  if ( server ) 
  //    init(); // init shared memory

  CPTRACE( Trace::TRACE2, "Constructing BimaDewarRegulationImpl Obj..." );

  _emulate = emulate;

  // Publish their IORs on the nameserver for now...
  ostringstream myName;
  myName << "carma." << _dereg.getDewar().getConfig().getAntenna() << "."
    << carma::antenna::bima::control::DEWARREG_NAME;
  CPTRACE( Trace::TRACE2, "  myName: " << myName.str() );
  
  namespace POA_cabc = POA_carma::antenna::bima::control;
  _server.addServant< POA_cabc::BimaDewarRegulation_tie >( *this, 
                                                           myName.str() );
}

BimaDewarRegulationImpl::~BimaDewarRegulationImpl()
{
}

void BimaDewarRegulationImpl::thread( BimaDewarRegulationImpl &This )
{
  This.run();
}

void BimaDewarRegulationImpl::run()
{
  CPTRACE( Trace::TRACE2, " run( true ) " );

  _server.run( true );

  CPTRACE( Trace::TRACE2, " run( true ) - exited." );
}

void BimaDewarRegulationImpl::on()
{
  try
  {
    CPTRACE( Trace::TRACE2, " on() called " );
    _log << Priority::INFO << " on() called";

    _dereg.on();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

void BimaDewarRegulationImpl::off()
{
  try
  {
    CPTRACE( Trace::TRACE2, " off() called " );
    _log << Priority::INFO << " off() called";

    _dereg.off();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

void BimaDewarRegulationImpl::setPoint( CORBA::Float point )
{
  try
  {
    CPTRACE( Trace::TRACE2, " setPoint( " << point << " ) called " );
    _log << Priority::INFO << " setPoint( << " << point << " ) called";

    _dereg.setPoint( (double)point); // hm, what's the point of float then?
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

void BimaDewarRegulationImpl::defrost()
{
  try
  {
    CPTRACE( Trace::TRACE2, " defrost() called " );
    _log << Priority::INFO << " defrost() called";

    _dereg.defrost();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

void BimaDewarRegulationImpl::cancelDefrost()
{
  try
  {
    CPTRACE( Trace::TRACE2, " cancelDefrost() called " );
    _log << Priority::INFO << " cancelDefrost() called";

    _dereg.cancelDefrost();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// No-op for now, to avoid compiler warnings for the
// main bimaDewarReg program
bool BimaDewarRegulationImpl::isOk()
{
  return true;
}

