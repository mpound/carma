#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/SisReceiver.h"

#include "carma/canbus/Utilities.h"

using namespace carma::antenna;
using namespace carma::canbus;
using namespace carma::antenna::bima;

SisReceiver::SisReceiver( carma::canbus::nodeType node,
                          carma::canbus::CanOutput & io,
                          carma::monitor::AntennaCommon & antCommon,
                          carma::monitor::StateMonitorPointEnum & state,
                          carma::monitor::SisReceiver & sis,
                          carma::monitor::Xac & xac,
                          carma::antenna::common::AntennaIF & antIF,
                          AntennaNameResolver & anr ) :
    common::SisReceiver( node, io, antCommon, state, sis, xac, antIF ),
    anr_( anr )
{
    _bimaShm = new SharedMemory( anr_.getAntennaName().c_str() );
}

SisReceiver::~SisReceiver( )
{
    delete _bimaShm;
    _bimaShm = NULL;
}

void SisReceiver::processIVCurvePoint( carma::canbus::DataVector & data ) 
{
    // Make a copy of the data vector and pass it to the common version so that
    // it gets processed as well.
    DataVector dataCopy = data;
    common::SisReceiver::processIVCurvePoint( dataCopy );

    int rawjd = (int)dataToUlong( data );
    const short rawVj = dataToShort( data );
    const short rawIj = dataToShort( data );

    // const double LSBS_PER_DAY = 1000000000.0;
    // const double fjd = rawjd / LSBS_PER_DAY; // Fractional julian day.

    const double LSBS_PER_MICROAMP = 10.0;
    // double Ij = rawIj / LSBS_PER_MICROAMP;

    const double LSBS_PER_MILLIVOLT = 1000.0;
    // double Vj = rawVj / LSBS_PER_MILLIVOLT;

    // James uses a jd of 0 to indicate that we are done, you can use this to
    // set a flag in shared memory indicating you're done if you want.
    // const bool done = ( rawjd == 0 );

    // Now stick the points in shared memory

    int npts;
    _bimaShm->getData( "RCP_IVNPTS", &npts) ;      // npts in current scan (zeroed by calling program)
    if (rawjd == 0) {
      _bimaShm->putData( "RCP_IVSCAN", &rawjd );    // if 0, signals that scan is done
      _bimaShm->putData( "RCP_VJ", rcpVj, NMAX );   // NMAX is defined in SisReceiver.h, should = MAX_SCAN_PTS in Rx.h
      _bimaShm->putData( "RCP_IJ", rcpIj, NMAX );
    } else if (npts < NMAX) {			   // too lazy to send error; just ignore npts > NMAX
      rcpVj[npts] = rawVj / LSBS_PER_MILLIVOLT;
      rcpIj[npts] = rawIj / LSBS_PER_MICROAMP;
      npts++;
      _bimaShm->putData( "RCP_IVNPTS", &npts );
    }
}

// intercept blanking frame packet1, store Vj,Ij into bima shared memory
// this saves 0.5 sec relative to readback through the monitor system

void SisReceiver::processBlankingFramePacket1( carma::canbus::DataVector &data )
{

    DataVector dataCopy = data;
    common::SisReceiver::processBlankingFramePacket1( dataCopy );

    float setVj = (float)(dataToShort( data ))/1000.;
    float actualVj = (float)(dataToShort( data ))/1000.;
    float setIj = (float)(dataToShort( data ))/10.;
    float actualIj = (float)(dataToShort( data ))/10.;

    _bimaShm->putData( "RCP_VJSET", &setVj );
    _bimaShm->putData( "RCP_VJACT", &actualVj );
    _bimaShm->putData( "RCP_IJSET", &setIj );
    _bimaShm->putData( "RCP_IJACT", &actualIj );
}


// intercept blanking frame packet3, store lastVg and tuneState to bima shared memory
// this saves 0.5 sec relative to readback through the monitor system

void SisReceiver::processBlankingFramePacket3( carma::canbus::DataVector &data )
{
    DataVector dataCopy = data;
    common::SisReceiver::processBlankingFramePacket3( dataCopy );

    float lastVg = (float)(dataToShort( data ))/1000.;
    //const short ttId = 
      dataToShort( data );
    //const byteType calMonth = 
      dataToUbyte( data );
    //const byteType calDay = 
      dataToUbyte( data );
    //const byteType calYear = 
      dataToUbyte( data );
    int tuneState = (int)dataToUbyte( data );

    _bimaShm->putData( "RCP_VGAP", &lastVg );
    _bimaShm->putData( "RTUNESTATE", &tuneState );
}

