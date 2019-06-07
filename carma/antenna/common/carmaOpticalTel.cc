/**
 * @file 
 * This is the primary program for handling the Optical Telescope and 
 * exposing its interface in Distributed Object form.
 *
 * @author Colby Gutierrez-Kraybill
 * @author Andrew Beard
 * $Revision: 1.49 $
 * $Date: 2014/03/19 17:30:31 $
 * $Id: carmaOpticalTel.cc,v 1.49 2014/03/19 17:30:31 scott Exp $
 *
 * @usage carmaOpticalTel [antenna=bima1-9|ovro1-6|sza1-8] [emulate=false]
 * [fginput=0] [fgdev=/dev/video0] [imr=imrhost]
 *
 * @key antenna "bima1"       s Used to uniquely identify this instance, by 
 *                              antenna.
 * @key awdelay .150          d Monitor system write delay in seconds.
 * @key forcetype @noDefault  s Force antenna type incase the 'antenna' keyword
 *                              is set to some name not bima|ovro|sza
 * @key emulate f             b Emulate Frame Grabber hardware with fake star.
 * @key fgdev   "/dev/video0" s Device file name for Frame Grabber board.
 * @key fginput 0             i Input ID on front of Frame Grabber board 0-2 
 *                              (three inputs).
 * @key conf    "antenna/opticaltel.conf" s Configuration file name & location.
 * @logger DEFAULT_FACILITY carma.antenna.common.OpticalTel
 */

// One include to bind them.
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/antenna/common/OpticalTelControl_skel.h"
#include "carma/antenna/common/OpticalTelControl_skel_tie.h"
#include "carma/antenna/common/FrameGrabber.h"

// Strange bedfellows
#include "carma/antenna/bima/OpticalTelControlImpl.h"
#include "carma/antenna/ovro/control/OpticalTelControlImpl.h"

#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"

#include "carma/services/Table.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Trace.h"

#include <iosfwd> // Used for ostringstream

#if 1
#define COUT(statement) \
  {\
  }
#else
#define COUT(statement) \
  {\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    std::cout << _macroOs.str();	\
  }
#endif

using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::antenna;
using namespace carma::antenna::bima;
using namespace carma::antenna::ovro;
using namespace carma::antenna::common;

namespace { // Anonymous namespace for local constants, typedefs & riffraff

    const std::string poaName_( "opticalTelPOA" );

    typedef carma::monitor::AntennaCommon AntCom;

    struct OpticalTelInfo {
        float grossRotationInDegrees;
        float fineRotationInDegrees;
        float azFovInArcminutes;
        float elFovInArcminutes;
    }; 

    // Return an OpticalTelInfo structure for a given antenna...
    OpticalTelInfo getOpticalTelInfo( string antenna,
                                      services::Table & table )
    {
        //COUT("OTI 0");
        OpticalTelInfo info;
        //COUT("OTI 0a ant = " << antenna);
        vector<string> antNames;

        try {
            antNames = table.getColumn("name");
        } catch(carma::util::ErrorException& err) {
            COUT("Caught an error: " << err.what());
        } catch(...) {
            COUT("Caught some other error");
        }
        vector<double> grossRotations = table.getDoubleColumn("gross_rotation");
        vector<double> fineRotations = table.getDoubleColumn("fine_rotation");
        vector<double> azFov = table.getDoubleColumn("az_fov");
        vector<double> elFov = table.getDoubleColumn("el_fov");
        //COUT("OTI 1");
        vector<double>::size_type antIndex = 0;
        vector<string>::const_iterator i = antNames.begin( );
        const vector<string>::const_iterator iEnd = antNames.end( );
        for ( ; i != iEnd; ++i ) {

            if ( *i == antenna ) {
                info.grossRotationInDegrees = grossRotations.at( antIndex );
                info.fineRotationInDegrees  = fineRotations.at( antIndex );
                info.azFovInArcminutes      = azFov.at( antIndex );
                info.elFovInArcminutes      = elFov.at( antIndex );
                CARMA_CPTRACE( Trace::TRACE5, "Retrieved optical tel info "
                    "for " << antenna 
                    << " grossRot=" << info.grossRotationInDegrees
                    << " fineRot=" << info.fineRotationInDegrees
                    << " azFov=" << info.azFovInArcminutes
                    << " elFov=" << info.elFovInArcminutes << "." );
                return info; 
            }
            ++antIndex;
        }

        //COUT("OTI 2");
        ostringstream err;
        err << "Antenna \'" << antenna << "\' not found in " 
            << table.getPathAndFileName( ) << ".";

        throw CARMA_ERROR( err.str( ) );
    }

    enum AntType { OVRO, BIMA, SZA };

    struct AntInfo {
        AntType type;
        int number;
    };

    AntInfo 
    parseAntennaInfo( const string & antenna, const string & forcetype ) 
    {
        const string::size_type antNoIdx = antenna.find_first_of("123456789");

        if ( antNoIdx == string::npos )  
                throw CARMA_EXCEPTION( 
                        carma::util::IllegalArgumentException, 
                        "Invalid antenna parameter - antenna must be of form "
                        "'bima[1-9]'|'ovro[1-6]'|'sza[1-8]'." );

        const string antType = antenna.substr( 0, antNoIdx ); 
        const string antNumber = antenna.substr( antNoIdx );
        const int antNo = atoi( antNumber.c_str( ) );

        AntInfo result;

        result.number = antNo;

        if ( ( antType == "ovro" || forcetype.find( "ovro" ) != string::npos ) 
             && ( antNo > 0 && antNo <= 6 ) ) {
            result.type = OVRO;
        } else if ( ( antType == "bima" || forcetype.find( "bima" ) != string::npos )
                    && ( antNo > 0 && antNo <= 9 ) ) {
            result.type = BIMA;
        } else if ( ( antType == "sza" || forcetype.find( "sza" ) != string::npos ) 
             && ( antNo > 0 && antNo <= 8 ) ) {
            result.type = SZA;
        } else {
            throw CARMA_EXCEPTION( 
                carma::util::IllegalArgumentException, 
                "Invalid antenna parameter - antenna must be of form "
                "'bima[1-9]'|'ovro[1-6]'|'sza[1-8]'." );
        }

        return result;

    } // parseAntennaInfo

    // Based on input antenna, return an OpticalTelCommon implementation.
    auto_ptr< carma::antenna::common::OpticalTelCommon >  
    getOpticalControl ( 
            const string & antenna,
            monitor::AntennaCommon::OpticalTel & opticalTelMon,
            const AntType type,
            FrameGrabber& fg,
            const OpticalTelInfo  & info,
            const bool simulate)
    {
        if ( type == BIMA ) {

            Configuration config( antenna, Program::getConfDir() ); 

            return auto_ptr< common::OpticalTelCommon >(
                new carma::antenna::bima::OpticalTelControlImpl( 
                    opticalTelMon, fg, config, 
                    info.azFovInArcminutes, 
                    info.elFovInArcminutes, 
                    info.grossRotationInDegrees,
                    simulate ) ); 

        } else if ( type == OVRO ) {
            return auto_ptr< common::OpticalTelCommon >(
                new carma::antenna::ovro::OpticalTelControlImpl(
                    antenna, fg, false, opticalTelMon,
                    info.azFovInArcminutes, 
                    info.elFovInArcminutes, 
                    info.grossRotationInDegrees,
                    simulate,
                    Program::getProgram().getCorbaClient() ) );
	        //COUT("Here 1");
        } else if ( type == SZA ) {
            return auto_ptr< common::OpticalTelCommon >(
                new carma::antenna::common::OpticalTelCommon(
                    opticalTelMon, 
                    fg,
                    info.azFovInArcminutes,
                    info.elFovInArcminutes,
                    info.grossRotationInDegrees,
                    simulate ) );
        } 
        
        throw CARMA_EXCEPTION( carma::util::IllegalArgumentException, 
                               "Invalid antenna.");

    } // End getOpticalControl

    struct WriteMonitorDataThreadArgs {
        
        WriteMonitorDataThreadArgs( 
            OpticalTelCommon & opticalTelCommon,
            monitor::MonitorSubsystem & monitorSubsystem ); 

        OpticalTelCommon & otc;
        monitor::MonitorSubsystem & monSubsys;
        long writeDelayInNanos;
    };

    WriteMonitorDataThreadArgs::WriteMonitorDataThreadArgs( 
        OpticalTelCommon & opticalTelCommon,
        monitor::MonitorSubsystem & monitorSubsystem ) : 
    otc( opticalTelCommon ),
    monSubsys( monitorSubsystem ),
    writeDelayInNanos( 0 ) { };
        
    void writeMonitorDataThread( WriteMonitorDataThreadArgs & args )
    try {
        const int periodFrames = 1;
        FrameAlignedTimer timer( args.writeDelayInNanos, periodFrames );
        timer.ResetNextFireTime( ); 

        while ( !Program::getProgram().imrTerminationRequested()  ) {

            ThreadQuitTestSelf( );
            timer.WaitForNextFireTime( );
            ThreadQuitTestSelf( );

            args.otc.writeMonitorData( );
            args.monSubsys.writeWithoutResettingValidities();
        }

    } catch ( ... ) {
        try {

            if ( !CaughtExceptionIsThreadQuitRequestedError( ) )
                logCaughtAsError( );

        } catch (...) {
            // Stifle
        }
    } // writeMonitorDataThread 
    
} // End namespace <unnamed> 

int Program::main()
{
    //COUT("Here -1");
    try
    {
        const string antenna( getStringParameter( "antenna" ) );
        const string carmaCanonicalName( "carma." + antenna + ".OpticalTel" );
        const string fgdev( getStringParameter( "fgdev" ) );
        const int fginput = getIntParameter( "fginput" );
        const bool emulate = getBoolParameter( "emulate" );
        const double autoWriteDelayInS = getDoubleParameter( "awdelay" );
        const string configParam = getStringParameter( "conf" );
        const string configFilename = ProgramBase::getConfFile( configParam );

        //COUT("Config file anme = " << configFilename);

        services::Table configTable( configFilename );

        //COUT("Here -2 ncols = " << configTable.getNcols());
        //COUT("Here -3a ncols = " << configTable.getNcols());

        OpticalTelInfo telInfo;
        //COUT("Here -3b ncols = " << configTable.getNcols());

        string forcetype;
        if ( parameterWasSpecified( "forcetype" ) )
        {
            forcetype = getStringParameter( "forcetype" );
            telInfo = getOpticalTelInfo( forcetype, configTable );
        }
        else
            telInfo = getOpticalTelInfo( antenna, configTable );

        //COUT("Here -4 ncols = " << configTable.getNcols());

#if 1
        FrameGrabber fg( fgdev, fginput, emulate, &configTable );
#else
        FrameGrabber fg(&configTable);
#endif
        //COUT("Here -4a ncols = " << configTable.getNcols());

        const AntInfo antInfo = parseAntennaInfo( antenna, forcetype );

        const ScopedLogNdc logContext( antenna );

        auto_ptr< monitor::MonitorSubsystem > antennaSubsystem;

        //COUT("Here -5");

        auto_ptr< common::OpticalTelCommon > opticalTel;

        //COUT("Here -6");

        if ( antInfo.type == BIMA ) {
            auto_ptr< monitor::BimaSubsystem > 
                bimaMonSubsys( new monitor::BimaSubsystem(antInfo.number) );

            opticalTel = getOpticalControl ( 
                    antenna,
                    bimaMonSubsys->antennaCommon().opticalTel(),
                    BIMA, fg, telInfo, emulate );

            antennaSubsystem.reset( bimaMonSubsys.release( ) );

        } else if ( antInfo.type == OVRO ) {
            COUT("Here 2");
            auto_ptr< monitor::OvroSubsystem > 
                ovroMonSubsys( new monitor::OvroSubsystem(antInfo.number));

            opticalTel = getOpticalControl ( 
                    antenna,
                    ovroMonSubsys->antennaCommon().opticalTel(),
                    OVRO, fg, telInfo, emulate );

            antennaSubsystem.reset( ovroMonSubsys.release( ) );
            COUT("Here 3");
        } else if ( antInfo.type == SZA ) {

            auto_ptr< monitor::SzaSubsystem > 
                szaMonSubsys( new monitor::SzaSubsystem(antInfo.number));

            opticalTel = getOpticalControl ( 
                    antenna,
                    szaMonSubsys->antennaCommon().opticalTel(),
                    SZA, fg, telInfo, emulate );

            antennaSubsystem.reset( szaMonSubsys.release( ) );

        } else {
            throw CARMA_EXCEPTION( carma::util::IllegalArgumentException, 
                    "Invalid antenna.");
        }

        //COUT("Here 4");

        AutoPthreadQuitAndJoinGroup pthreadQuitter;
        WriteMonitorDataThreadArgs args( *( opticalTel ), 
                                         *( antennaSubsystem.get( ) ) );

        args.writeDelayInNanos = 1000l * 1000l * 
            static_cast<long>( ::round( autoWriteDelayInS * 1000.0 ) );

        pthreadQuitter.insert( 
                StartPthreadWithCopy( 
                    &writeMonitorDataThread,
                    args,
                    "carmaOpticalTel writeMonitorDataThread" ) );
        //COUT("Here 5");

        corba::Server & server = getCorbaServer();

        namespace POA_cac = POA_carma::antenna::common;
        server.addServant< POA_cac::OpticalTelControl_tie >(
                *opticalTel, carmaCanonicalName.c_str() );
        //COUT("Here 6");

        server.run( false );

    } catch (...) { 
        logCaughtAsError( );
        return 1; // MISSION ABORTED
    } 
    // MISSION ACCOMPLISHED!!!
    return 0;
}
