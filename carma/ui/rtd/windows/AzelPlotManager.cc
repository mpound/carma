#include "carma/ui/rtd/windows/AzelPlotManager.h"

#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/util/ErrorException.h"


using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


AzelPlotManager::PerAntInfo::PerAntInfo( ) :
legit( false ),
reqAz( 0.0 ),
reqEl( 0.0 ),
actAz( 0.0 ),
actEl( 0.0 )
{
}


AzelPlotManager::PerSaPerAntInfo::PerSaPerAntInfo( ) :
online( false )
{
}


AzelPlotManager::PerSaInfo::PerSaInfo( ) :
sourceType( RtAzelPlot::AZEL_SOURCE_TYPE ),
saPerAntInfo(),
azelPlot()
{
}


AzelPlotManager::AzelPlotManager( MonitorSystem & cms,
                                  const int       maxAntNum,
                                  const int       maxSaNum ) :
cms_( cms ),
maxAntNum_( maxAntNum ),
maxSaNum_( maxSaNum ),
windSpeed_( 0 ),
windDirection_( 0 )
{
    {
        const WeatherSubsystem & wc = cms_.weather();
        
        windSpeed_ = wc.windSpeed().getValue();
        windDirection_ = wc.windDirection().getValue();
    }

    perAntInfo_.resize( maxAntNum_ );

    perSaInfo_.resize( maxSaNum_ );
    {
        vector< PerSaInfo >::iterator i = perSaInfo_.begin();
        const vector< PerSaInfo >::iterator iEnd = perSaInfo_.end();
        
        for ( ; i != iEnd; ++i )
            i->saPerAntInfo.resize( maxAntNum_ );
    }
}


AzelPlotManager::~AzelPlotManager( )
{
}


RtAzelPlotPtr
AzelPlotManager::getAzelPlot( const int saNo )
{
    if ( (saNo < 1) || (saNo > maxSaNum_) ) {
	ostringstream os;
	os  << "Input subarray number "
	    << saNo
	    << " outside valid range: [1,"
	    << maxSaNum_
	    << "]";
        throw CARMA_ERROR( os.str() );
    }
        
    PerSaInfo & saInfo = perSaInfo_.at( saNo - 1 );
        
    if ( saInfo.azelPlot.get() != 0 )
        return saInfo.azelPlot;
        
    RtAzelPlotPtr azelPlotAP( new RtAzelPlot( maxAntNum_,
                                    saInfo.sourceType,
                                    windSpeed_,
                                    windDirection_ ) );

    for ( int antIndex = 0; antIndex < maxAntNum_; ++antIndex ) {
        const PerAntInfo & antInfo = perAntInfo_.at( antIndex );

        azelPlotAP->addAnt( &(antInfo.legit),
                            &(saInfo.saPerAntInfo.at( antIndex ).online),
                            &(antInfo.reqAz),
                            &(antInfo.reqEl),
                            &(antInfo.actAz),
                            &(antInfo.actEl) );
    }
    
    saInfo.azelPlot = azelPlotAP;

    return saInfo.azelPlot;
}


void
AzelPlotManager::updateCommonInfo( )
{
    const WeatherSubsystem & wc = cms_.weather();
    
    windSpeed_ = wc.windSpeed().getValue();
    windDirection_ = wc.windDirection().getValue();
    
    vector< PerSaInfo >::const_iterator i = perSaInfo_.begin();
    const vector< PerSaInfo >::const_iterator iEnd = perSaInfo_.end();
    
    for ( ; i != iEnd; ++i ) {
        RtAzelPlotPtr azelPlot = i->azelPlot;

        if ( azelPlot.get() == 0 )
            continue;
            
        azelPlot->setWindSpeed( windSpeed_ );
        azelPlot->setWindDirection( windDirection_ );
    }
}


void
AzelPlotManager::updatePerAntInfo( )
{
    for ( int antIndex = 0; antIndex < maxAntNum_; ++antIndex ) {
        const AntennaCommon::Track & track =
            cms_.antennaCommon( antIndex ).drive().track();

        const bool azGood = track.requestedAzimuth().isValid();
        const bool elGood = track.requestedElevation().isValid();
        const bool legit = (azGood && elGood);
        
        PerAntInfo & antInfo = perAntInfo_.at( antIndex );
        
        antInfo.legit = legit;

        if ( legit ) {
            antInfo.reqAz = track.requestedAzimuth().getValue();
            antInfo.reqEl = track.requestedElevation().getValue();
            
            antInfo.actAz = track.actualAzimuth().getValue();
            antInfo.actEl = track.actualElevation().getValue();
        }
    }
}


void
AzelPlotManager::updatePerSaInfo( )
{
    const ControlSubsystem & controlSubsys = cms_.control();
        
    for ( int saNo = 1; saNo <= maxSaNum_; ++saNo ) {
        PerSaInfo & saInfo = perSaInfo_.at( saNo - 1 );

        int idleModeAnts = 0;
        int radecModeAnts = 0;
        int azelModeAnts = 0;
        
        for ( int antIndex = 0; antIndex < maxAntNum_; ++antIndex )
            saInfo.saPerAntInfo.at( antIndex ).online = false;

        const set< ControlSubsystem::Antenna * > saAnts =
            controlSubsys.getSubarrayAntennaGroup( saNo );

        set< ControlSubsystem::Antenna * >::const_iterator i =
            saAnts.begin();
            
        const set< ControlSubsystem::Antenna * >::const_iterator iEnd =
            saAnts.end();
        
        for ( ; i != iEnd; ++i ) {
            const ControlSubsystem::Antenna * const ant = *i;
                
            if ( ant == 0 )
                continue;
                    
            const int carmaAntNo = ant->carmaAntennaNumber().getValue();
                
            if ( (carmaAntNo < 1) || (carmaAntNo > maxAntNum_) )
                continue;
                    
            const int antIndex = carmaAntNo - 1;

            saInfo.saPerAntInfo.at( antIndex ).online = true;

            const int antTrackMode = 
                controlSubsys.antenna( antIndex ).trackMode().getValue();
            
            switch ( antTrackMode ) {
                case 0:  ++idleModeAnts;   break;
                case 1:  ++radecModeAnts;  break;
                case 2:  ++azelModeAnts;   break;
            }
        }
        
        if ( (radecModeAnts > 0) && (radecModeAnts > azelModeAnts) )
            saInfo.sourceType = RtAzelPlot::RADEC_SOURCE_TYPE;
        else
            saInfo.sourceType = RtAzelPlot::AZEL_SOURCE_TYPE;
    }
}


void
AzelPlotManager::update( )
{
    updateCommonInfo();
    updatePerAntInfo();
    updatePerSaInfo();
}


