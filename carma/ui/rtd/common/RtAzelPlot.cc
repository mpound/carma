/**
 * Implementation for the RtAzelPlot class
 *
 * @author Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <carma/ui/rtd/common/RtAzelPlot.h>
#include <carma/ui/rtd/common/RTD.pb.h>

#include <carma/util/Trace.h>

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <stdexcept>

#include <unistd.h>


using namespace ::std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::ui::rtd;


RtAzelPlot::RtAzelPlot( const ::size_t maxNumAnts,
                        SourceType &   sourceType,
                        double         wspeed,
                        double         wdir )
    : RtObject()
    , maxNumAnts_( maxNumAnts )
    , fieldWidth_( 6 )  // six characters for each item
    , legitSourcePositionCode_( 'N' )
    , sourceAz_( 90 )
    , sourceEl_( 90 )
    , sourceType_( sourceType )
    , windSpeed_( wspeed )
    , windDir_( wdir )
    , debugCount_( 0 )
{
    scratchOSS_.setf(ios::fixed);
}


void RtAzelPlot::serialize(bool initialize, int fontSize, ::rtdproto::RtObject *rtobj)
{
    ::rtdproto::RtAzelPlot *plot = rtobj->mutable_azelplot();

    // static data
    if (initialize) {
        ::rtdproto::RtAzelPlot::StaticData *sd = plot->mutable_staticdata();

        sd->set_title("Azel Plot");
        sd->set_maxnumants(maxNumAnts_);
        sd->set_fieldwidth(fieldWidth_);
    }

    // dynamic data
    {
        // placeholder until actual dynamic data is needed
        ::rtdproto::RtAzelPlot::DynamicData *dd = plot->mutable_dynamicdata();

        if (sourceType_ == RADEC_SOURCE_TYPE)
            dd->set_plotmode(::rtdproto::RtAzelPlot::PLOTMODE_RADEC);
        else
            dd->set_plotmode(::rtdproto::RtAzelPlot::PLOTMODE_AZEL);

        dd->set_windspeed(windSpeed_);
        dd->set_winddirection(windDir_);
        dd->set_legitsourceposition(legitSourcePositionCode_ == 'Y');
        dd->set_sourceaz(sourceAz_);
        dd->set_sourceel(sourceEl_);

        for (::size_t i = 0; i < actualAntInfos_.size(); i++) {
            ::rtdproto::RtAzelPlot::TelescopeAzEl *tele = dd->add_telescopes();


            tele->set_online(actualAntInfos_.at(i).online());
            tele->set_actaz(actualAntInfos_.at(i).actAz());
            tele->set_actel(actualAntInfos_.at(i).actEl());
        }
    }
}


RtAzelPlot::AntInfo::AntInfo(
    const bool * const   legit, const bool * const   online,
    const double * const reqAz, const double * const reqEl,
    const double * const actAz, const double * const actEl ) :
legit_( legit ),
online_( online ),
reqAz_( reqAz ),
reqEl_( reqEl ),
actAz_( actAz ),
actEl_( actEl ) {
    if ( legit_ == 0 )
        throw runtime_error( "legit_ is NULL in RtAzelPlot::AntInfo c'tor" );

    if ( online_ == 0 )
        throw runtime_error( "online_ is NULL in RtAzelPlot::AntInfo c'tor" );

    if ( reqAz_ == 0 )
        throw runtime_error( "reqAz_ is NULL in RtAzelPlot::AntInfo c'tor" );

    if ( reqEl_ == 0 )
        throw runtime_error( "reqEl_ is NULL in RtAzelPlot::AntInfo c'tor" );

    if ( actAz_ == 0 )
        throw runtime_error( "actAz_ is NULL in RtAzelPlot::AntInfo c'tor" );

    if ( actEl_ == 0 )
        throw runtime_error( "actEl_ is NULL in RtAzelPlot::AntInfo c'tor" );
}


bool
RtAzelPlot::AntInfo::legit( ) const {
    return *legit_;
}


bool
RtAzelPlot::AntInfo::online( ) const {
    return *online_;
}


double
RtAzelPlot::AntInfo::reqAz( ) const {
    return *reqAz_;
}


double
RtAzelPlot::AntInfo::reqEl( ) const {
    return *reqEl_;
}


double
RtAzelPlot::AntInfo::actAz( ) const {
    return *actAz_;
}


double
RtAzelPlot::AntInfo::actEl( ) const {
    return *actEl_;
}


void
RtAzelPlot::addAnt( const bool * const   legit, const bool * const   online,
                    const double * const reqAz, const double * const reqEl,
                    const double * const actAz, const double * const actEl ) {
    if ( actualAntInfos_.size() >= maxNumAnts_ )
        throw runtime_error( "actualAntInfos_ vector is already full" );

    actualAntInfos_.push_back( AntInfo( legit, online,
                                        reqAz, reqEl,
                                        actAz, actEl ) );
}


namespace {


double
calcAzDelta( const double azBasis,
             const double az )
{
    double azDelta = az - azBasis;

    if ( azDelta < -540.0 )
        azDelta += 720.0;
    else if ( azDelta < -180.0 )
        azDelta += 360.0;
    else if ( azDelta > 540.0 )
        azDelta -= 720.0;
    else if ( azDelta > 180.0 )
        azDelta -= 360.0;

    return azDelta;
}


}  // namespace < anonymous >


// Use source posn from all antennae and see if they agree to set legit
// source flag. Also compute a consensus az/el for the source.
void
RtAzelPlot::formSourcePosition( ) {
    CARMA_CPTRACE( Trace::TRACE2, "RtAzelPlot::formSourcePosition begin" );

    legitSourcePositionCode_ = 'N';
    sourceAz_ = 90;
    sourceEl_ = 90;

    bool haveOne = false;

    double azBasis = 0;
    double minAzDelta = 0;
    double maxAzDelta = 0;
    double minEl = 0;
    double maxEl = 0;

    {
        const ::size_t numActuals = actualAntInfos_.size();
        ::size_t actual = 0;

        vector< AntInfo >::const_iterator i = actualAntInfos_.begin();
        const vector< AntInfo >::const_iterator iEnd = actualAntInfos_.end();

        for ( ; i != iEnd; ++i, ++actual ) {
            if ( i->legit() != true )
                continue;

            if ( i->online() != true )
                continue;

            CARMA_CPTRACE( Trace::TRACE3,
                           "ant " << actual << "/" << numActuals <<
                           " is legit and online" );

            const double az = i->reqAz();
            const double el = i->reqEl();

            if ( haveOne == false ) {
                haveOne = true;

                azBasis = az;
                minAzDelta = 0;
                maxAzDelta = 0;

                minEl = el;
                maxEl = el;
            } else {
                const double azDelta = calcAzDelta( azBasis, az );

                if (azDelta < minAzDelta) minAzDelta = azDelta;
                if (azDelta > maxAzDelta) maxAzDelta = azDelta;

                if (el < minEl) minEl = el;
                if (el > maxEl) maxEl = el;
            }
        }
    }

    if ( haveOne == false ) {
        CARMA_CPTRACE( Trace::TRACE2,
                       "RtAzelPlot::formSourcePosition early end" );

        return;
    }

    legitSourcePositionCode_ = 'Y';

    // Not legit if greater than 1 degree of spread in az
    if ( (maxAzDelta - minAzDelta) > 1 )
        legitSourcePositionCode_ = 'N';

    // Not legit if greater than 1 degree of spread in el
    if ( (maxEl - minEl) > 1 )
        legitSourcePositionCode_ = 'N';

    const double avgAz = azBasis + ((maxAzDelta + minAzDelta) / 2);

    if ( avgAz <= -360.0 )
        sourceAz_ = avgAz + 360.0;
    else if ( avgAz >= 360.0 )
        sourceAz_ = avgAz - 360.0;
    else
        sourceAz_ = avgAz;

    sourceEl_ = (maxEl + minEl) / 2;

    CARMA_CPTRACE( Trace::TRACE2, "RtAzelPlot::formSourcePosition end" );
}


void
RtAzelPlot::update( ) {
    // Compute source posn, source legit flag
    formSourcePosition();

    // Rewind buffer
    scratchOSS_.str( "" );

    // One place to the right of the decimal
    scratchOSS_ << setprecision(1);

    // Source type radec?
    scratchOSS_ << ((sourceType_ == RADEC_SOURCE_TYPE) ? 'Y' : 'N'); // Y=radec

    scratchOSS_ << setw(fieldWidth_) << windSpeed_;
    scratchOSS_ << setw(fieldWidth_) << windDir_;

    // Legit source posn?
    scratchOSS_ << legitSourcePositionCode_;

    // Source az, el, wind speed and direction
    scratchOSS_ << setw(fieldWidth_) << sourceAz_;
    scratchOSS_ << setw(fieldWidth_) << sourceEl_;

    const ::size_t numActualAnts = actualAntInfos_.size();

    scratchOSS_ << setw(fieldWidth_) << static_cast< double >( numActualAnts );

    // for each antenna online and actual az/el
    for ( ::size_t i = 0; i < maxNumAnts_; ++i ) {
        bool antOnline = false;

        if ( i < numActualAnts )
            antOnline = actualAntInfos_.at( i ).online();

        scratchOSS_ << (antOnline ? 'Y' : 'N');

        bool haveAzEl = false;
        double az;
        double el;

        if ( (i < numActualAnts) && actualAntInfos_.at( i ).legit() ) {
            az = actualAntInfos_.at( i ).actAz();
            el = actualAntInfos_.at( i ).actEl();
            haveAzEl = true;
        }

        if ( haveAzEl ) {
            scratchOSS_ << setw(fieldWidth_) << az;
            scratchOSS_ << setw(fieldWidth_) << el;
        } else {
            scratchOSS_ << setw(fieldWidth_) << "?";
            scratchOSS_ << setw(fieldWidth_) << "?";
        }
    }

    output_ = scratchOSS_.str();

    // Some debugging code to catch stream translation errors by checking
    // the length of the string produced.

    const ::size_t perfectLength = 2 + 5 * fieldWidth_ +
                                   maxNumAnts_ * (1 + 2 * fieldWidth_);

    if ( output_.size() != perfectLength ) {
        CARMA_CPTRACE( Trace::TRACE1,
                       "RtAzelPlot::formSourcePosition bad update text size" );

        if ( debugCount_ <= 1 ) {
            ofstream ofs;

            ofs.open("/tmp/azelplot.log", ios::app);

            ofs << debugCount_ << " " << perfectLength << "  "
                << output_.size() << "  " << output_ << endl;

            ofs.close();

            debugCount_++;
        }
    }
}


void
RtAzelPlot::setWindSpeed(double windSpeed)
{
    if ( (windSpeed <= -1) || (windSpeed >= 100) )
        windSpeed_ = 0;
    else
	windSpeed_ = windSpeed;
}

void
RtAzelPlot::setWindDirection(double windDirection)
{
    if ( (windDirection <= -361) || (windDirection >= 361) )
        windDir_ = 0;
    else
	windDir_ = windDirection;
}
