/**
 *
 * Implementation for Control monitor subsystem.
 * This is an auto-generated stub to which a developer may have added
 * additional implementation manually.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */
#include <map>
#include <iostream>

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/Antenna.h"
#include "carma/monitor/SignalPath.h"
#include "carma/monitor/Subarray.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/util/corrUtils.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

#if 0
namespace {


MonitorCorrelatorDesignation
getCorrDesignationForCorrelatorType( const CorrelatorType corrType )
{
  switch(corrType) {
  case CORR_NONE:
    return MONITOR_CORR_NONE;
    break;
  case CORR_SPECTRAL:
    return MONITOR_CORR_SPECTRAL;
    break;
  case CORR_WIDEBAND:
    return MONITOR_CORR_WIDEBAND;
    break;
  case CORR_C3GMAX8:
    return MONITOR_CORR_C3GMAX8;
    break;
  case CORR_C3GMAX23:
    return MONITOR_CORR_C3GMAX23;
    break;
  default:
    ThrowCarmaError("corrTypeToCorrDes() - Invalid conversion");
    return MONITOR_CORR_NONE;
  }
  
  // never gonna get to this?
  ostringstream oss;
  
  oss << "Unknown correlator type " << corrType;
  
  throw CARMA_ERROR( oss.str() );
}

}  // namespace < anonymous >
#endif


//------------ ControlSubsystem ------------------------------

ControlSubsystem::ControlSubsystem(SystemFrameBuffer * const buffer) :
    ControlSubsystemBase(buffer)
{
    typedef CorrDesignation CD;

    const int saCount = subarrayCount();

    for ( int i = 0; i < saCount; ++i ) {
        const string subarrayName (getSubarrayName (i+1));
        subarray(i).name().setValue (subarrayName);
        subarray(i).number().setValue (i+1);

        const monitor::Subarray subarrayObj( i + 1 );

        MonitorCorrelatorDesignation cd = monitor::corrTypeToCorrDes( util::CORR_NONE );
        //@TODO Remove monitor::Subarray and do this correctly.
        if (subarrayObj.hasCorrelator())  {
            const Correlator& correlator = subarrayObj.correlator();
            const util::CorrelatorSet corrset(correlator.correlatorType());
            const string moncorrstr = corrset.mpString();
            cd = monitor::corrTypeToCorrDes( corrset.corrType() );
            ostringstream oss;
            if(i == 0 || i == 1){
              oss << "Sci#" << i+1;
            }
            else if(i == 2 || i == 3){
              oss << "Eng#" << i+1;
            }
            else if(i == 4){
              oss << "Maint";
            }
            else{
              oss << "NONE";
            }

            if( corrset.includesSpectral() ) {
              spectralLineCorrelator().controllingSubarray().setValue(oss.str());
            }
            if( corrset.includesWideband() ) {
              widebandCorrelator().controllingSubarray().setValue(oss.str());
            }
            if( corrset.includesC3gMax8() ) {
              c3gMax8Correlator().controllingSubarray().setValue(oss.str());
            }
            if( corrset.includesC3gMax23() ) {
              c3gMax23Correlator().controllingSubarray().setValue(oss.str());
            }
        }
        subarray(i).CORRELATOR_DESIGNATION_MP().setValue(cd);
    }
    /// @TODO - comment out the initializeAntennas() - it will be done in control
    initializeAntennas();
    initializeCorrelators();
}

ControlSubsystem::~ControlSubsystem()
{
    if (debug_) cout << "ControlSubsystem destructor" << endl;
}

// add additional functionality for ControlSubsystem here

ControlSubsystemBase::Subarray *
ControlSubsystem::getSubarray( const string & subarrayName ) const
{
    const int saCount = subarrayCount();

    int  i;
    for ( i = 0; i < saCount; ++i ) {
        Subarray & controller = subarray(i);

        if (controller.name().getValue() == subarrayName)
            break;
    }

    if ( i == saCount )
        return 0;

    return &(subarray(i));
}


ControlSubsystem::Antenna *
ControlSubsystem::antennaByName (const string& antennaName)
{
    int index = -1;
    string lowerCase = StringUtils::lowASCIIAlphaNumericToLower (antennaName);
    for (int i = 0;  i < antennaCount ()  &&  index == -1;  i++)  {
        if (antenna(i).name().getValue() == lowerCase) index = i;
    }

    // Interpret aliases if there was no match
    if (index == -1) {
        string::size_type idx;
        idx = lowerCase.find("10m#");
        if (idx != string::npos)lowerCase.replace(idx, 4, "ovro");
        idx = lowerCase.find("6m#");
        if (idx != string::npos)lowerCase.replace(idx, 3, "bima");
        idx = lowerCase.find("3m#");
        if (idx != string::npos)lowerCase.replace(idx, 3, "sza");

        //cout << "Antname alias: " << lowerCase << endl;
        for (int i = 0;  i < antennaCount ()  &&  index == -1;  i++)  {
            if (antenna(i).name().getValue() == lowerCase) index = i;
        }
    }

    Antenna * ant = 0;
    if (index > -1) {
        ant = &(antenna(index));
    }

     return ant;
}


string
ControlSubsystem::getSubarrayName( const int saNo )
{
    const int saCount = subarrayCount();

    if ( (saNo < 1) || (saNo > saCount) ) {
        string msg;
        {
            ostringstream oss;

            oss << "ControlSubsystem::getSubarrayName: "
                << "Input subarraynumber = " << saNo
                << " is outside of the legal range of 1-" << saCount;

            msg = oss.str();
        }

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    return monitor::Subarray::subarrayName( saNo );
}


string
ControlSubsystem::getSubarrayAlphanumericName( const int saNo )
{
    const int saCount = subarrayCount();

    if ( (saNo < 1) || (saNo > saCount) ) {
        string msg;
        {
            ostringstream oss;

            oss << "ControlSubsystem::getSubarrayAlphanumericName: "
                << "Input subarraynumber = " << saNo
                << " is outside of the legal range of 1-" << saCount;

            msg = oss.str();
        }

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    return monitor::Subarray::subarrayAlphanumericName( saNo );
}


int
ControlSubsystem::getMaintenanceSubarrayNo( )
{
    return ControlSubsystem::subarrayCount();
}


namespace {


class SaAntGroup {
    public:
        explicit SaAntGroup( ControlSubsystem::Subarray & subarray );

        void addAntenna( ControlSubsystem::Antenna & ant );

    private:
        ControlSubsystem::Subarray *       subarray_;
        set< ControlSubsystem::Antenna * > antGroup_;
};


SaAntGroup::SaAntGroup( ControlSubsystem::Subarray & subarray ) :
subarray_( &subarray ),
antGroup_( ) {
}


void
SaAntGroup::addAntenna( ControlSubsystem::Antenna & ant ) {
    antGroup_.insert( &ant );
}


}  // namespace < anonymous >


// protected methods begin here

// Obsolete - the control subsystem will soon do all of this setup
void
ControlSubsystem::initializeAntennas( )
{
    const ScopedLogNdc ndc( "ControlSubsystem::initializeAntennas()" );

    const int antCount = antennaCount();

    int antIndex = 0;

    const int typeCount = monitor::Antenna::numAntennaTypes();
    for ( int type = 1; type <= typeCount; ++type ) {
        const string typeName =
            monitor::Antenna::antennaTypeName( type );

        const int typeNoCount =
            monitor::Antenna::numberOfAntennasOfType( typeName );

        for ( int typeNo = 1; typeNo <= typeNoCount; ++typeNo ) {
            if ( antIndex >= antCount )
                return;

            Antenna & ant = antenna( antIndex );

            {
                const int correctCarmaNo = (antIndex + 1);
                const int antCarmaNo = ant.carmaAntennaNumber().getValue();

                if ( antCarmaNo != correctCarmaNo ) {
                    if ( antCarmaNo != 0 ) {
                        ostringstream oss;

                        oss << "Overwriting bad carma number " << antCarmaNo
                            << " for index " << antIndex
                            << " with a value of " << correctCarmaNo;

                        programLogErrorIfPossible( oss.str() );
                    }

                    ant.carmaAntennaNumber().setValue( correctCarmaNo );
                }
            }

            {
                const string correctName =
                       monitor::Antenna::antennaName( typeName, typeNo );

                const string antName = ant.name().getValue();

                if ( antName != correctName ) {
                    if ( antName.empty() == false ) {
                        ostringstream oss;

                        oss << "Overwriting bad name \"" << antName << "\""
                            << " for index " << antIndex
                            << " with a value of \"" << correctName << "\"";

                        programLogErrorIfPossible( oss.str() );
                    }

                    ant.name().setValue( correctName );
                }
            }

            ++antIndex;
        }
    }
}


void
ControlSubsystem::initializeSpectralLineCorrelator()
{
    const int bandCount =
        ControlSubsystemBase::SpectralLineCorrelator::slcBandCount();

    ControlSubsystemBase::SpectralLineCorrelator& spectralLineCorrelator
                                            = this->spectralLineCorrelator();

    spectralLineCorrelator.numBands().setValue(bandCount);

    Correlator correlator(CORR_SPECTRAL);
    spectralLineCorrelator.name().setValue (correlator.correlatorTypeName());

    spectralLineCorrelator.CORRELATOR_DESIGNATION_MP().
      setValue(corrTypeToCorrDes(CORR_SPECTRAL));

    for (int i = 0;  i < bandCount;  i++)  {
        // This is a different SlcBand object than the SlcBandSubsystem.
        // Ok to leave in.
        SlcBand& slcBand = spectralLineCorrelator.slcBand(i);
        ControlBandPoints&  bandPoints = slcBand.controlBandPoints();
        bandPoints.number().setValue(i+1);
    }
}

void
ControlSubsystem::initializeWidebandCorrelator()
{
    const int bandCount =
        ControlSubsystemBase::WidebandCorrelator::wbcBandCount();

    ControlSubsystemBase::WidebandCorrelator& widebandCorrelator =
                                                   this->widebandCorrelator();

    widebandCorrelator.numBands().setValue(bandCount);

    Correlator correlator(CORR_WIDEBAND);
    widebandCorrelator.name().setValue (correlator.correlatorTypeName());

    widebandCorrelator.CORRELATOR_DESIGNATION_MP().
      setValue(corrTypeToCorrDes(CORR_WIDEBAND));

    for (int i = 0;  i < bandCount;  i++)  {
        WbcBand& wbcBand = widebandCorrelator.wbcBand(i);
        ControlBandPoints&  bandPoints = wbcBand.controlBandPoints();
        bandPoints.number().setValue(i+1);
    }
}

void
ControlSubsystem::initializeC3gMax8Correlator()
{
    const int bandCount =
        ControlSubsystemBase::C3gMax8Correlator::c3gMax8BandCount();

    ControlSubsystemBase::C3gMax8Correlator& c3gMax8Correlator =
                                                   this->c3gMax8Correlator();

    c3gMax8Correlator.numBands().setValue(bandCount);

    Correlator correlator(CORR_C3GMAX8);
    c3gMax8Correlator.name().setValue (correlator.correlatorTypeName());

    c3gMax8Correlator.CORRELATOR_DESIGNATION_MP().
      setValue(corrTypeToCorrDes(CORR_C3GMAX8));

    for (int i = 0;  i < bandCount;  i++)  {
      C3gMax8Band& c3gBand = c3gMax8Correlator.c3gMax8Band(i);
      ControlBandPoints&  bandPoints = c3gBand.controlBandPoints();
      bandPoints.number().setValue(i+1);
    }
}
void
ControlSubsystem::initializeC3gMax23Correlator()
{
    const int bandCount =
        ControlSubsystemBase::C3gMax23Correlator::c3gMax23BandCount();

    ControlSubsystemBase::C3gMax23Correlator& c3gMax23Correlator =
                                                   this->c3gMax23Correlator();

    c3gMax23Correlator.numBands().setValue(bandCount);

    Correlator correlator(CORR_C3GMAX23);
    c3gMax23Correlator.name().setValue (correlator.correlatorTypeName());

    c3gMax23Correlator.CORRELATOR_DESIGNATION_MP().
      setValue(corrTypeToCorrDes(CORR_C3GMAX23));

    for (int i = 0;  i < bandCount;  i++)  {
      C3gMax23Band& c3gBand = c3gMax23Correlator.c3gMax23Band(i);
      ControlBandPoints&  bandPoints = c3gBand.controlBandPoints();
      bandPoints.number().setValue(i+1);
    }
}

void
ControlSubsystem::initializeCorrelators()
{
    initializeSpectralLineCorrelator();
    initializeWidebandCorrelator();
    initializeC3gMax8Correlator();
    initializeC3gMax23Correlator();
}


set< ControlSubsystemBase::Antenna * >
ControlSubsystem::getSubarrayAntennaGroup( const int saNo ) const
{
    const int saCount = subarrayCount();

    if ( (saNo < 1) || (saNo > saCount) ) {
        string msg;
        {
            ostringstream oss;

            oss << "ControlSubsystem::getSubarrayAntennaGroup: "
                << "Input subarraynumber = " << saNo
                << " is outside of the legal range of 1-" << saCount;

            msg = oss.str();
        }

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    set< ControlSubsystemBase::Antenna * > result;

    const int mpNumAnts = subarray( saNo - 1 ).numberOfAntennas().getValue();
    const int antCount = antennaCount();

    for ( int i = 0; i < antCount; ++i ) {
        Antenna & ant = antenna( i );

        if ( ant.subarrayNumber().getValue() != saNo )
            continue;

        result.insert( &ant );
    }

    if ( static_cast< int >( result.size() ) != mpNumAnts ) {
        if ( saNo != getMaintenanceSubarrayNo() ) {
            ostringstream oss;

            oss << "Size of result " << result.size()
                << " disagrees with number of antennas monitor point value "
                << mpNumAnts << " for subarray #" << saNo;

            programLogWarnIfPossible( oss.str() );
        }
    }

    return result;
}

