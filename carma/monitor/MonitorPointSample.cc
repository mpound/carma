/* *
 * @file
 * MonitorPointSample.cc - Contains method definitions for monitor point
 * sample class.
 *
 * @author: N. S. Amarnath
 *
 * @CarmaCopyright@
 */

#include "carma/monitor/MonitorPointSample.h"

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/sanityChecks.h"
#include "carma/util/checking.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

#include <iosfwd>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
compileTimeChecks( )
{
    compileTimeCheck< sizeof( MonitorValue ) == 8 >();

#if __WORDSIZE == 64
    compileTimeCheck< sizeof( MonitorSampleValue ) == 16 >();
#else
    compileTimeCheck< sizeof( MonitorSampleValue ) == 12 >();
#endif
    
}


}  // namespace < anonymous >


//
// MonitorPointSample methods
//


void
MonitorPointSample::setKnownSaneValidityFlags( const uchar saneFlags,
                                               const bool markMpAsModified )
{
    sample_.validityFlags = saneFlags;

    if ( set_ != 0 && markMpAsModified ) {
        CARMA_CHECK( index_ != SubsystemFrame::MONITOR_POINT_ABSENT );
        set_->markMpAtIndexModified( index_ );
    }
}


void
MonitorPointSample::setValidityFlags( const uchar flags )
{
    setKnownSaneValidityFlags( 
        sanityCheckValidityFlags(
            flags,
            0,
            true,
            "MonitorPointSample::setValidityFlags" ) );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const MonitorValue & value,
    const uchar          saneFlags )
{
    const int index = index_;
    sample_.value = value;
    sample_.validityFlags = saneFlags;
    if ( set_ != 0 ) {
        CARMA_CHECK( index_ != SubsystemFrame::MONITOR_POINT_ABSENT );
        set_->markMpAtIndexModified( index );
    }
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const char  value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.byte = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const short value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.sh = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const long  value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.lo = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const bool  value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.bo = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const float value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.fl = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const double value,
    const uchar  saneFlags )
{
    MonitorValue data;

    data.db = value;
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setMonitorValueAndKnownSaneValidityFlags(
    const float value[2],
    const uchar saneFlags )
{
    MonitorValue data;

    data.complex[0] = value[0];
    data.complex[1] = value[1];
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setValueStringChunkAndKnownSaneValidityFlags(
    const MonitorValueStringChunk & c,
    const uchar                     saneFlags )
{
    MonitorValue data;

    for ( size_t i = 0; i < sizeof( data.str ); ++i )
        data.str[ i ] = c.chunkChars[ i ];

    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}


void
MonitorPointSample::setValueSerialNoAndKnownSaneValidityFlags(
    const long  value,
    const uchar saneFlags )
{
    MonitorValue data;

    data.sn = static_cast< long >( value );
    setMonitorValueAndKnownSaneValidityFlags( data, saneFlags );
}

void
MonitorPointSample::fillInTransportSample(
    const CORBA::ULong metaIdx,
    MonitorSampleValues & outSamples,
    const MonitorValueType   mvType,
    const tagIDType          tagId,
    const int                sampleIndex ) const
{
    const uchar saneVf =
        sanityCheckValidityFlags(
            this->getValidityFlags(),
            tagId,
            sampleIndex,
            true,
            "MonitorPointSample::fillInTransportSample" );
    
    outSamples.dataType[metaIdx] = mvType;
    outSamples.pointID[metaIdx] = dbms::TagIDAuthority::getPointID( tagId );
    outSamples.validityFlags[metaIdx] = saneVf;
    outSamples.iSample[metaIdx] = this->getSampleNumber();

    const MonitorValue & mv = sample_.value;

    CORBA::ULong dataSeqIdx = 0;
    switch ( mvType ) {
        case MONITOR_VALUE_TYPE_BYTE:
            {
                const uchar ch = mv.byte + 0;

                dataSeqIdx = outSamples.charValues.length();
                outSamples.charValues.length( dataSeqIdx + 1 );
                outSamples.charValues[dataSeqIdx] = ch;
            }
            break;

        case MONITOR_VALUE_TYPE_SHORT:
            {
                const short sh = mv.sh;

                dataSeqIdx = outSamples.shortValues.length();
                outSamples.shortValues.length( dataSeqIdx + 1 );
                outSamples.shortValues[dataSeqIdx] = sh;
            }
            break;

        case MONITOR_VALUE_TYPE_INTEGER:
            {
                const long lo = mv.lo;

                dataSeqIdx = outSamples.longValues.length();
                outSamples.longValues.length( dataSeqIdx + 1 );
                outSamples.longValues[dataSeqIdx] = lo;
            }
            break;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            {
                const bool bo = mv.bo;

                dataSeqIdx = outSamples.boolValues.length();
                outSamples.boolValues.length( dataSeqIdx + 1 );
                outSamples.boolValues[dataSeqIdx] = bo;
            }
            break;

        case MONITOR_VALUE_TYPE_FLOAT:
            {
                const float fl = mv.fl;

                dataSeqIdx = outSamples.floatValues.length();
                outSamples.floatValues.length( dataSeqIdx + 1 );
                outSamples.floatValues[dataSeqIdx] = fl;
            }
            break;

        case MONITOR_VALUE_TYPE_DOUBLE:
            {
                const double dbl = mv.db;

                dataSeqIdx = outSamples.doubleValues.length();
                outSamples.doubleValues.length( dataSeqIdx + 1 );
                outSamples.doubleValues[dataSeqIdx] = dbl;
            }
            break;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            {
                const long sn = mv.sn;

                dataSeqIdx = outSamples.serialNumberValues.length();
                outSamples.serialNumberValues.length( dataSeqIdx + 1 );
                outSamples.serialNumberValues[dataSeqIdx] = sn;
            }
            break;

        case MONITOR_VALUE_TYPE_COMPLEX:
            {
                dataSeqIdx = outSamples.complexValues.length();
                outSamples.complexValues.length( dataSeqIdx + 1 );
                outSamples.complexValues[dataSeqIdx].real = mv.complex[0];
                outSamples.complexValues[dataSeqIdx].imag = mv.complex[1];
            }
            break;

        case MONITOR_VALUE_TYPE_STRING:
            {
                const size_t kStringCount = 8;
                Char8 str;
                str.length( kStringCount );
                memcpy( str.get_buffer(), mv.str, kStringCount );

                dataSeqIdx = outSamples.stringValues.length();
                outSamples.stringValues.length( dataSeqIdx + 1 );
                outSamples.stringValues[dataSeqIdx] = str;
            }
            break;

        default:
            {
                const ScopedLogNdc 
                    ndc( "MonitorPointSample::setTransportValue" );
                ostringstream oss;

                oss << "Uknown MonitorValueType " 
                    << static_cast< int >( mvType );

                programLogErrorIfPossible( oss.str() );
            }
    } // switch (mvType)

    outSamples.sequenceIdx[metaIdx] = dataSeqIdx;
}

void
MonitorPointSample::getTransportedSample(
    const CORBA::ULong metaIdx,
    const MonitorSampleValues & inValues,
    const int headerOffset )
{
    sample_.blankingFlags = 0;
    sample_.validityFlags = 0;
    sample_.dummy = (headerOffset & 0x00FF);
    sample_.iSample = inValues.iSample[metaIdx];

    sample_.validityFlags =
        sanityCheckValidityFlags(
            static_cast< char >( inValues.validityFlags[metaIdx] ),
            0,
            true,
            "MonitorPointSample::getTransportedSample" );
    
    MonitorValue & mv = sample_.value;

    const CORBA::ULong dataSeqIdx = inValues.sequenceIdx[metaIdx];
    switch ( inValues.dataType[metaIdx] )  {
        case MONITOR_VALUE_TYPE_BYTE:
            mv.byte = inValues.charValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_SHORT:
            mv.sh = inValues.shortValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_INTEGER:
            mv.lo = inValues.longValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            mv.bo = inValues.boolValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_FLOAT:
            mv.fl = inValues.floatValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_DOUBLE:
            mv.db = inValues.doubleValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            mv.sn = inValues.serialNumberValues[ dataSeqIdx ];
            return;

        case MONITOR_VALUE_TYPE_COMPLEX:
            {
                const ComplexFloat cmplx = inValues.complexValues[ dataSeqIdx ];
                mv.complex[0] = cmplx.real;
                mv.complex[1] = cmplx.imag;
            }
            return;

        case MONITOR_VALUE_TYPE_STRING:
            const Char8 str;
            memcpy( mv.str,
                    static_cast< const char * >( 
                        inValues.stringValues[dataSeqIdx].get_buffer() ),
                    sizeof( mv.str ) );
            return;
    }

    {
        const ScopedLogNdc ndc( "MonitorPointSample::getTransportedValue" );

        ostringstream oss;
    
        oss << "Unknown MonitorValueType " 
            << static_cast< int >( inValues.dataType[metaIdx] );
    
        programLogErrorIfPossible( oss.str() );

    }
}

void
MonitorPointSample::setTransportValue(
    const MonitorValueType  mvType,
    TransportMonitorValue & transportValue ) const
{
    const MonitorValue & mv = sample_.value;

    switch ( mvType ) {
        case MONITOR_VALUE_TYPE_BYTE:
            {
                const uchar ch = mv.byte + 0;
                transportValue.c( ch );
            }
            return;

        case MONITOR_VALUE_TYPE_SHORT:
            {
                const short sh = mv.sh;
                transportValue.s( sh );
            }
            return;

        case MONITOR_VALUE_TYPE_INTEGER:
            {
                const long lo = mv.lo;
                transportValue.i( lo );
            }
            return;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            {
                const bool bo = mv.bo;
                transportValue.b( bo );
            }
            return;

        case MONITOR_VALUE_TYPE_FLOAT:
            {
                const float fl = mv.fl;
                transportValue.f( fl );
            }
            return;

        case MONITOR_VALUE_TYPE_DOUBLE:
            {
                const double dbl = mv.db;
                transportValue.d( dbl );
            }
            return;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            {
                const long sn = mv.sn;
                transportValue.sn( sn );
            }
            return;

        case MONITOR_VALUE_TYPE_COMPLEX:
            {
                float fl[2];

                fl[0] = mv.complex[0];
                fl[1] = mv.complex[1];

                transportValue.complex( fl );
            }
            return;

        case MONITOR_VALUE_TYPE_STRING:
            {
                const size_t kStringCount = 8;
                char str[ kStringCount ];

                memcpy( str, mv.str, kStringCount );
                transportValue.str( str );
            }
            return;
    }

    {
        const ScopedLogNdc ndc( "MonitorPointSample::setTransportValue" );
        
        ostringstream oss;

        oss << "Uknown MonitorValueType " << static_cast< int >( mvType );

        programLogErrorIfPossible( oss.str() );
    }
}


void
MonitorPointSample::getTransportedValue(
    const MonitorValueType        mvType,
    const TransportMonitorValue & transportValue )
{
    MonitorValue & mv = sample_.value;

    switch ( mvType )  {
        case MONITOR_VALUE_TYPE_BYTE:
            mv.byte = transportValue.c();
            return;

        case MONITOR_VALUE_TYPE_SHORT:
            mv.sh = transportValue.s();
            return;

        case MONITOR_VALUE_TYPE_INTEGER:
            mv.lo = transportValue.i();
            return;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            mv.bo = transportValue.b();
            return;

        case MONITOR_VALUE_TYPE_FLOAT:
            mv.fl = transportValue.f();
            return;

        case MONITOR_VALUE_TYPE_DOUBLE:
            mv.db = transportValue.d();
            return;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            mv.sn = transportValue.sn();
            return;

        case MONITOR_VALUE_TYPE_COMPLEX:
            {
                const float * const cmplx = transportValue.complex();

                mv.complex[0] = cmplx[0];
                mv.complex[1] = cmplx[1];
            }
            return;

        case MONITOR_VALUE_TYPE_STRING:
            memcpy( mv.str,
                    static_cast< const char * >( transportValue.str() ),
                    sizeof( mv.str ) );
            return;
    }

    {
        const ScopedLogNdc ndc( "MonitorPointSample::getTransportedValue" );

        ostringstream oss;
    
        oss << "Uknown MonitorValueType " << static_cast< int >( mvType );
    
        programLogErrorIfPossible( oss.str() );
    }
}


//
//  End MonitorPointSample
//


//
// SampleInvalidExceptionObj methods
//


namespace {


string
makeSampleInvalidExceptionMessage( const MonitorSampleValue & sampleValue,
                                   const MonitorValueType     mvType )
{
    ostringstream oss;

    oss << " Sample type " << mvType
        << " Sample # " << sampleValue.iSample
        << " blanking flags " << sampleValue.blankingFlags
        << " validity flags " << sampleValue.validityFlags;

    return oss.str();
}


string
makeSampleInvalidExceptionMessage( const MonitorPointSample & sample,
                                   const MonitorValueType     mvType )
{
    ostringstream oss;

    oss << " Sample type " << mvType
        << " Sample # " << sample.getSampleNumber()
        << " blanking flags " << sample.getBlankingFlags()
        << " validity flags " << sample.getValidityFlags();

    return oss.str();
}


}  // namespace < anonymous >


SampleInvalidExceptionObj::SampleInvalidExceptionObj(
    const char * const mesg,
    const char * const fileName,
    const int          lineNum ) :
BaseException( mesg, fileName, lineNum )
{
}


SampleInvalidExceptionObj::SampleInvalidExceptionObj(
    const ostringstream & errStream,
    const char * const    fileName,
    const int             lineNum ) :
BaseException( errStream, fileName, lineNum )
{
}


SampleInvalidExceptionObj::SampleInvalidExceptionObj(
    const MonitorSampleValue & sampleValue,
    const MonitorValueType     mvType,
    const char * const         fileName,
    const int                  lineNum ) :
BaseException( makeSampleInvalidExceptionMessage( sampleValue, mvType ),
               fileName,
               lineNum )
{
}


SampleInvalidExceptionObj::SampleInvalidExceptionObj (
    const MonitorPointSample & sample,
    const MonitorValueType     mvType,
    const char * const         fileName,
    const int                  lineNum ) :
BaseException( makeSampleInvalidExceptionMessage( sample, mvType ),
               fileName,
               lineNum )
{
}
