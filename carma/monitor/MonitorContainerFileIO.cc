#include "carma/monitor/MonitorContainerFileIO.h"

#include "carma/dbms/TagIDAuthority.h"   
#include "carma/monitor/MonitorContainer.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/types.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

#include <boost/lexical_cast.hpp>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace boost;
using namespace std;

namespace {

MonitorValueType 
stringToValueType( const string & type )
{
    // TODO: Optimize this with a static map if it slows things down
    typedef MonitorPoint MP;

    if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_BYTE) ) 
        return MONITOR_VALUE_TYPE_BYTE; 
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_SHORT) )
        return MONITOR_VALUE_TYPE_SHORT;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_INTEGER) ) 
        return MONITOR_VALUE_TYPE_INTEGER;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_BOOLEAN) )
        return MONITOR_VALUE_TYPE_BOOLEAN;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_FLOAT) ) 
        return MONITOR_VALUE_TYPE_FLOAT;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_DOUBLE) )
        return MONITOR_VALUE_TYPE_DOUBLE;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_COMPLEX) )
        return MONITOR_VALUE_TYPE_COMPLEX;
    else if ( type == MP::valuetypeToString(MONITOR_VALUE_TYPE_STRING) )
        return MONITOR_VALUE_TYPE_STRING;
    else if ( type == 
            MP::valuetypeToString(MONITOR_VALUE_TYPE_SERIAL_NUMBER) )
        return MONITOR_VALUE_TYPE_SERIAL_NUMBER;
    else 
        throw CARMA_ERROR( "String \"" + type + "\" doesn't match a type." );
}

struct LineInfo {
    tagIDType tagId;
    string canonicalName;
    MonitorValueType type;
    string valueString;

    string asString() const {
        ostringstream dump;
        dump << "tagId=" << tagId << ", canon=" << canonicalName
            << ", type=" << MonitorPoint::valuetypeToString( type ) 
            << ", valueString=" << valueString << ".";
        return dump.str();
    };
};

LineInfo 
parseLine( const string & line )
{
    LineInfo info;
    string localLine = line;
    vector< string > tokens;
    const unsigned int numTokens = 4;

    for ( unsigned int token = 0; token < numTokens - 1; ++token ) {
        string::size_type separator = localLine.find_first_of( ' ' ); 
        if ( separator == string::npos ) 
            tokens.push_back( localLine.substr( 0 ) );
        else 
            tokens.push_back( localLine.substr( 0, separator ) );
        localLine = localLine.substr( separator + 1 ); 
    }
    tokens.push_back( localLine ); // Last token is whatever is left over.

    istringstream tagIdToken( tokens.at( 0 ) ); 
    tagIdToken >> info.tagId;

    info.canonicalName = tokens.at( 1 );
    info.type = stringToValueType( tokens.at( 2 ) );
    info.valueString = tokens.at( 3 );

    return info;
} 
    

void 
setMonitorPointFromLine( MonitorContainer & container, const LineInfo & line ) 
{
    // Lookup MP by tagId (can also use canonical name)
    // Currently we use the canonical name which is SLOOOW but necessary
    // to get around the fact that unique id to canonical names aren't 
    // guaranteed for on-the-fly generated MPs.
    dbms::TagIDAuthority & authority = dbms::TagIDAuthority::getAuthority();

    tagIDType tag;
    try {
        tag = authority.lookupID( line.canonicalName );
    } catch (...) {
        logCaughtAsError( );
        return;
    }

    try {
        MonitorPoint * mp = container.getMonitorPointPtr( tag );

        if ( !mp ) return; // Abort if mp comes back null.

        // Use RTTI to reset the MP.
        switch ( line.type ) {
            case MONITOR_VALUE_TYPE_BYTE: 
                ( dynamic_cast< MonitorPointChar * >( mp ) )->setValue( 
                        boost::lexical_cast< char >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_SHORT:
                ( dynamic_cast< MonitorPointShort * >( mp ) )->setValue(
                        boost::lexical_cast< short >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_INTEGER:
                {
                    MonitorPointInt * mpi = dynamic_cast< MonitorPointInt * >( mp );
                    if ( mpi ) { 
                        mpi->setValue( boost::lexical_cast< long >( 
                                    line.valueString ) );
                    } else {
                        MonitorPointEnum * mpe 
                            = dynamic_cast< MonitorPointEnum * >( mp );
                        if ( !mpe ) {
                            ostringstream msg;
                            msg << "Invalid dynamic cast for "
                                << mp->getName() << ", line info: " 
                                << line.asString();
                            throw CARMA_ERROR( msg.str() );
                        }

                        mpe->setValue( boost::lexical_cast< long >( 
                                    line.valueString ), 0 );
                    }
                }
                break;
            case MONITOR_VALUE_TYPE_BOOLEAN:
                ( dynamic_cast< MonitorPointBool * >( mp ) )->setValue( 
                        boost::lexical_cast< bool >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_FLOAT:
                ( dynamic_cast< MonitorPointFloat * >( mp ) )->setValue( 
                        boost::lexical_cast< float >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_DOUBLE:
                {
                    MonitorPointDouble * mpd = 
                        dynamic_cast< MonitorPointDouble * >( mp );
                    if ( mpd ) {
                        mpd->setValue( 
                                boost::lexical_cast< double >(line.valueString) );
                    } else {
                        MonitorPointAbstime * mpa = 
                            dynamic_cast< MonitorPointAbstime * >( mp );
                        mpa->setValue( 
                                boost::lexical_cast< double >(line.valueString) );
                        if ( !mpa ) 
                            throw CARMA_ERROR( "Invalid dynamic cast." );
                    }
                }
                break;
            case MONITOR_VALUE_TYPE_COMPLEX:
                ( dynamic_cast< MonitorPointComplex * >( mp ) )->setValue( 
                        boost::lexical_cast< complex<float> >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_STRING:
                ( dynamic_cast< MonitorPointString * >( mp ) )->setValue( 
                        boost::lexical_cast< string >( line.valueString ) );
                break;
            case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
                ( dynamic_cast< MonitorPointSerialNo * >( mp ) )->setValue( 
                        boost::lexical_cast< int >( line.valueString ) );
                break;
            default:
                throw CARMA_ERROR( "Invalid type enum." );
        }
    } catch (...) {
        ostringstream errmsg;
        errmsg << "Unable to set MP for canonical name "
            << line.canonicalName << " due to: "
            << getStringForCaught();
        programLogErrorIfPossible( errmsg.str() );
    }

}

} // namespace <unnamed>

void 
carma::monitor::writeContainerToFile( const MonitorContainer & container,
                                      const std::string & filename, 
                                      const std::vector< std::string > & skip )
{
    ScopedLogNdc ndc("writeContainerToFile(file=" + filename );
    std::fstream out( filename.c_str(), ios::out ); // Open file for writing

    if ( !out ) 
        throw CARMA_ERROR( "Unable to open " + filename + " for writing." );

    const int FULL_RECURSE = 0;
    MonitorPointIterator mpi( container, FULL_RECURSE, skip ); 
    
    while ( ++mpi ) { // Iterator must be iterated once prior to use!?
        const MonitorPoint & mp = mpi.getMonitorPoint( ); 

        const string canonicalName = mp.getCanonicalName();

        if ( mp.getValidity() >= MonitorPoint::VALID ) {
            out << mp.getTagID() << " " << mp.getCanonicalName() << " ";
            out << mp.valuetypeToString( ) << " ";
            out << mp.getCoreValueAsString( ) << endl;
        } // if mp valid
    } // while ( mpi++ )
} // writeContainerToFile

void
carma::monitor::setContainerFromFile( MonitorContainer & container,
                                      const std::string & filename )
{
  ScopedLogNdc ndc("setContainerFromFile(file=" + filename );
  try {
    std::fstream fin( filename.c_str(), ios::in ); // Open file for reading 

    if ( !fin ) 
        throw CARMA_EXCEPTION( ErrorException, 
                               "Unable to open " + filename + " for reading." );

    string line;
    unsigned int linesRead = 0;
    while ( getline( fin, line ) ) {
        try { 
            const LineInfo parsedLine = parseLine( line ); 
            setMonitorPointFromLine( container, parsedLine );
        } catch (...) {
            programLogErrorIfPossible( "For line: " + line );
        }
        ++linesRead;
    }

    if ( !fin.eof() ) throw std::ios::failure( "Error reading file." );

  } catch (...) {
    ostringstream msg; 
    msg << "Error setting monitor container from file " << filename 
        << ".  Error is: " << getStringForCaught();
    programLogErrorIfPossible( msg.str() );
    throw;
  }
}

std::set< carma::monitor::tagIDType >
carma::monitor::compareContainerToFile( MonitorContainer & container,
                                        const std::string & filename ) 
{
    ScopedLogNdc ndc("compareContainerToFile(file=" + filename );

    std::fstream fin( filename.c_str(), ios::in ); // Open file for reading 

    if ( !fin ) 
        throw CARMA_EXCEPTION( ErrorException, 
                               "Unable to open " + filename + " for reading." );

    set< tagIDType > answer;

    dbms::TagIDAuthority & authority = dbms::TagIDAuthority::getAuthority();

    // Rip through the file reading one line at a time...
    string line;
    while ( getline( fin, line ) ) {

        const LineInfo parsedLine = parseLine( line ); 

        // Note on tagId use:  Because on the fly generated tagIds are not
        // guaranteed to be unique, we can't use tags for persistant storage.
        // However, here, we can use tagIds because they come from the same
        // tagIDauthority and the mp lookup routine using tags is believed to
        // be quicker than the corresponding routine using string lookup.
        tagIDType tag;
        try {
            tag = authority.lookupID( parsedLine.canonicalName );
        } catch (...) {
            ostringstream err;
            err << "compareContainerToFile( ) - Unable to find tagId for " 
                << "canonical name " << parsedLine.canonicalName 
                << " in state file " << filename << ".";
            programLogErrorIfPossible( err.str() );
            continue; // But don't fold, just skip it and try to process more.
        }

        MonitorPoint * mp = container.getMonitorPointPtr( tag );

        if ( !mp ) continue; // If new/old monitor point appears or disappears.

        // Compare textual version of mp to lexically cast container version.
        // Works because boost::lexical_cast consistently round trips floats.
        // Note we assume that a value in the file is automatically VALID
        if ( mp->getCoreValueAsString( ) != parsedLine.valueString ||
             mp->getValidity() < MonitorPoint::VALID ) {
            answer.insert( tag );
        }
    }

    return answer;
}
