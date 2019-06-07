/**
 *
 * Carma control interface server implementation for various queries
 * about the monitor system and sources.
 *
 * @author: Marc Pound
 *
 * $Id: SubarrayControlQuery.cc,v 1.90 2014/11/03 19:23:09 iws Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/control/SubarrayControlImpl.h"

#include <sstream>
#include <vector>
#include <set>

#include "carma/corba/corba.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/ProjectDatabaseManagerHandle.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/nearestInfoUtils.h"
#include "carma/control/WorkerPool.h"

#include "carma/monitor/MonitorPointIterator.h"

#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/observertools/PDB_Util.h"

#include "carma/services/AstroTime.h"
#include "carma/services/EphemerisException.h"
#include "carma/services/FluxCatalog.h"
#include "carma/services/FluxDensity.h"
#include "carma/services/FluxSource.h"
#include "carma/services/Length.h"
#include "carma/services/Neighbor.h"
#include "carma/services/OpticalCatalog.h"
#include "carma/services/Source.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Star.h"
#include "carma/services/Types.h"

#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"
#include "carma/util/WorkResult.h"

//NB: not using namespace carma::observertools
//because of name conflict with class Source
using namespace ::std;
using namespace CORBA;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::antenna::common;
using namespace log4cpp;

namespace {

// mutex for making calls to project database
::pthread_mutex_t gPDBGuard = PTHREAD_MUTEX_INITIALIZER;
typedef ScopedLock< ::pthread_mutex_t > PDBLock;

} // namespace < anonymous >

namespace carma {
namespace control {
// Helper exception, only used in this file
class InvalidDataException : public ErrorException {
public:
    InvalidDataException(const ostringstream& o, const char* fn, int line):
        ErrorException(o, fn, line)
    {}
    InvalidDataException(const InvalidDataException& exc):
        ErrorException(exc)
    {}

};
}} // End carma.control namespace


// All of the queries should return average values!
char *
SubarrayControlImpl::query( const char * const componentName )
try {
    MonitorComponent* c = findMonitorComponent(componentName);
    ostringstream o;
    if (c != 0) {
        carmaMonitor_.readNewestConditionalCopy();
        o << c->hierarchyToStringAverage();
    }
    else {
        o << "monitor component \"" << componentName << "\" not found";
        throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                    o.str().c_str());
    }

    // NB: Must use string_dup or you'll get a SIGSEGV
    // http://mail.ooc.nf.ca/pipermail/ob-users/1999-March/004364.html
    return CORBA::string_dup(o.str().c_str());
} catch ( const control::MonitorPointNotFoundException & ) {
    throw;
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


double
SubarrayControlImpl::getMonitorValue( const string & componentName )
{
    MonitorComponent* c = findMonitorComponent(componentName);
    if (c == 0) {
        ostringstream errOs;
        errOs << "Can\'t find monitor point \"" << componentName << "\"";
        throw CARMA_EXCEPTION(util::NotFoundException, errOs);
    }
    MonitorPointNumeric* n;
    if ( (n = dynamic_cast<MonitorPointNumeric*>(c) ) ) {
        carmaMonitor_.readNewestConditionalCopy();
        if (!n->isValid()) {
            ostringstream errOs;
            errOs << "Invalid value for monitor point: " << componentName;
            throw CARMA_EXCEPTION(control::InvalidDataException, errOs);
        }
        return n->getAveNumeric();
    } else {
        if (MonitorPointEnum* n = dynamic_cast<MonitorPointEnum*>(c)) {
            carmaMonitor_.readNewestConditionalCopy();
            if (!n->isValid()) {
                ostringstream errOs;
                errOs << "Invalid value for monitor point: " << componentName;
                throw CARMA_EXCEPTION(control::InvalidDataException, errOs);
            }
            return static_cast<double>(n->getAve());
        }
    }

    ostringstream o;
    o << "Can't get a numerical value for monitor point: " << componentName;
    throw CARMA_ERROR(o);
}

char *
SubarrayControlImpl::queryString(const char * const monitorPointName)
try {
    MonitorComponent* c = findMonitorComponent(monitorPointName);
    if (c == 0) {
        ostringstream errOs;
        errOs << "Can\'t find monitor point \"" << monitorPointName << "\"";
        throw CARMA_EXCEPTION(util::NotFoundException, errOs);
    }

    // We allow return values from both string types and enumeration types.
    if (MonitorPointString* n = dynamic_cast<MonitorPointString*>(c)) {
        carmaMonitor_.readNewestConditionalCopy();
        if (!n->isValid()) {
            ostringstream errOs;
            errOs << "Invalid value for monitor point: " << monitorPointName ;
            throw CARMA_EXCEPTION(control::InvalidDataException, errOs);
        }
        string retVal = n->getValue();
        return CORBA::string_dup(retVal.c_str());
    } else {
        if (MonitorPointEnum* n = dynamic_cast<MonitorPointEnum*>(c)) {
            carmaMonitor_.readNewestConditionalCopy();
            if (!n->isValid()) {
                ostringstream errOs;
                errOs << "Invalid value for monitor point: "
                    << monitorPointName ;
                throw CARMA_EXCEPTION(control::InvalidDataException, errOs);
            }
            string retVal = n->getValueToString(0);
            return CORBA::string_dup(retVal.c_str());
        }
    }

    ostringstream o;
    o << "Monitor point " << monitorPointName << " is not of string type.";
    throw CARMA_ERROR(o);

} catch ( const util::NotFoundException & nfe ) {
    throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                nfe.getMessage());
} catch ( const control::InvalidDataException & bde ) {
    throw CARMA_EXCEPTION(control::InvalidMonitorDataException,
                bde.getMessage());
} catch ( const carma::util::BaseException & ex ) {
    throw CARMA_EXCEPTION(util::UserException, ex.getMessage());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

double
SubarrayControlImpl::queryDouble(const char * const monitorPointName)
try {
    return  getMonitorValue(monitorPointName);
} catch ( const util::NotFoundException & nfe ) {
    throw CARMA_EXCEPTION( control::MonitorPointNotFoundException,
                           nfe.getMessage() );
} catch ( const control::InvalidDataException & bde ) {
    throw CARMA_EXCEPTION( control::InvalidMonitorDataException,
                           bde.getMessage());
} catch ( const carma::util::BaseException & ex ) {
    throw CARMA_EXCEPTION(util::UserException, ex.getMessage());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


CORBA::Long
SubarrayControlImpl::queryInt(const char * const monitorPointName)
try
{
    return static_cast<CORBA::Long>(round(getMonitorValue(monitorPointName)));
} catch ( const util::NotFoundException & nfe ) {
    throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                nfe.getMessage());
} catch ( const control::InvalidDataException & bde ) {
    throw CARMA_EXCEPTION(control::InvalidMonitorDataException,
                bde.getMessage());
} catch ( const carma::util::BaseException & ex ) {
    throw CARMA_EXCEPTION( util::UserException, ex.getMessage() );
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

carma::control::SeqFloat *
SubarrayControlImpl::queryComplex(const char * const monitorPointName)
try {
    MonitorComponent* c = findMonitorComponent(monitorPointName);
    if (c == 0) {
        ostringstream o;
        o << "Couldn't find monitor point: " << monitorPointName;
        throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                    o.str().c_str());
    }
    MonitorPointComplex* cx;
    if ( (cx = dynamic_cast<MonitorPointComplex*>(c)) ) {
        carmaMonitor_.readNewestConditionalCopy();
        if (!cx->isValid()) {
            ostringstream o;
            o << "Invalid value for monitor point: " << monitorPointName;
            //throw CARMA_EXCEPTION(control::InvalidMonitorDataException,
            //            o.str().c_str());
        }
        complex<float> val = cx->getAve();

        carma::control::SeqFloat_var seq( new SeqFloat(2) );

        seq->length(2);

        seq[0] = val.real();
        seq[1] = val.imag();

        return seq._retn();
    }
    else {
        ostringstream o;
        o << monitorPointName << " is not a complex monitor point";
        throw CARMA_EXCEPTION( util::UserException, o.str().c_str() );
    }
    throw CARMA_EXCEPTION( util::UserException,
            "queryComplex: code error, it is impossible to get here");
} catch ( const control::InvalidMonitorDataException & ) {
    throw;
} catch ( const control::MonitorPointNotFoundException & ) {
    throw;
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

bool
SubarrayControlImpl::queryBool(const char * const monitorPointName)
try {
    MonitorComponent* c = findMonitorComponent(monitorPointName);
    if (c == 0) {
        ostringstream o;
        o << "Can't find monitor point: " << monitorPointName;
        throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                    o.str().c_str());
    }
    MonitorPointBool* b;
    if ( (b = dynamic_cast<MonitorPointBool*>(c)) ) {
        carmaMonitor_.readNewestConditionalCopy();
        if (!b->isValid()) {
            ostringstream o;
            o << "Invalid data for: " << monitorPointName;
            throw CARMA_EXCEPTION(control::InvalidMonitorDataException,
                    o.str().c_str());
        }
        return b->getAve();
    }
    else {
        ostringstream o;
        o << monitorPointName << " is not a boolean monitor point";
        throw CARMA_EXCEPTION( util::UserException, o.str().c_str() );
    }
    throw CARMA_EXCEPTION( util::UserException,
            "queryBool: impossible to get here");
} catch ( const control::InvalidMonitorDataException & ) {
    throw;
} catch ( const control::MonitorPointNotFoundException & ) {
    throw;
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

MonitorComponent*
SubarrayControlImpl::findMonitorComponent(const string& componentName) const
{
    string name(componentName);
    string::size_type index  = name.find('.');
    string monitorSystemName = name.substr(0, index);
    static MonitorContainer* sa[4];
    static MonitorContainer* ant[23];
    static bool accelInit = false;
    if (!accelInit) {
        accelInit = true;
        for (int i=0; i<4; i++)  sa[i]  = &carmaMonitor_.control().subarray(i);
        for (int i=0; i<23; i++) ant[i] = &carmaMonitor_.control().antenna(i);
    }
    static const string saPrefix  = "Control.Subarray";
    static const string antPrefix = "Control.Antenna";
    static const string::size_type  saPrefixSize  = saPrefix.size();
    static const string::size_type  antPrefixSize = antPrefix.size();

    // Do case insensitive checks because the lower-level monitor
    // system search is case insensitive.

    // Raw monitor system is done first
    if ( StringUtils::equalsIgnoreCase(monitorSystemName, "Raw") ) {
        string theRest = name.substr(index+1);
        return rawCarmaMonitor_.getComponentPtr(theRest, false);
    }

    // "Carma" may or may not have been prepended; if it was, strip it off.
    if ( StringUtils::equalsIgnoreCase(monitorSystemName, "Carma") ) {
        name = name.substr(index+1);
    }

    // Control.Subarray and Antenna  accelerator
    //  Approx 6-14x speedup for top level nodes
    string prefix = name.substr(0, saPrefixSize);
    if (StringUtils::equalsIgnoreCase(prefix, saPrefix)) {
        const string saNoString = name.substr(saPrefixSize, 1);
        const int saNo = atoi( saNoString.c_str() );
        if ((saNo > 0) && (saNo <= 4)) {
            string::size_type nextDot = name.find('.', saPrefixSize);
            if (nextDot != string::npos) {
                string mp   = name.substr(nextDot+1);
                return sa[saNo-1]->getComponentPtr(mp, false);
            }
            else {
                return sa[saNo-1];
            }
        }
    }
    else {
        prefix = name.substr(0, antPrefixSize);
        if (StringUtils::equalsIgnoreCase(prefix, antPrefix)) {
            string::size_type nextDot = name.find('.', antPrefixSize);
            if (nextDot != string::npos) {
                const int nDigits = nextDot - antPrefixSize;
                const string antNumberString = name.substr(antPrefixSize,
                                                           nDigits);
                const int antNo = atoi(antNumberString.c_str());
                if ((antNo > 0) && (antNo <= 23)) {
                    string mp  = name.substr(nextDot+1);
                    return ant[antNo-1]->getComponentPtr(mp, false);
                }
            }
        }
    }

    return carmaMonitor_.getComponentPtr(name, false);
}
// Implemented in two steps:
//  1) Get vector of all MPs
//  2) Go though MP vec and get attributes and put in result
SeqMonitorPointValue*
SubarrayControlImpl::queryMonitorPoint(const SeqString& monitorComponent)
try
{
    vector<string> mc     =
            convertSequenceToVector<string>(monitorComponent);
    int componentCount    = monitorComponent.length();
    vector<string> leafMP;
    vector<bool> foundMP;

    // Get vector of all MP names (or not found strings)
    for (int i=0; i < componentCount; i++) {
        string            compName = mc[i];
        MonitorComponent* comp     = findMonitorComponent(compName);
        if (comp != 0) {
            if (comp->isMonitorPoint()) {
                compName = comp->getCanonicalName();
                leafMP.push_back(compName);
                foundMP.push_back(true);
            }
            else {
                // Its a container
                MonitorContainer& mc = dynamic_cast<MonitorContainer&>(*comp);
                MonitorPointIterator mpi(mc, 1);
                // Iterate through all MPs in the container
                while (mpi++) {
                    string mpName = mpi.getMonitorPoint().getCanonicalName();
                    leafMP.push_back(mpName);
                    foundMP.push_back(true);
                }
            }
        }
        else {
            // Didn't find it
            leafMP.push_back(compName);
            foundMP.push_back(false);
        }
    }

    int leafCount         = leafMP.size();
    SeqMonitorPointValue_var result(new SeqMonitorPointValue(leafCount));
    result->length(leafCount);

    TransportMonitorValue tmv;
    carmaMonitor_.readNewestConditionalCopy();
    for (int i=0; i < leafCount; i++) {
        // Default values
        tmv.b(false);
        (*result)[i].valid = false;
        (*result)[i].found = false;
        string stringValue = "?";
        string compName = leafMP[i];
        if (foundMP[i]) {
            MonitorComponent* comp = findMonitorComponent(compName);
            if (comp != 0) {
                MonitorPoint* mp   = dynamic_cast<MonitorPoint*>(comp);
                if (mp != 0) {
                    compName = mp->getCanonicalName();
                    MonitorPointSample sample = mp->getSampleAverage();
                    MonitorValueType   valueType = mp->getValuetype();
                    sample.setTransportValue(valueType, tmv);
                    (*result)[i].valid  = mp->isValid();
                    (*result)[i].found  = true;
                    stringValue         = mp->getAverageToString();
                }
            }
        }
        (*result)[i].name        = string_dup(compName.c_str());
        (*result)[i].value       = tmv;
        (*result)[i].stringValue = string_dup(stringValue.c_str());
    }
    return result._retn();
} catch (const util::UserException &) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

// MP name should not start with 'Control.', but should be just below it
// in themonitor point tree.
void SubarrayControlImpl::setMonitorPointReal(const char* monitorPointName, 
		double value)
try
{
    const bool caseSensitive = false;
    MonitorComponent* c = controlSubsystem_.getComponentPtr(
            monitorPointName, caseSensitive);
    if (c == 0) {
        ostringstream o;
        o << "Can't find control monitor point: " << monitorPointName;
        throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,
                    o.str().c_str());
    }
    
    MonitorPointDouble* d;
    if ( (d = dynamic_cast<MonitorPointDouble*>(c)) ) {
        d->setValue(value);
        return;
    }
    else {
        MonitorPointFloat* f;
        if ( (f = dynamic_cast<MonitorPointFloat*>(c)) ) {
            f->setValue(value);
            return;
        }    
        else {
            ostringstream o;
            o << monitorPointName << " is not a double or float monitor point";
            throw util::UserException(o.str().c_str(), __FILE__, __LINE__);
        }
    }
    throw CARMA_EXCEPTION( util::UserException,
            "setMonitorPointReal: impossible to get here");
} catch (const util::UserException&) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}
    
void SubarrayControlImpl::setMonitorPointInvalid(const char* monitorPointName)
try
{
    const bool caseSensitive = false;
    MonitorComponent* c = controlSubsystem_.getComponentPtr(
            monitorPointName, caseSensitive);
    if (c == 0) {
        ostringstream o;
        o << "Can't find control monitor point: " << monitorPointName;
        throw util::UserException(o.str().c_str(), __FILE__, __LINE__);
        //throw CARMA_EXCEPTION(control::MonitorPointNotFoundException,o.str().c_str());
    }
    MonitorPoint* mp;
    if ( (mp = dynamic_cast<MonitorPoint*>(c)) ) {
        mp->setValidity(MonitorPoint::INVALID_NO_DATA);
        return;
    }
    else {
        ostringstream o;
        o << monitorPointName << " is not a monitor point";
        throw CARMA_EXCEPTION( util::UserException, o.str().c_str() );
    }
    throw CARMA_EXCEPTION(util::UserException,
            "setmonitorPointInvalid: impossible to get here");
} catch (const util::UserException &) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


FluxMeasurement *
SubarrayControlImpl::queryFlux( const char * const source,
                                const float        freq,
                                const float        deltaFreq,
                                const float        deltaTime )
try
{
    const string GHZ("GHz");
    // potential for speedup here by
    // having private FluxCatalog member variable,
    // but then must be sure to mutex protect for thread
    // safety.  it may be worth it since FluxSource.cat
    // is ~5800 entries on 5/2007 and will grow with time.
    // (100+ entries/year)
    FluxCatalog fc;
    fc.open( FluxCatalog::defaultCatalog() );
    const string sname( source );
    Frequency base( freq, GHZ);
    Frequency delta( deltaFreq, GHZ);
    // lookup() throws SourceNotFound if it can't match
    // the query inputs with an existing flux measurement.
    FluxSource fs = fc.lookup( sname, base, delta , deltaTime );
    FluxDensity fd = fs.getFlux();
    FluxMeasurement_var fm(new FluxMeasurement);
    fm->flux = fd.jansky();
    fm->freq = fs.getFrequency().gigahertz();
    fm->rms  = fs.getRms();
    fm->daysback = Time::MJD() - fs.getMJD();
    fm->source = CORBA::string_dup( sname.c_str() );
    fm->date = CORBA::string_dup( fs.getDate().c_str() );
    return fm._retn();
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


float
SubarrayControlImpl::queryMag( const char * const starName )
try
{
    OpticalCatalog stars;
    stars.open( OpticalCatalog::defaultCatalog() );
    const string theStar( starName );
    Star star = stars.lookup( theStar );
    return star.getMagnitude();

} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

void
SubarrayControlImpl::elevLimit( const float limit )
try {
    elevLimit_ = limit;

    subarrayContainer_.elevLimit().setValue( elevLimit_ );
    markStateChange();

} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


char *
SubarrayControlImpl::info( const char * const sourceName )
try {
    const string sname(sourceName);
    ScopedLogNdc ndc("SubarrayControlImpl::info("+sname+")");
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    try {
        checker.setSource(sname, userCatalog_);
    } catch ( const SourceNotFoundException & snfe ) {
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }
    checker.setMJD();
    checker.showHeader(true);
    ostringstream infoStream;
    infoStream << checker.info() << "\n" << times() ;
    string srcInfo = infoStream.str();
    return CORBA::string_dup(srcInfo.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


char *
SubarrayControlImpl::times( )
try {
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    ostringstream timeStream;
    timeStream << "Current LST: " << lstString()
               << "  UT: " << checker.ut() ;
               //<< "  Local: " << checker.localTime() << endl;
    string timeInfo = timeStream.str();
    return CORBA::string_dup(timeInfo.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


RaDecSeq *
SubarrayControlImpl::getRaDec(const char* const sourceName)
try {
    const string sname(sourceName);
    ScopedLogNdc ndc("SubarrayControlImpl::getRaDec("+sname+")");

    Ephemeris src;
    src.setSource(sname, userCatalog_);
    src.setMJD(Time::MJD());

    RaDecSeq_var theRadec(new RaDecSeq(2));

    theRadec->length(2);

    theRadec[0] = src.getRa()*180.0/M_PI;
    theRadec[1] = src.getDec()*180.0/M_PI;

    return theRadec._retn();
} catch ( const SourceNotFoundException & snfe ) {
    throw CARMA_EXCEPTION( util::UserException, snfe.getMessage() );
} catch ( const EphemerisException & ee ) {
    throw CARMA_EXCEPTION( util::UserException,
        "Requested date out of range of Ephemeris (2000-2050)" );
} catch ( const IllegalArgumentException & iae ) {
    ostringstream foo;

    foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
    foo << iae.getMessage();

    throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


AzElSeq *
SubarrayControlImpl::azel( const char * const sourceName,
                           const double       minutes )
try {
    const string sname( sourceName );
    ScopedLogNdc ndc("SubarrayControlImpl::azel("+sname+")");

    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    checker.setSource(sname, userCatalog_);

    const double now = Time::MJD();
    const double requestedMJD = now + (minutes / AstroTime::MINUTES_PER_DAY);

    checker.setMJD( requestedMJD );

    AzElSeq_var theAzel( new AzElSeq(2) );

    theAzel->length(2);

    theAzel[ 0 ] = checker.getAzimuth().degrees();
    theAzel[ 1 ] = checker.getElevation().degrees();

    return theAzel._retn();
} catch ( const SourceNotFoundException & snfe ) {
    throw CARMA_EXCEPTION( util::UserException, snfe.getMessage() );
} catch ( const EphemerisException & ee ) {
    throw CARMA_EXCEPTION( util::UserException,
        "Requested date out of range of Ephemeris (2000-2050)" );
} catch ( const IllegalArgumentException & iae ) {
    ostringstream foo;

    foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
    foo << iae.getMessage();

    throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


bool
SubarrayControlImpl::isUp( const char * const sourceName )
try {
    const string sname(sourceName);
    ScopedLogNdc ndc("SubarrayControlImpl::isUp("+sname+")");
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    try {

        checker.setSource(sname, userCatalog_);
    } catch ( const SourceNotFoundException & snfe ) {
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }
    checker.setMJD();
    return checker.isUp();
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


float
SubarrayControlImpl::whenUp( const char * const sourceName )
try {
    const string sname(sourceName);
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    try {
        checker.setSource(sname, userCatalog_);
    } catch ( const SourceNotFoundException & snfe ) {
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }
    checker.setMJD();
    return static_cast<float>(checker.minutesUntilRise());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


float
SubarrayControlImpl::whenDown( const char * const sourceName )
try {
    const string sname(sourceName);
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    try {
        checker.setSource(sname, userCatalog_);
    } catch ( const SourceNotFoundException & snfe ) {
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }
    checker.setMJD();
    return static_cast<float>(checker.minutesUntilSet());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


float
SubarrayControlImpl::whenTransit( const char * const sourceName )
try {
    const string sname(sourceName);
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    try {
        checker.setSource(sname, userCatalog_);
    } catch ( const SourceNotFoundException & snfe ) {
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }
    checker.setMJD();
    return static_cast<float>(checker.minutesUntilTransit());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


char *
SubarrayControlImpl::whazUp( )
try {
    ostringstream upInfo;
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    checker.setMJD();
    checker.showHeader(true);
    try {
        upInfo << " System Catalog" << endl;
        for( SourceIterator si = sourceCatalog_.catalogBegin();
                si != sourceCatalog_.catalogEnd(); si++)
        {
            Source source = si->second;
            if ( source.isRadio() )
            {
                checker.setSource(source);
                if ( checker.isUp() ) {
                    upInfo << checker.info() << endl;
                    // no headers after the first one
                    checker.showHeader(false);
                }
            }
        }

        // if user catalog is in use, then print info for that too.
        if ( ! userCatalog_.empty() ) {
            upInfo << endl << " User Catalog: " << userCatalog_ << endl;
            SourceCatalog usercat;
            usercat.open(userCatalog_);
            for( SourceIterator si = usercat.catalogBegin();
                    si != usercat.catalogEnd(); si++)
            {
                Source source = si->second;
                if ( source.isRadio() )
                {
                    checker.setSource(source);
                    if ( checker.isUp() ) {
                        upInfo << checker.info() << endl;
                        // no headers after the first one
                        checker.showHeader(false);
                    }
                }
            }
        }
        checker.showHeader(true);
    } catch ( const SourceNotFoundException & snfe ) {
        // this shouldn't happen since we are looking in the
        // default catalog, but just in case...
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }

    string upStr = upInfo.str();
    return CORBA::string_dup(upStr.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


char *
SubarrayControlImpl::whazUpOptical( )
try {
    ostringstream upInfo;
    SourceChecker checker;
    checker.setElevLimit(elevLimit_);
    checker.setMJD();
    checker.showHeader(true);
    try {
        for( SourceIterator si = sourceCatalog_.catalogBegin();
             si != sourceCatalog_.catalogEnd(); si++)
        {
            Source source = si->second;
            if ( source.isOptical() )
            {
                checker.setSource(source);
                if ( checker.isUp() ) {
                    upInfo << checker.info() << endl;
                    checker.showHeader(false);
                }
            }
        }

        // if user catalog is in use, then print info for that too.
        if ( ! userCatalog_.empty() ) {
            upInfo << endl << " User Catalog: " << userCatalog_ << endl;
            SourceCatalog usercat;
            usercat.open(userCatalog_);
            for( SourceIterator si = usercat.catalogBegin();
                    si != usercat.catalogEnd(); si++)
            {
                Source source = si->second;
                if ( source.isOptical() )
                {
                    checker.setSource(source);
                    if ( checker.isUp() ) {
                        upInfo << checker.info() << endl;
                        // no headers after the first one
                        checker.showHeader(false);
                    }
                }
            }
        }
        checker.showHeader(true);

    } catch ( const SourceNotFoundException & snfe ) {
        // this shouldn't happen since we are looking in the
        // default catalog, but just in case...
        throw CARMA_EXCEPTION( util::UserException, snfe.getMessage());
    } catch ( const IllegalArgumentException & iae ) {
        ostringstream foo;
        foo << "Error reading catalog. Perhaps there is a space character in a source name? ";
        foo << iae.getMessage();
        throw CARMA_EXCEPTION( util::UserException, foo.str().c_str() );
    } catch ( const util::UserException & ) {
        throw;
    } catch ( ... ) {
        rethrowCaughtAsUser();

        // Just in case rethrowCaughtAsUser() is broken
        throw util::UserException( "rethrowCaughtAsUser() is broken",
                                   __FILE__, __LINE__ );
    }

    string upStr = upInfo.str();
    return CORBA::string_dup(upStr.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


double
SubarrayControlImpl::lst( )
try {
    return AstroTime(obs_->getReference()).localSiderealTime();
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


double
SubarrayControlImpl::mjd( const double seconds )
try {
    return Time::MJD() + seconds / AstroTime::SECONDS_PER_DAY ;
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


char *
SubarrayControlImpl::lstString( )
try {
    AstroTime at(obs_->getReference());
    string lstStr = at.lstString();
    return CORBA::string_dup(lstStr.c_str());
} catch ( const util::UserException & ) {
    throw;
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

SubarrayControlImpl::ProjectDatabaseGroup
SubarrayControlImpl::getProjectDatabaseGroup()
{
    ProjectDatabaseGroup result;

    ProjectDatabaseManagerHandle * const pdp = projectDatabaseManager_.get();

    if ( pdp != 0 )
        result.insert( pdp );

    return result;
}


observertools::ProjectSequence *
SubarrayControlImpl::queryProject(
                        const observertools::ItemValueSequence & ivSeq )
try {
    // mutex protect this call
    const PDBLock pdblock( gPDBGuard );

    const string methodName = "SaCI::queryProject";
    const ScopedLogNdc ndc( methodName );
    //programLogNoticeIfPossible( "Acquired lock.");
    CARMA_CPTRACE( Trace::TRACE4, "Entering" );

    if ( ivSeq.length() == 0 ) {
        ostringstream os;
        os << methodName << ": input ItemValueSequence length is zero.";
        throw CARMA_ERROR( os.str() );
    }

    ostringstream hos;
    hos << "ProjectDatabaseHandle::projectQuery( ItemValueSequence = "
	<< observertools::itemValueSequenceToString( ivSeq )
	<< " )";
    const string handleMethod = hos.str();

    CARMA_CPTRACE( Trace::TRACE4, "Calling " << handleMethod << "via WRS" );
    WorkResultSet wrs( handleMethod + " result set" );

    const ProjectDatabaseGroup projectDatabaseGroup = getProjectDatabaseGroup();
    CARMA_CPTRACE( Trace::TRACE4, "got project database group");

    if ( ! projectDatabaseGroup.empty() ) {

        // Generate an ID number for this request.
        // This is to handle overlapping requests or
        // timeouts on a request, so that the handle
        // hands back the correct project sequence.
        pdmRequestId_++;
        queueFunctorWorkRequestGroup(
            handleMethod,
            makeHandleMethodFunctorGroup(
        	projectDatabaseGroup,
        	&ProjectDatabaseManagerHandle::projectQuery,
        	ivSeq,
        	pdmRequestId_),
            wrs,
            *workerPool_,
            false );
         CARMA_CPTRACE( Trace::TRACE4, "Queued Functor Work Request");

        // wait up to 2 minutes.  Note waitForAllNormal
        // will wait 2*lateAfterMillis before giving up!
        const unsigned long lateAfterMillis = 60000UL;
        waitForAllNormal( wrs, lateAfterMillis, false );

        CARMA_CPTRACE( Trace::TRACE4, "Exiting" );

        ProjectDatabaseManagerHandle * const pdp
	                             = projectDatabaseManager_.get();
        if ( pdp != 0 ) {
            //programLogNoticeIfPossible( " Releasing lock via return.");
            return pdp->getProjectSequence( pdmRequestId_ );
        }
    }

    throw CARMA_ERROR("ProjectDatabaseManagerHandle is null/group is empty");

} catch ( ... ) {
    programLogNoticeIfPossible( "SaCI::queryProject Releasing lock via catch.");
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

short
SubarrayControlImpl::projectRun(const char* projectID, const char* obsblock,
                     const char* subObsblock, const bool isCommissioning,
                     const bool isDualCorr, const char* arrayConfig1,
                     const char* arrayConfig2,
                     const char* scriptFile, const char* catalogFile)
try {
    // mutex protect this call
    const PDBLock pdblock( gPDBGuard );

    const string methodName = "SaCI::projectRun";
    const ScopedLogNdc ndc( methodName );
    //programLogNoticeIfPossible( "Acquired lock.");
    CARMA_CPTRACE( Trace::TRACE4, "Entering" );

    ostringstream hos;
    hos << "ProjectDatabaseHandle::projectRun( projectID = "
        << projectID << ", obsblock = " << obsblock << ", subObsblock = "
        << subObsblock << ", isCommissioning = " << isCommissioning
        << ", isDualCorr = " << isDualCorr << ", arrayConfig1 = "
        << arrayConfig1 << ", arrayConfig2 = " << arrayConfig2
        << ", scriptFile = " << scriptFile << ", catalogFile = "
        << catalogFile << " )";

    const string handleMethod = hos.str();
    cmdlog() << handleMethod;

    const ProjectDatabaseGroup projectDatabaseGroup = getProjectDatabaseGroup();
    CARMA_CPTRACE( Trace::TRACE4, "got project database group");

    if (projectDatabaseGroup.empty()) {
        throw CARMA_ERROR("ProjectDatabaseManagerHandle is null/group is empty");
    }

    CARMA_CPTRACE( Trace::TRACE4, "Calling " << handleMethod << "via WRS" );
    WorkResultSet wrs( handleMethod + " result set" );

    // Generate an ID number for this request.
    // This is to handle overlapping requests or
    // timeouts on a request, so that the handle
    // hands back the correct project sequence.
    runId_++;

    queueFunctorWorkRequestGroup(
        handleMethod,
        makeHandleMethodFunctorGroup(
            projectDatabaseGroup,
            &ProjectDatabaseManagerHandle::runProject,
            projectID,
            obsblock,
            subObsblock,
            isCommissioning,
            isDualCorr,
            arrayConfig1,
            arrayConfig2,
            scriptFile,
            catalogFile,
            runId_),
        wrs,
        *workerPool_);

     CARMA_CPTRACE( Trace::TRACE4, "Queued Functor Work Request");

    // wait up to 2 minutes.  Note waitForAllNormal
    // will wait 2*lateAfterMillis before giving up!
    const unsigned long lateAfterMillis = 60000UL;
    waitForAllNormal( wrs, lateAfterMillis, false );

    CARMA_CPTRACE( Trace::TRACE4, "Exiting" );

    ProjectDatabaseManagerHandle * const pdp = projectDatabaseManager_.get();
    if ( pdp != 0 ) {
        //programLogNoticeIfPossible( " Releasing lock via return.");
        return pdp->getRunResult( runId_ );
    }

} catch ( ... ) {
    programLogNoticeIfPossible( "SaCI::queryProject Releasing lock via catch.");
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


void
SubarrayControlImpl::projectOscriptAdd(
	const char * project,
	const char * obsblock,
	const char * subobsblock,
	const char * scriptFile,
	const char * catalogFile )

try {
    // mutex protect this call
    const PDBLock pdblock( gPDBGuard );

    const string methodName = "SaCI::projectOScriptAdd";
    const ScopedLogNdc ndc( methodName );
    //programLogNoticeIfPossible( "Acquired lock.");
    const string handleMethod("ProjectDatabaseHandle::addScriptOrCatalog()");

    CARMA_CPTRACE( Trace::TRACE4, "Calling " << handleMethod << "via WRS" );

    WorkResultSet wrs( handleMethod + " result set" );

    const ProjectDatabaseGroup projectDatabaseGroup = getProjectDatabaseGroup();

    if ( ! projectDatabaseGroup.empty() ) {

	// structure to hold project name data
	observertools::ProjectId pid;
	pid.project = string( project );
	pid.obsblock = string( obsblock );
	pid.subobsblock = string( subobsblock );

	queueFunctorWorkRequestGroup(
	    handleMethod,
	    makeHandleMethodFunctorGroup(
		projectDatabaseGroup,
		&ProjectDatabaseManagerHandle::addScriptOrCatalog,
		pid,
		scriptFile,
		catalogFile
		),
	    wrs,
	    *workerPool_,
	    false );

	// wait up to 1 minute.
	const unsigned long lateAfterMillis = 60000UL;
	waitForAllNormal( wrs, lateAfterMillis, false );

	CARMA_CPTRACE( Trace::TRACE4, "Exiting" );
        //programLogNoticeIfPossible( " Releasing lock via return.");

    }

} catch ( ... ) {
    programLogNoticeIfPossible( "SaCI::projectOScriptAdd Releasing lock via catch.");
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

bool
SubarrayControlImpl::projectEdit( const char * project,
                         const char * obsblock,
	                 const char * subobsblock,
                         CORBA::Short trial,
                         const observertools::ItemValueSequence & ivSeq,
                         const observertools::EditStatus action)
try {

    // mutex protect this call
    const PDBLock pdblock( gPDBGuard );
    const string methodName = "SaCI::projectEdit";
    const ScopedLogNdc ndc( methodName );
    //programLogNoticeIfPossible( "Acquired lock.");

// removed to allow for specialized querries that use an IVseq length of zero
/*    if ( ivSeq.length() == 0 ) {
	ostringstream os;
	os << methodName << ": input ItemValueSequence length is zero.";
	throw CARMA_ERROR( os.str() );
	}*/

    const string handleMethod("ProjectDatabaseHandle::addScriptOrCatalog()");

    CARMA_CPTRACE( Trace::TRACE4, "Calling " << handleMethod << "via WRS" );

    WorkResultSet wrs( handleMethod + " result set" );

    const ProjectDatabaseGroup projectDatabaseGroup = getProjectDatabaseGroup();

    if ( ! projectDatabaseGroup.empty() ) {

	// structure to hold project name data
	observertools::ProjectId pid;
	pid.project = string( project );
	pid.obsblock = string( obsblock );
	pid.subobsblock = string( subobsblock );
	pid.trial = trial;

	// Generate an ID number for this edit
	// This is to handle overlapping requests or
	// timeouts on a request, so that the handle
	// hands back the correct boolen result
	pdmEditId_++;

	queueFunctorWorkRequestGroup(
	    handleMethod,
	    makeHandleMethodFunctorGroup(
		projectDatabaseGroup,
		&ProjectDatabaseManagerHandle::projectEdit,
		pid,
		ivSeq,
		action,
		pdmEditId_
		),
	    wrs,
	    *workerPool_,
	    false );

	// wait up to 1 minute.
	const unsigned long lateAfterMillis = 60000UL;
	waitForAllNormal( wrs, lateAfterMillis, false );

	CARMA_CPTRACE( Trace::TRACE4, "Exiting" );

	ProjectDatabaseManagerHandle * const pdp
	                             = projectDatabaseManager_.get();
	if ( pdp != 0 ) {
            //programLogNoticeIfPossible( "Releasing lock via return.");
	    return pdp->getEditResult( pdmEditId_ );
	}
    }

    throw CARMA_ERROR("ProjectDatabaseManagerHandle is null/group is empty");


} catch ( ... ) {
    programLogNoticeIfPossible( "SaCI::projectEdit releasing lock via catch.");
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}


carma::control::NearestInfoSeq *
SubarrayControlImpl::getNearest( const char * source,
            CORBA::Float elMin,
            CORBA::Float elMax,
            const SeqString & sourceList,
            NearestActionType action,
            CORBA::Short numReturn,
            CORBA::Boolean ignoreNorthSouth,
            const char * coordSys,
            CORBA::Boolean optical,
            CORBA::Float fluxLimit,
            CORBA::Float frequency )
try {
    const string sname(source);
    ScopedLogNdc ndc("SubarrayControlImpl::getNearest("+sname+")");

    const string RADEC_STR("RADEC");
    const string AZEL_STR("AZEL");

    // figure out how in what coordinate system
    // they want the calculation done.
    // Right now only, RADEC and AZEL are supported.
    // Maybe someday in the future TIME will be supported, but probably not!
    const string csStr( coordSys );
    coordSysType cType;
    if ( StringUtils::equalsIgnoreCase( csStr , RADEC_STR ) )
        cType = services::COORDSYS_RADEC;
    else {
        if ( StringUtils::equalsIgnoreCase( csStr , AZEL_STR ) )
            cType = services::COORDSYS_AZEL;
        else  {
            ostringstream errOs;
            errOs << "Unknown coordSys parameter " << coordSys
                  << ".  Valid values are "
                  << RADEC_STR
                  << " and " << AZEL_STR;
            throw CARMA_EXCEPTION( IllegalArgumentException, errOs );
        }
    }

    SourceChecker sc;

    if (StringUtils::equalsIgnoreCase(source, "Subarray")) {
        // Special case: if the reference source is "Subarray" then
        // we try to use the current RA/Dec. We can't assume that this
        // source still exists in a catalog because the user catalog may
        // be unloaded.
        carmaMonitor_.readNewestConditionalCopy();
        ControlSubsystemBase::Subarray& sa =
        	carmaMonitor_.control().subarray(subarrayNo_-1);
        string sname = sa.source().getValue();
        double ra    = sa.phaseCenterRa().getValue();
        double dec   = sa.phaseCenterDec().getValue();
        bool valid   = sa.source().isValid()           &&
                       (!StringUtils::equalsIgnoreCase(sname, "None")) &&
                       sa.phaseCenterRa().isValid()    &&
                       sa.phaseCenterDec().isValid();
        if (valid) {
            Source src(sname, Angle(ra,"radians"), Angle(dec,"radians"),
        	       Velocity(0.0,"km/s"), Angle(0.0,"radians"));
            sc.setSource(src);
        }
        else {
            // The subarray doesn't have a valid source; use Polaris
            sc.setSource("AUMI", userCatalog_);
        }
    }
    else {
        // Common usage; source name passed in as argument
        programLogInfoIfPossible("Setting source in SourceChecker. This involves a disk read.");
        sc.setSource( sname, userCatalog_ );
    }

    sc.setElevLimit( elMin );
    sc.setElevUpperLimit( elMax );
    // frequency from python wrapper is GHz.
    Frequency inputFreq(frequency,services::GHZ );
    sc.setFrequency( inputFreq.hertz() ); // in Hertz
    sc.setMJD( Time::MJD() );

    const sourcePntType pType = ( optical ? PNT_OPTICAL : PNT_RADIO );

    // We have to convert all input to upper case for
    // pattern matching in the getNearest call, since all
    // sources in system catalogs are upper case.
    vector<string> sourceListUpperVec
	            = convertSequenceToVector<string>( sourceList );
    StringUtils::toUpper( sourceListUpperVec );
    set<string> sourceListUpperSet;
    std::copy( sourceListUpperVec.begin(), sourceListUpperVec.end(),
	       inserter( sourceListUpperSet, sourceListUpperSet.begin() )
	     );
    NeighborSet neighborSet =
	sc.getNearest( sourceListUpperSet,
                       action == ACTION_INCLUDE,
                       numReturn,
                       ignoreNorthSouth,
                       cType,
                       pType,
                       fluxLimit
		     );
    NearestInfoSeq_var nis = convertNeighborSetToNearestInfoSeq( neighborSet );

    return nis._retn();
} catch ( ... ) {
    rethrowCaughtAsUser();

    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException( "rethrowCaughtAsUser() is broken",
                               __FILE__, __LINE__ );
}

