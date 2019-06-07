/**
 *
 * Implementation for a generic monitor system container base class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorContainer.cc,v 1.50 2011/05/10 14:21:03 scott Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorContainer.h"

#include <iomanip>
#include <iostream>
#include <string>
#include <stdexcept>

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
compileTimeChecks( )
{
    // Document and keep an eye the present size of a MonitorContainer instance
#if __WORDSIZE == 64
    compileTimeCheck< (sizeof( MonitorContainer ) == 80) >();
    compileTimeCheck< (sizeof( MonitorContainer::Child ) == 16) >();
#else
    compileTimeCheck< (sizeof( MonitorContainer ) == 40) >();
    compileTimeCheck< (sizeof( MonitorContainer::Child ) == 8) >();
#endif
}


}  // namespace < anonymous >


MonitorContainer::Child::Child( MonitorComponent & component ) :
componentPtr_( &component ),
flags_( 0 )
{
    if ( componentPtr_ == 0 )
        throw CARMA_ERROR( "MonitorComponent instance pointer is NULL" );

    MonitorPoint * const dynamicMpPtr =
        dynamic_cast< MonitorPoint * >( componentPtr_ );

    MonitorContainer * const dynamicContainerPtr =
        dynamic_cast< MonitorContainer * >( componentPtr_ );

    MonitorSubsystem * const dynamicSubsystemPtr =
        dynamic_cast< MonitorSubsystem * >( componentPtr_ );

    if ( (dynamicMpPtr == 0) && (dynamicContainerPtr == 0) ) {
        throw CARMA_ERROR( "MonitorComponent instance"
                           " is neither a MonitorPoint instance nor"
                           " a MonitorContainer instance" );
    }

    if ( (dynamicMpPtr != 0) && (dynamicContainerPtr != 0) ) {
        throw CARMA_ERROR( "MonitorComponent instance"
                           " is both a MonitorPoint instance and"
                           " a MonitorContainer instance" );
    }

    if ( (dynamicSubsystemPtr != 0) && (dynamicContainerPtr == 0) ) {
        throw CARMA_ERROR( "MonitorComponent instance"
                           " is a MonitorSubsystem instance but not"
                           " a MonitorContainer instance" );
    }

    if ( dynamicMpPtr != 0 ) {
        flags_ |= kMpBitMask;

        MonitorPoint * const staticMpPtr =
            static_cast< MonitorPoint * >( componentPtr_ );

        if ( staticMpPtr != dynamicMpPtr ) {
            throw CARMA_ERROR( "MonitorComponent instance"
                               " is a MonitorPoint instance but"
                               " is not statically castable as such" );
        }
    }

    if ( dynamicContainerPtr != 0 ) {
        flags_ |= kContainerBitMask;

        MonitorContainer * const staticContainerPtr =
            static_cast< MonitorContainer * >( componentPtr_ );

        if ( staticContainerPtr != dynamicContainerPtr ) {
            throw CARMA_ERROR( "MonitorComponent instance"
                               " is a MonitorContainer instance but"
                               " is not statically castable as such" );
        }
    }
    
    if ( dynamicSubsystemPtr != 0 ) {
        flags_ |= kSubsystemBitMask;

        MonitorSubsystem * const staticSubsystemPtr =
            static_cast< MonitorSubsystem * >( componentPtr_ );

        if ( staticSubsystemPtr != dynamicSubsystemPtr ) {
            throw CARMA_ERROR( "MonitorComponent instance"
                               " is a MonitorSubsystem instance but"
                               " is not statically castable as such" );
        }
    }
}


MonitorComponent &
MonitorContainer::Child::handleBadComponentDeref( ) const
{
    throw CARMA_ERROR( "Bad component deref of MonitorContainer child" );
}


MonitorPoint &
MonitorContainer::Child::handleBadMpDeref( ) const
{
    throw CARMA_ERROR( "Bad mp deref of MonitorContainer child" );
}


MonitorContainer &
MonitorContainer::Child::handleBadContainerDeref( ) const
{
    throw CARMA_ERROR( "Bad container deref of MonitorContainer child" );
}


MonitorSubsystem &
MonitorContainer::Child::handleBadSubsystemDeref( ) const
{
    throw CARMA_ERROR( "Bad subsystem deref of MonitorContainer child" );
}


MonitorSubsystem *
MonitorContainer::Child::subsystemPtr( ) const
{
    if ( flags_ & kSubsystemBitMask )
        return static_cast< MonitorSubsystem * >( componentPtr_ );
    else
        return 0;
}


MonitorSubsystem &
MonitorContainer::Child::subsystemRef( ) const
{
    if ( flags_ & kSubsystemBitMask )
        return *static_cast< MonitorSubsystem * >( componentPtr_ );
    else
        return handleBadSubsystemDeref();
}


const MonitorContainer::Child MonitorContainer::kNullChild;


MonitorContainer::MonitorContainer( const string & name ) :
MonitorComponent(name)
{
}


MonitorContainer::~MonitorContainer( )
{
}


int
MonitorContainer::getNumMonitorPoints( const bool recurse ) const
{
    int total = 0;

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->isMp() )
            total++;
        else if ( recurse && i->isContainer() )
            total += i->containerPtr()->getNumMonitorPoints( true );
    }

    return total;
}


int
MonitorContainer::getNumContainerChildren( ) const
{
    int result = 0;

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->isContainer() )
            ++result;
    }

    return result;
}


int
MonitorContainer::getNumDescendants( ) const
{
    int result = children_.size();

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->isContainer() )
            result += i->containerPtr()->getNumDescendants();
    }

    return result;
}


int
MonitorContainer::getNumContainerDescendants( ) const
{
    int result = 0;

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->isContainer() ) {
            ++result;

            result += i->containerPtr()->getNumContainerDescendants();
        }
    }

    return result;
}


void
MonitorContainer::setMonitorPointAttributes( )
{

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->isContainer() )
            i->containerPtr()->setMonitorPointAttributes();
    }
    
    // The order here is important. By setting the local monitor points
    // last the tree will be filled from the bottom up. This is essential
    // in propagating container archive priorities to MP's.
    setLocalMonitorPointAttributes();
}


void
MonitorContainer::add( MonitorComponent & component )
{
    children_.push_back( Child( component ) );
}


void
MonitorContainer::setLocalMonitorPointAttributes( )
{
   // Default is to do nothing
   // However, it is overridden in MPML generated code all over the place
}


// Hierarchical name, so walk down the tree to find it
MonitorContainer::Child
MonitorContainer::findDescendant( const MonitorContainer & rootContainer,
                                  const string &           hierarchicalName,
                                  const bool               caseSensitive )
{
    const MonitorContainer * c = &rootContainer;
    string hierarchicalNameLeft = hierarchicalName;
    string childName;
    
    while ( true ) {
        const string::size_type firstDotPos = hierarchicalNameLeft.find( '.' );
    
        if ( firstDotPos == string::npos )
            childName = hierarchicalNameLeft;
        else
            childName = hierarchicalNameLeft.substr( 0, firstDotPos );
    
        vector< Child >::const_iterator i = c->children_.begin();
        const vector< Child >::const_iterator iEnd = c->children_.end();
    
        if ( caseSensitive ) {
            for ( ; i != iEnd; ++i ) {
                if ( i->componentPtr_->getName().compare( childName ) == 0 )
                    break;
            }
        } else {
            for ( ; i != iEnd; ++i ) {
                if ( util::StringUtils::equalsIgnoreCase(
                        childName,
                        i->componentPtr_->getName() ) )
                    break;
            }
        }
        
        if ( i == iEnd )
            break;  // No child matching childName so we fail
            
        if ( firstDotPos == string::npos )
            return *i;  // no more hierarchical name left so we're done
    
        c = i->containerPtr();
    
        if ( c == 0 )
            break; // child is not a container so we fail
    
        hierarchicalNameLeft.erase( 0, (firstDotPos + 1) );
    }
    
    return Child();  // We failed
}


void MonitorContainer::setValidity(MonitorPoint::VALIDITY validity)
{
    MonitorPointIterator iter(*this);
    while (++iter) {
        MonitorPoint& m = iter.getMonitorPoint();
        for (int i = 0; i < m.getNumSamples(); i++) {
            m.setValidity(validity, i);
        }
    }
}

MonitorPoint& MonitorContainer::getFirstMonitorPoint()
{
    MonitorPointIterator iter(*this);
    ++iter;
    return iter.getMonitorPoint();
}

int MonitorContainer::getNumMonitorPoints()
{
    int count = 0;
    MonitorPointIterator iter(*this);
    while (++iter) count++;
    return count;
}

int MonitorContainer::getNumSamples()
{
    int count = 0;
    MonitorPointIterator iter(*this);
    while (++iter) {
        MonitorPoint& m = iter.getMonitorPoint();
        count += m.getNumSamples();
    }
    return count;
}


MonitorComponent *
MonitorContainer::getComponentPtr( const string & hierarchicalName,
                                   const bool     caseSensitive ) const
{
    return findDescendant( *this,
                           hierarchicalName,
                           caseSensitive ).componentPtr_;
}


// Brute force lookup for now, descending the hierarchy...
MonitorPoint *
MonitorContainer::getMonitorPointPtr( const tagIDType tagId ) const
{
    MonitorPointIterator iter( *this );

    while ( ++iter ) {
        MonitorPoint & m = iter.getMonitorPoint();

        if ( m.getTagID() == tagId )
            return &m;
    }

    CARMA_CPTRACE( util::Trace::TRACE1, "getMonitorPoint("
                                        << tagId
                                        << "): not found" );

    return 0;
}


MonitorPoint *
MonitorContainer::getMonitorPointPtr( const string & hierarchicalName,
                                      const bool     caseSensitive ) const
{
    return findDescendant( *this,
                           hierarchicalName,
                           caseSensitive ).mpPtr();
}


MonitorPoint &
MonitorContainer::getMonitorPoint( const tagIDType tagId ) const
{
    MonitorPoint * const mp = getMonitorPointPtr( tagId );

    if ( mp == 0 ) {
        ostringstream oss;

        oss << "getMonitorPointPtr(" << tagId << ") returned NULL.";

        throw CARMA_ERROR( oss.str() );
    }

    return *mp;
}


MonitorPoint &
MonitorContainer::getMonitorPoint( const string & hierarchicalName,
                                   const bool     caseSensitive ) const
{
    MonitorPoint * const mp =
        getMonitorPointPtr( hierarchicalName, caseSensitive );

    if ( mp == 0 ) {
        ostringstream oss;

        oss << "getMonitorPointPtr(\"" << hierarchicalName << "\", "
            << (caseSensitive ? "true" : "false")
            << ") returned NULL.";

        throw CARMA_ERROR( oss.str() );
    }

    return *mp;
}


namespace {

const char * const kOpEqDiagString = "MonitorContainer::==, ";

}  // namespace < anonymous >


// Checks type, name and equality of all contained components
bool
MonitorContainer::operator==( const MonitorComponent & rhs ) const
{
    // Is rhs a MonitorContainer?
    const MonitorContainer * const rhsMcPtr =
        dynamic_cast< const MonitorContainer * >( &rhs );

    if ( rhsMcPtr == 0 ) {
        if (debug_ == true) {
            cout << kOpEqDiagString << "not a container:"
                 << rhs.getName() << endl;
        }
        return false;
    }

    // Handy ref to type specific component
    const MonitorContainer & rhsMc = *rhsMcPtr;

    // Check name for equality of name
    if ( rhsMc.getName() != getName() ) {
        if (debug_ == true) {
            cout << kOpEqDiagString << "names not equal:"
                 << rhs.getName() << endl;
        }
        return false;
    }

    // Check for same number of components
    if ( rhsMc.getNumChildren() != getNumChildren() ) {
        if (debug_ == true) {
            cout << kOpEqDiagString << "numComponents not equal:"
                 << rhs.getName() << endl;
        }
        return false;
    }

    // Check all components for equality
    for (int i=0; i<getNumChildren(); i++) {
        if (!(rhsMc.getChild(i).componentRef() == getChild(i).componentRef())) {
            if (debug_ == true) {
                cout << kOpEqDiagString << "component not equal:"
                    << rhs.getName() << "."
                    << rhsMc.getChild(i).componentPtr()->getName() << endl;
            }
            return false;
        }
    }

    return true;
}


void
MonitorContainer::setNoData( ) const
{
    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i )
        i->componentPtr_->setNoData();
}


bool
MonitorContainer::hasAllData( ) const
{
    bool debug = false;

    if ( debug ) {
        cout << getCanonicalName() << ".hasAllData():"
             << " numComps=" << getNumChildren() << endl;
    }

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();

    for ( ; i != iEnd; ++i ) {
        if ( i->componentPtr_->hasAllData() == false )
            return false;
    }

    return true;
}


// The sampleIndex is not used...
string MonitorContainer::toString(bool canonicalName, bool verbose,
        bool value, int sampleIndex, int indent) const
{
    ostringstream o;
    o << setw(indent) << "";
    if (canonicalName) {
        o << getCanonicalName();
    }
    else {
        o << getName();
    }
    o << getPhysicalDeviceString() ;

    if (value) {
        if (hasAllData()) {
            o << "   ";
        }
        else {
            o << " NC";  // for NotComplete
        }
    }

    if (verbose) {
        o << "  " << "shortName:"     << getShortName()
          << "  " << "numComponents:" << getNumChildren()
          << "  " << "MPs:"           << getNumMonitorPoints()
          << "  " << "containers:"    << getNumContainerChildren()
          << "\n"
          << setw(indent) << ""
          << "Desc:" << "\'" << getDescription() << "\'"
          << "\n";
     }
     return o.str();
}

string MonitorContainer::toStringAverage(bool canonicalName, bool verbose,
        bool value, int indent) const
{
    return toString(canonicalName, verbose, value, 0, indent);
}

void MonitorContainer::setPersistent(bool persistent)
{
    MonitorPointIterator iter(*this);

    while (iter++) {
        iter.getMonitorPoint().setPersistent(persistent);
    }
}

void MonitorContainer::setArchivePriority(
        const MonitorComponent::ARCHIVE_PRIORITY priority, bool onlyDEFAULT)
{
    MonitorPointIterator iter(*this);

    while (iter++) {
        if (onlyDEFAULT) {
            iter.getMonitorPoint().setDefaultArchivePriority(priority);
        }
        else {
            iter.getMonitorPoint().setArchivePriority(priority);
        }
    }

}


string
MonitorContainer::monitorPointTags( const bool untagged ) const
{
    string result;

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();
    
    for ( ; i != iEnd; ++i )
        result += i->componentPtr_->monitorPointTags( untagged );

    return result;
}


string
MonitorContainer::hierarchyToString( const bool canonical,
                                     const bool verbose,
                                     const bool value,
                                     const int  sampleIndex,
                                     const int  indent,
                                     const int  levels ) const
{
    string result = toString( canonical, verbose, value, sampleIndex, indent );
    if (!verbose) result += "\n";

    if ( levels != 0 ) {
        const int childLevels = levels - 1;
        const int childIndent = indent + 1;

        vector< Child >::const_iterator i = children_.begin();
        const vector< Child >::const_iterator iEnd = children_.end();
        
        for ( ; i != iEnd; ++i ) {
            result += i->componentPtr_->hierarchyToString( canonical,
                                                           verbose,
                                                           value,
                                                           sampleIndex,
                                                           childIndent,
                                                           childLevels );
        }
    }

    return result;
}


void
MonitorContainer::hierarchyToVector( vector< string > & hierarchyList,
                                     const bool         canonical,
                                     const bool         verbose,
                                     const int          sampleIndex ) const
{
    hierarchyList.push_back( toString( canonical, verbose, sampleIndex, 0 ) );
    
    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();
    
    for ( ; i != iEnd; ++i ) {
        i->componentPtr_->hierarchyToVector( hierarchyList,
                                             canonical,
                                             verbose );
    }
}


string
MonitorContainer::hierarchyToStringAverage( const bool canonical,
                                            const bool verbose,
                                            const bool value,
                                            const int  indent,
                                            const int  levels ) const
{
    string result = toStringAverage( canonical, verbose, value, indent );
    if (!verbose) result += "\n";

    if ( levels != 0 ) {
        const int childLevels = levels - 1;
        const int childIndent = indent + 1;

        vector< Child >::const_iterator i = children_.begin();
        const vector< Child >::const_iterator iEnd = children_.end();
        
        for ( ; i != iEnd; ++i ) {
            result +=
                i->componentPtr_->hierarchyToStringAverage( canonical,
                                                            verbose,
                                                            value,
                                                            childIndent,
                                                            childLevels );
        }
    }

    return result;
}


string
MonitorContainer::leafToString( const bool verbose,
                                const bool value,
                                const int  sampleIndex ) const
{
    string result;

    if ( children_.empty() ) {  // Contains no components, must be leaf
        result += toString(false, verbose, value, sampleIndex, 0);
        if (!verbose) result += "\n";
    } else {  // Has components, is not a leaf
        vector< Child >::const_iterator i = children_.begin();
        const vector< Child >::const_iterator iEnd = children_.end();
        
        for ( ; i != iEnd; ++i ) {
            result += i->componentPtr_->leafToString( verbose,
                                                      value,
                                                      sampleIndex );
        }
    }

    return result;
}


void
MonitorContainer::setCanonicalName( const string & parents )
{
    MonitorComponent::setCanonicalName( parents );

    const string & canonicalName = getCanonicalName();

    vector< Child >::const_iterator i = children_.begin();
    const vector< Child >::const_iterator iEnd = children_.end();
    
    for ( ; i != iEnd; ++i )
        i->componentPtr_->setCanonicalName( canonicalName );
}
