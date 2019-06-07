/**
 *
 * Implementation for a generic monitor system component base class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorComponent.cc,v 1.36 2010/02/18 17:00:08 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include <sstream>
#include <string>
#include <stdexcept>

#include "carma/monitor/MonitorComponent.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"


using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
compileTimeChecks( )
{
    // Document and keep an eye the present size of a MonitorComponent instance
#if __WORDSIZE == 64
    compileTimeCheck< (sizeof( MonitorComponent ) == 56) >();
#else
    compileTimeCheck< (sizeof( MonitorComponent ) == 28) >();
#endif

}


const string kDefaultDescription( "indescribable" );


}  // namespace < anonymous >


MonitorComponent::MonitorComponent( const string & componentName ) :
debug_( false ),
name_( componentName ),
canonicalName_( componentName ),
shortName_( componentName ),
description_( kDefaultDescription )
{
}


MonitorComponent::~MonitorComponent()
{
    if (debug_)cout << "MonitorComponent destructor" << endl;
}


const string &
MonitorComponent::getName( ) const
{
    return name_;
}


void
MonitorComponent::setName( const string & name )
{
    name_ = name;
    canonicalName_ = name;
}


void
MonitorComponent::setCanonicalName( const string & parents )
{
    string c;
    
    if ( parents.empty() )
        c = name_;
    else {
        c.reserve( parents.size() + 1 + name_.size() );
        c.append( parents );
        c.push_back( '.' );
        c.append( name_ );
    }

    canonicalName_.swap( c );
}


const string &
MonitorComponent::getShortName( ) const
{
    return shortName_;
}


void
MonitorComponent::setShortName( const string & str )
{
    shortName_ = str;
}


void
MonitorComponent::setShortName( const string & str,
                                const int      index )
{
    if ( index > 0 ) {
        ostringstream oss;

        oss << str << index;

        shortName_ = oss.str();
    } else
        shortName_ = str;
}


const string &
MonitorComponent::getLongName( ) const
{
    return longName_;
}


void
MonitorComponent::setLongName( const string & str )
{
    longName_ = str;
}


const string &
MonitorComponent::getDescription( ) const
{
    return description_;
}


void
MonitorComponent::setDescription( const string & text )
{
    description_ = text;
}


bool MonitorComponent::isSubsystem() const
{
    // Default behavior
    return false;
}

std::string MonitorComponent::transportStatisticsToString(bool canonical) const
{
    ostringstream o;

    o << "Transport statistics for ";
    if (canonical)
        o << getCanonicalName();
    else
        o << getName();
    o << " Transport statistics are available only at the "
      << std::endl
      << "monitor subsystem or monitor system level, at the ACC."
      << std::endl;
    return o.str();
}

// Provide default implementation
string
MonitorComponent::getPhysicalDeviceName( ) const
{
    return string();
}


string
MonitorComponent::getPhysicalDeviceString( ) const
{
    string p = getPhysicalDeviceName();

    if ( p.empty() )
        return string();

    p.reserve( p.size() + 2 );
    p.insert( 0, 1, '(' );
    p.push_back( ')' );

    return p;
}


string
MonitorComponent::getPhysicalName( ) const
{
    string p = getPhysicalDeviceName();

    if ( p.empty() )
        return name_;

    p.reserve( p.size() + 1 + name_.size() );
    p.push_back( '.' );
    p.append( name_ );

    return p;
}


string MonitorComponent::convertAllLower(string name)
{
    string n = name;
    for(unsigned int i=0; i<n.size(); i++) n[i] = tolower(n[i]);
    return n;
}

// This default implementation returns false
bool MonitorComponent::isMonitorPoint() const
{
    return false;
}

bool MonitorComponent::operator!=(const MonitorComponent& c) const
{
    return !operator==(c);
}

std::ostream& carma::monitor::operator<<(ostream &os,
        const MonitorComponent& component)
{
    os << component.toString();
    return os;
}


string MonitorComponent::archivePriorityToString(
    MonitorComponent::ARCHIVE_PRIORITY priority)
{
  switch (priority) {
  case VITAL:       return "VITAL";
  case USEFUL:      return "USEFUL";
  case NORMAL:      return "NORMAL";
  case DEBUG:       return "DEBUG";
  case VERBOSE:     return "VERBOSE";
  case DEFAULT:     return "DEFAULT";
  case DONTARCHIVE: return "DONTARCHIVE";
  default:          return "No archive priority string!!";
  }
  return "archivePriorityToSting() - can't get here";
}
