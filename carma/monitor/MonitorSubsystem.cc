
/**
 *
 * Implementation for a generic monitor subsystem base class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSubsystem.cc,v 1.85 2014/04/02 23:11:09 iws Exp $
 * $CarmaCopyright$
 *
 */


#include "carma/monitor/MonitorSubsystem.h"

#include <iomanip>
#include <iostream>
#include <string>


#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/ScratchAverages.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SystemFrameBuffer.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"


using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


MonitorSubsystem::MonitorSubsystem(const string& subsystemName, 
                                   SystemFrameBuffer* const buffer,
                                   int maxMonitorPoints, int maxSamples,
                                   ARCHIVE_PRIORITY archivePriority,
                                   bool onlyDEFAULT):
        MonitorSystemContainer(subsystemName), 
        monitorPointSet_(
         ((buffer == NULL) ? 
           MonitorPointSet::getMonitorPointSet(
	          dbms::TagIDAuthority::getAuthority().
                  lookupSubsystemID(subsystemName),
                  maxMonitorPoints, maxSamples) :
           buffer->getMonitorPointSet(
              dbms::TagIDAuthority::getAuthority().
              lookupSubsystemID(subsystemName),
              maxMonitorPoints, maxSamples)
           )
        ),
        maxMonitorPoints_(maxMonitorPoints),
        maxSamples_(maxSamples),
        archivePriority_(archivePriority),
        onlyDEFAULT_(onlyDEFAULT)
{
    CPTRACE(Trace::TRACE6, "Subsystem " << subsystemName << " constructor complete");
}


MonitorSubsystem::~MonitorSubsystem() 
try {
    if ( debug_ )
        cout << "MonitorSubsystem (" << getName() << ") destructor" << endl;
                    
    // Delete the monitor point set and associated IPQ
    delete &monitorPointSet_;
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


bool
MonitorSubsystem::configure() 
{
    const ScopedLogNdc ndc( getName() + " MonitorSubsystem::configure" );

    createHierarchy();
    setIdentity();
    allocateAllMonitorPoints();
    setMonitorPointAttributes();
    // consolidate storage so there are no "holes"
    // due to the setNumSamplesPerCycle method calls.
    monitorPointSet().consolidateStorage();
    setArchivePriority(archivePriority_, onlyDEFAULT_);

    return true;
} 


namespace {


struct StackElem {
    MonitorContainer * container;
    int                nextChildIndex;
    
    explicit StackElem( MonitorContainer & c );
};


StackElem::StackElem( MonitorContainer & c ) :
container( &c ),
nextChildIndex( 0 )
{
}


}  // namespace < anonymous >


void
MonitorSubsystem::allocateAllMonitorPoints( )
{
    const ScopedLogNdc ndc( "MonitorSubsystem::allocateAllMonitorPoints" );

    vector< MonitorPoint * > mps;
    {
        mps.reserve( maxMonitorPoints_ );

        vector< StackElem > stack;

        stack.reserve( maxMonitorPoints_ );
        stack.push_back( StackElem( *this ) );
        
        while ( stack.empty() == false ) {
            MonitorContainer & c = *(stack.back().container);
            int i = stack.back().nextChildIndex;

            while ( true ) {
                if ( i >= c.getNumChildren() ) {
                    // We're done with this container
                    
                    stack.pop_back();
                    
                    break;
                }
                
                const MonitorContainer::Child child = c.getChild( i );

                ++i;

                MonitorPoint * const childAsMp = child.mpPtr();
                
                if ( childAsMp != 0 )
                    mps.push_back( childAsMp );
                else {
                    MonitorContainer * const childAsContainer =
                        child.containerPtr();
                        
                    if ( childAsContainer == 0 ) {
                        ostringstream oss;
                        
                        if ( child.isNull() )
                            oss << "getChild(" << i << ") returned NULL child";
                        else {
                            oss << "Dynamic cast to MonitorContainer for " 
                                << child.componentRef().getCanonicalName()
                                << " failed";
                        }
                        
                        throw CARMA_ERROR( oss.str() );
                    }
                    
                    stack.back().nextChildIndex = i;
                    
                    stack.push_back( StackElem( *childAsContainer ) );
                    
                    break;
                }
            }
        }
    }
    
    SubsystemFrame & ssFrame = monitorPointSet_.getSubsystemFrame();
            
    {
        vector< SubsystemFrame::AllocMpInfo > infos;

        infos.reserve( mps.size() );

        vector< MonitorPoint * >::const_iterator i = mps.begin();
        const vector< MonitorPoint * >::const_iterator iEnd = mps.end();
    
        for ( ; i != iEnd; ++i ) {
            MonitorPoint * const mp = *i;
            const tagIDType tagID = mp->getTagID();
            const unsigned short pointId = dbms::TagIDAuthority::getPointID( tagID );
                
            if ( pointId == 0 ) {
                ostringstream oss;
    
                oss << "Monitor point \"" << mp->getCanonicalName() << "\""
                    << "( " << tagID << ", " << pointId << ") "
                    << "has a monitor point ID of 0, so it can't be allocated.";
                  
                throw CARMA_ERROR( oss.str() );
            }
            
            SubsystemFrame::AllocMpInfo info;
            
            info.tagID = tagID;
            info.valueType = mp->getValuetype();
            info.nSamplesPerCycle = 1;
            
            infos.push_back( info );
        }

        if ( infos.empty() == false )
            ssFrame.allocateMonitorPoints( &(infos.at( 0 )), infos.size(), false );
    }
    
    {
        vector< MonitorPoint * >::const_iterator i = mps.begin();
        const vector< MonitorPoint * >::const_iterator iEnd = mps.end();
    
        for ( ; i != iEnd; ++i ) {
            MonitorPoint * const mp = *i;
            const int tagID = mp->getTagID();
            const int index = ssFrame.getIndex( tagID );
    
            mp->setMonitorPointHeader( ssFrame.getHeaderByIndex( index ) );
    
            mp->setAllValidity( MonitorPoint::INVALID_NO_DATA, false );
        }
    }
}


void
MonitorSubsystem::setIdentity()
{
    setCanonicalName( string() );

    dbms::TagIDAuthority & tagIDAuthority = dbms::TagIDAuthority::getAuthority();

    setMpTagIds( tagIDAuthority );
}


MonitorPointSet& MonitorSubsystem::getMonitorPointSet()
{
    return monitorPointSet_;
}

bool MonitorSubsystem::isSubsystem() const 
{
    return true;
}


std::string MonitorSubsystem::transportHeaderToString () 
{
    const int firstColumnWidth = 22;
    const int fieldWidth = 6;
    const int indent = 2;
    ostringstream o;
    
    o.setf(ios::fixed);
    
    o << setw(indent) << ""
      << setw(firstColumnWidth) 
      << left << "Component" << right 
      << setw(fieldWidth) << "Last" 
      << setw(fieldWidth) << "Last" 
      << setw(fieldWidth) << "FSP" 
      << setw(fieldWidth) << "FSP" 
      << setw(fieldWidth) << "Pub" 
      << setw(fieldWidth)  << "Rx" 
      << "\n";

    o << setw(indent) << "" 
      << setw(firstColumnWidth)
      << left << "Name"  << right
      << setw(fieldWidth) << "write" 
      << setw(fieldWidth) << "write" 
      << setw(fieldWidth) << "write" 
      << setw(fieldWidth) << "write" 
      << setw(fieldWidth) << "time" 
      << setw(fieldWidth) << "time" 
      << "\n";

    o << setw(indent) << "" 
      << setw(firstColumnWidth)<< "" 
      << setw(fieldWidth) << "delay" 
      << setw(fieldWidth) << "time" 
      << setw(fieldWidth) << "delay" 
      << setw(fieldWidth) << "time" 
      << setw(fieldWidth) << "" 
      << setw(fieldWidth) << "" 
      << "\n";

    return o.str();
}
    
std::string MonitorSubsystem::transportStatisticsToString (bool canonical) const
{
    const int firstColumnWidth = 22;
    const int fieldWidth = 6;
    const int indent = 2;
    ostringstream o;
    o.setf(ios::fixed);
    o << setw(indent) << "" << left;
    if (canonical) {
        o << setw(firstColumnWidth) << getCanonicalName();
    }
    else {
        o << setw(firstColumnWidth) << getName();
    }
    o << right;

    double frameTime = ::carma::util::Time::MJD(getFrameCount()+1);
    std::string separator = "  ";
    double writerWriteOffset = 0;
    double scriberWriteOffset = 0;
    double publishOffset = 0;
    double receiveOffset = 0;
    if (this->isCurrent())  {
        if (this->getLastWriteTime() > 0.0)  
            writerWriteOffset = 
                  round(1000*::carma::util::Time::SECONDS_PER_DAY*
                                   (this->getLastWriteTime() - frameTime));
    
	if (this->getScriberWriteTime() > 0.0)  
	    scriberWriteOffset = 
                  round(1000*::carma::util::Time::SECONDS_PER_DAY*
                                   (this->getScriberWriteTime() - frameTime));

	if (this->getPublishTime() > 0.0)  
	    publishOffset = 
                  round(1000*::carma::util::Time::SECONDS_PER_DAY*
                                   (this->getPublishTime() - frameTime));

	if (this->getReceiveTime() > 0.0)  
	    receiveOffset = 
                  round(1000*::carma::util::Time::SECONDS_PER_DAY*
                                   (this->getReceiveTime() - frameTime));

    }

    o << setw(fieldWidth)
      << setprecision(0) << round(1000*this->getLastWriterDelay())
      << setprecision(0) << setw(fieldWidth) << writerWriteOffset
      << setprecision(0) << setw(fieldWidth) 
          << round(1000*this->getScriberWriteDelay())
      << setprecision(0) << setw(fieldWidth) << scriberWriteOffset
      << setprecision(0) << setw(fieldWidth) << publishOffset
      << setprecision(0) << setw(fieldWidth) << receiveOffset
      << "\n";

    return o.str();
}


bool MonitorSubsystem::isCurrent () const
{
    return monitorPointSet_.getSubsystemFrame().isCurrentFrame();
}


bool MonitorSubsystem::isCurrent (long frameCount) const
{
    return (getFrameCount() == frameCount);
}


unsigned int MonitorSubsystem::read()
{
    return monitorPointSet_.read();
}
    
bool MonitorSubsystem::readNewest()
{
    return monitorPointSet_.readNewest();
}  
  
bool MonitorSubsystem::readNewestConditionalCopy()
{
    return monitorPointSet_.readNewestConditionalCopy();
}
    
void MonitorSubsystem::write( )
{
    
    // Write out data
    monitorPointSet_.write( false );
    
    // Reset validities.
    resetValidity();
}

void MonitorSubsystem::writeWithoutResettingValidities( )
{
    // Just write out the data
    monitorPointSet_.write( false );
}

void MonitorSubsystem::resetValidity()
{
    // Now mark all non-persistent monitor points as invalid.
    MonitorPointIterator mpi(*this);

    MonitorPoint* mp;
    while (mpi++) {
        mp = &mpi.getMonitorPoint();
        if (!mp->isPersistent()) { 
             mp->setAllValidity(MonitorPoint::INVALID_NO_DATA);
        }
    }    
}

double MonitorSubsystem::getFrameTime() const
{
    Time t;
    return t.MJD(getFrameCount());
}

double MonitorSubsystem::getPublishTime() const
{
    return monitorPointSet_.getSubsystemFrame().getPublishTime();
}

double MonitorSubsystem::getReceiveTime() const
{
    return monitorPointSet_.getSubsystemFrame().getReceiveTime();
}

string MonitorSubsystem::makeName(const string& name, int number)
{
    ostringstream o;
    o << name << number;
    return o.str();
}

void
MonitorSubsystem::updateFrameAverage( )
{
    ScratchAverages scratchAvgs;
    
    MonitorPointIterator mpi(*this);
    while (mpi++) {
        MonitorPoint& mp = mpi.getMonitorPoint();
        mp.updateFrameAverage( scratchAvgs );
    }    
}

int MonitorSubsystem::maxMonitorPoints() const
{
    return maxMonitorPoints_;
}


int MonitorSubsystem::maxSamples() const
{
    return maxSamples_;
}

double MonitorSubsystem::getLastWriterDelay() const
{
    return monitorPointSet_.getSubsystemFrame().getLastWriterDelay();
}

double MonitorSubsystem::getLastWriteTime() const
{
    return monitorPointSet_.getSubsystemFrame().getLastWriteTime();
}

double MonitorSubsystem::getScriberWriteDelay() const
{
    return monitorPointSet_.getSubsystemFrame().getScriberWriteDelay();
}

double MonitorSubsystem::getScriberWriteTime() const
{
    return monitorPointSet_.getSubsystemFrame().getScriberWriteTime();
}

void MonitorSubsystem::resetTimes() 
{
    return monitorPointSet_.getSubsystemFrame().clearAllTimes();
}

void MonitorSubsystem::startAutoWriter(double delay) const
{
    monitorPointSet_.startAutoWriter(delay);
}

void MonitorSubsystem::stopAutoWriter() const
{
    monitorPointSet_.stopAutoWriter();
}

bool MonitorSubsystem::autoWriterIsAlive() const
{
    return monitorPointSet_.autoWriterIsAlive();
}

double MonitorSubsystem::getStartWriteTime() const
{
    return monitorPointSet_.getStartWriteTime();
}

double MonitorSubsystem::getEndWriteTime() const
{
    return monitorPointSet_.getEndWriteTime();
}
 
double MonitorSubsystem::getStartScriberWriteTime() const
{
    return monitorPointSet_.getStartScriberWriteTime();
}
 
MonitorPointSet& MonitorSubsystem::monitorPointSet() 
{ 
    return monitorPointSet_; 
}

    
int MonitorSubsystem::getFrameCount() const
{
    return monitorPointSet_.getSubsystemFrame().getFrameCount();
}

void MonitorSubsystem::setMpTagIds( dbms::TagIDAuthority & authority )
{
    vector< MonitorPoint * > notFound;
    notFound.reserve( maxMonitorPoints() );

    // First handle the ones that already in our maps
    {
        MonitorPointIterator mpi( *this );

        while ( ++mpi ) {
            MonitorPoint & mp = mpi.getMonitorPoint();

            dbms::TagIDAuthority::NameMapInfo 
                nameInfo( 0, dbms::TagIDAuthority::ON_THE_FLY_ORIGIN );

            const bool found = authority.retrieveTagInfo( mp.getCanonicalName(),
                                                          nameInfo );

            if ( found ) {
                mp.setTagID( 
                    nameInfo.tagId_,
                    (nameInfo.origin_ == dbms::TagIDAuthority::ON_THE_FLY_ORIGIN) );
            } else {
                notFound.push_back( &mp );
            }
        }
    }

    // Now patch up the ones that weren't found in our maps
    if ( notFound.empty() != true ) {
        const string subsysName = getName();

        vector< MonitorPoint * >::const_iterator i = notFound.begin();
        const vector< MonitorPoint * >::const_iterator iEnd = notFound.end();

        for ( ; i != iEnd; ++i ) {
            MonitorPoint * const mp = *i;

            bool assignedOTF = false;

            const tagIDType tagId =
                authority.findIdOrAssignOtf( mp->getCanonicalName(),
                                             assignedOTF,
                                             subsysName );
            mp->setTagID( tagId, assignedOTF );
        }
    }
}

