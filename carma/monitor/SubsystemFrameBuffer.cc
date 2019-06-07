#include "carma/monitor/SubsystemFrameBuffer.h"

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SubsystemFrameHeader.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IPQbuffer.h"
#include "carma/util/programLogging.h"

#include <iostream>
#include <sstream>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;



namespace {

const unsigned short FRAME_SAMPLES_COMPLETE = 0x80;

const string kSubsystemNamePrefix = "crmsubsys_";

}  // namespace < anonymous >


const int SubsystemFrameBuffer::kDefaultQueueDepth = 8;


class SubsystemFrameBuffer::InternalIpq : public IPQbuffer {
    public:
        InternalIpq( SubsystemHeader & frame,
                     size_t            frameSize,
                     const string &    fname,
                     bool              isCreator,
                     int               queueDepth );
                     
        void write( );
};


SubsystemFrameBuffer::InternalIpq::InternalIpq(
    SubsystemHeader & frame,
    const size_t      frameSize,
    const string &    fname,
    const bool        isCreator,
    const int         queueDepth ) :
IPQbuffer( &frame,
           frameSize,
           fname,
           isCreator,
           queueDepth ) {
    init();
}


SubsystemFrameBuffer::SubsystemFrameBuffer(
    const ushort            subsystemID, 
    const string &          fname, 
    const long              maxNumMonitorPoints,
    const long              maxNumSamples,
    const bool              isCreator,
    const int               queueDepth ) :
SubsystemFrame( subsystemID,
                maxNumMonitorPoints,
                maxNumSamples,
                true ),  // BULLSHIT: We leak the frame storage for now!
ipq_()
{
    const size_t frameSizeInBytes =
        SubsystemFrame::sizeFrame( maxNumMonitorPoints, maxNumSamples );

    ipq_ =
        auto_ptr< InternalIpq>( 
            new InternalIpq( frame_,
                             frameSizeInBytes,
                             fname,
                             isCreator,
                             queueDepth ) );

    setSubsystemID( subsystemID );
    setFrameCount( 0 );
    
    ipq_->setNoneAvailable();
}


SubsystemFrameBuffer::~SubsystemFrameBuffer () 
try { 
    //cout << "SubsystemFrameBuffer d'tor" << endl;
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


SubsystemFrameBuffer &
SubsystemFrameBuffer::getSubsystemFrameBuffer( const long subsystemID,
                                               const long maxNumMonitorPoints,
                                               const long maxNumSamples )
{
    if ( subsystemID == 0 )
        throw CARMA_ERROR( "Frame buffer for subsystemID == 0 does not exist" );

    string fname;
    {
        ostringstream oss;
        
        oss << kSubsystemNamePrefix << subsystemID;
        
        fname = oss.str();
    }
    
    SubsystemFrameBuffer * const frameBuffer =
        new SubsystemFrameBuffer( subsystemID,
                                  fname, 
                                  maxNumMonitorPoints,
                                  maxNumSamples,
                                  true,
                                  kDefaultQueueDepth );

    const ushort actualSubsystemID = frameBuffer->getSubsystemID();
    
    if ( actualSubsystemID != subsystemID ) {
        ostringstream oss;
        
        oss << "Actual subsystem ID " << actualSubsystemID
            << " does not match requested subsystem ID " << subsystemID;
            
        programLogErrorIfPossible( oss.str() );
    }

    return *frameBuffer;
}


void
SubsystemFrameBuffer::InternalIpq::write( ) {
    IPQbuffer::write( );
}


void  
SubsystemFrameBuffer::write (bool force)    // update to shared memeory
{  
    ipq_->write(); 
}


unsigned int
SubsystemFrameBuffer::read( )
{  
    const unsigned int result = ipq_->read();
    
    syncSubsystemToNewFrameData( );

    return result;
}


bool
SubsystemFrameBuffer::readNewest( )
{  
    const bool readSuccessful = ipq_->readNewest();    
    syncSubsystemToNewFrameData( );
    return readSuccessful;
}  

bool
SubsystemFrameBuffer::readNewestConditionalCopy( )
{  
    const bool copy = ipq_->readNewestConditionalCopy();    
    syncSubsystemToNewFrameData( );
    return copy;
}  


WriteStatus        
SubsystemFrameBuffer::writeSampleValue (const int index, const MonitorPointSample& value)
{
    if (! (index >= 0  &&  index < getNumMonitorPoints()))
        return MONITOR_NOT_WRITTEN;

    ushort numSamplesPerCycle;
    ushort sampleOffset;
    {
        const MonitorHeader & monitorHeader =
            getMonitorHeaderRefByIndex( index );
            
        numSamplesPerCycle = monitorHeader.getNumSamplesPerCycle();
        sampleOffset = monitorHeader.getSampleOffset();
    }
    
    MonitorPointSample 
        sample( dataPointers_.writableMonitorValues[
            sampleOffset + value.getSampleNumber()
        ]);

    // we need to protect all writes from timer so integrity of data is preserved
    // TimerBlock();

    // block of operations protected from timer

    // compute sum for average (take only valid samples)
    // keep track of # of valid samples in dummy field os 
    // average sample
    sample.allocate (index, value.getSampleNumber());
    SubsystemFrame::writeSampleValue (
	    index, value.getSample(), value.getSampleNumber()
	    );
    isDirty_ = true;
    clearPublished();
    // now unblock timer signals
    // TimerUnblock();

    return MONITOR_WRITTEN_ONCE ;
}



WriteStatus        
SubsystemFrameBuffer::writeSampleValue (const tagIDType tagID, const MonitorPointSample& value)
{
    int index = getIndex (tagID);

    if (index < 0)  
        throw IllegalMonitorPointExceptionObj (tagID, getSubsystemID(), __FILE__, __LINE__);

    // if (!validSampleValue (tagID, value)) 
    //        throw SampleInvalidExceptionObj (value, __FILE__, __LINE__);

    if ( index == MONITOR_POINT_ABSENT ) {
        // Log it?
    }

    return writeSampleValue (index, value);
}

bool        
SubsystemFrameBuffer::isDirty( ) const
{  
    return isDirty_;  
}


bool        
SubsystemFrameBuffer::isComplete( ) const
{  
    return (numCleanSamples_ == 0);  
}


void        
SubsystemFrameBuffer::clearFrame()  
{
    isDirty_ = false;
    numCleanSamples_ = getNumMonitorSamples();
    clearSamples();  
}


void        
SubsystemFrameBuffer::decrementCleanSamples( ) 
{  
   --numCleanSamples_;
   
    if ( numCleanSamples_ == 0 )
       SetFlag( frame_.statusFlags, FRAME_SAMPLES_COMPLETE );
}
