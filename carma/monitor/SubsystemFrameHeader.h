#ifndef CARMA_MONITOR_SUBSYSTEMFRAMEHEADER_H
#define CARMA_MONITOR_SUBSYSTEMFRAMEHEADER_H

namespace carma {
namespace monitor {

/**
 * Structure used to manage memory for a subsystem frame. Contains
 * bookkeeping information and the monitor data for a monitor subsystem.
 */
struct SubsystemHeader {
    unsigned short        subsystemID;  // first header field
    unsigned short        statusFlags;
    int                   numMonitorPoints;
    int                   numSingleSamplePoints;
    int                   numSamples;
    int                   numActualSamples;
    int                   maxMonitorPoints; // maximum # of monitor points
    int                   maxSamples; // maximum number of monitor samples
    int                   frameCount;   // timestamp for current subsystem frame
    double                lastWriterDelay;
                           // delay and write time for the last client to write
                           // monitor point values to the Scriber; if 
                           // delay is -1, or zero, then write was manual, 
                           // not using autoWriter
    double                lastWriteTime;
    double                scriberWriteDelay;
    double                scriberWriteTime;
    double                publishTime;
    double                receiveTime;       // final header field

    int obsolete1;
    int obsolete2;
    int obsolete3;
}; // 92 bytes


struct SubsystemDataPointers {
    public:
        explicit SubsystemDataPointers( );
        explicit SubsystemDataPointers( SubsystemHeader & frame );

        /* virtual */ ~SubsystemDataPointers();

        void swap( SubsystemDataPointers & rhs );

        bool operator==( const SubsystemDataPointers & rhs ) const;
        bool operator!=( const SubsystemDataPointers & rhs ) const;

        void syncSubsystemDataPointersToNewFrameData( SubsystemHeader & frame );

        // maxMonitorPoints number of indices
        int *       writableMonitorPointIndex;
        const int * monitorPointIndex;
        
        // maxMonitorPoints number of headers
        MonitorHeader *       writableMonitorHeaders;
        const MonitorHeader * monitorHeaders;

        // maxSamples number of monitor samples
        MonitorSampleValue *       writableMonitorValues;
        const MonitorSampleValue * monitorValues;
};


}  // namespace carma::monitor
}  // namespace carma


inline
carma::monitor::SubsystemDataPointers::SubsystemDataPointers( ) :
writableMonitorPointIndex( 0 ),
monitorPointIndex( 0 ),
writableMonitorHeaders( 0 ),
monitorHeaders( 0 ),
writableMonitorValues( 0 ),
monitorValues( 0 )
{
}


inline
carma::monitor::SubsystemDataPointers::~SubsystemDataPointers( )
{
    writableMonitorPointIndex = 0;
    monitorPointIndex = 0;
    writableMonitorHeaders = 0;
    monitorHeaders = 0;
    writableMonitorValues = 0;
    monitorValues = 0;
}


inline void
carma::monitor::SubsystemDataPointers::swap( SubsystemDataPointers & rhs )
{
    ::std::swap( writableMonitorPointIndex, rhs.writableMonitorPointIndex );
    ::std::swap( monitorPointIndex, rhs.monitorPointIndex );
    ::std::swap( writableMonitorHeaders, rhs.writableMonitorHeaders );
    ::std::swap( monitorHeaders, rhs.monitorHeaders );
    ::std::swap( writableMonitorValues, rhs.writableMonitorValues );
    ::std::swap( monitorValues, rhs.monitorValues );
}


inline bool
carma::monitor::SubsystemDataPointers::operator==(
    const SubsystemDataPointers & rhs ) const
{
    return ((writableMonitorPointIndex == rhs.writableMonitorPointIndex) &&
            (monitorPointIndex == rhs.monitorPointIndex) &&
            (writableMonitorHeaders == rhs.writableMonitorHeaders) &&
            (monitorHeaders == rhs.monitorHeaders) &&
            (writableMonitorValues == rhs.writableMonitorValues) &&
            (monitorValues == rhs.monitorValues));
}


inline bool
carma::monitor::SubsystemDataPointers::operator!=(
    const SubsystemDataPointers & rhs ) const
{
    return (operator==( rhs ) == false);
}


#endif
