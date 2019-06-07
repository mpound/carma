#ifndef CARMA_MONITOR_MONITORPOINTUPDATESERVANT_H
#define CARMA_MONITOR_MONITORPOINTUPDATESERVANT_H

#include "carma/util/PthreadMutex.h"
#include "carma/monitor/monitorframe_skel.h"

#include <boost/thread/mutex.hpp>
#include <tao/Basic_Types.h>


namespace carma {
namespace monitor {

class SubsystemFrameBuffer;

/**
 * @class MonitorPointUpdateServant
 * @brief class that represents the scriber part of the frameScriberPublisher.
 * Collects the data that the scriber will write to local IPQ.
 */
class MonitorPointUpdateServant {
public:

    /**
     * Constructor
     * @param buffer SubsystemFrameBuffer& reference to a subsystem frame
     *        buffer (IPQ) associated with this monitor subsystem.
     * @param  bufferWriteGuard Mutex to synchronize thread writes to buffer.
     */
    explicit MonitorPointUpdateServant( 
        SubsystemFrameBuffer & buffer,
        boost::mutex & bufferWriteMutex );

    /**
     * Destructor
     * Stops thread and deactivates root POA.
     */
    virtual ~MonitorPointUpdateServant();

    /**
     * Static method that returns DO name based on associated subsystemID.
     *
     * @param subsystemID unsigned short, represents ID of associated
     *        monitor subsystem.
     * @return std::string name of DO associated with DO reference in
     *         CORBA NameService.
     */
    static std::string makeName (ushort subsystemID);

    //
    // IDL:mmarray.org/carma/monitor/MonitorPointUpdate/monitorPointSeqUpdate:1.0
    //
    /**
     * DO method to write modified monitor points. Used by host programs in
     * associated monitor subsystem.
     *
     * @param dataSeq MontitorSampleValueSeq& reference to sequence of
     *        transported monitor point samples containg modified values.
     * @param frameTime CORBA::Long frameCount (timestamp) associated with
     *        sequence of modified values.
     * @param writeDelay CORBA::Double delay of auto writer thread writing
     *        to this DO represented in seconds as seconds after
     *        half-second tick.
     * @return CORBA::Double returns time that method received dataSeq
     *         as an MJD.
     */
    virtual CORBA::Double
    monitorPointSampleUpdate(
        const MonitorSampleValues        & values,
        CORBA::Long                        frameTime,
        CORBA::Double                      writeDelay );

  private:

    // No copying
    MonitorPointUpdateServant( const MonitorPointUpdateServant & rhs );
    MonitorPointUpdateServant & operator=( const MonitorPointUpdateServant & rhs );

    SubsystemFrameBuffer &  frameBuffer_;
    boost::mutex & frameBufferMutex_;

}; // End class MonitorPointUpdateServant


}  // namespace carma::monitor
}  // namespace carma

#endif
