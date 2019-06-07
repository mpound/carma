/**
 * @file
 * Frame writer that incorporates a timer thread, used for writing
 * subsystem and system monitor frames.
 *
 * Author: N. S. Amarnath
 * 
 */

#ifndef CARMA_MONITOR_FRAME_IPQ_WRITER_H
#define CARMA_MONITOR_FRAME_IPQ_WRITER_H

#include "carma/util/FrameAlignedTimer.h"

namespace carma {
namespace monitor {


extern const long HALF_SECOND_IN_NANOSECONDS;


class FrameIPQwriter {
    public:

        /** 
         * Constructor.
         * When a delay greater than 0.5 seconds is used, the first fire time
         * is set to the full delay time.  After that the delay is mod 500ms
         * for each frame.
         */
        FrameIPQwriter(
            const ::std::string &    ipqWriterThreadName,
            double                   delayInS );
        
        virtual ~FrameIPQwriter( );
        
        /**
         * Functor definition for threading.
         * This gets passed in to the boost::thread constructor.
         */
        void operator()();
        
    protected:
    
        /**
        * Returns next absolute time when timer will fire as an MJD.
        *
        * @return double MJD - time of next firing.
        */
        double getNextFireTime( ) const;

        /**
         * Resets next fire time.
         *
         * @return double MJD - time of next firing.
         */
        double resetNextFireTime( long delayFrames );
        
    private:
        
        virtual void writeBuffer() = 0;
        
        struct ::timespec nextTime_;
        
        /**
        * FrameAligned Timer instance used to get timer pulse
        */
        util::FrameAlignedTimer timer_;
        
        size_t writeCount_;
        size_t nextWriteLogCount_;
};


} // End namespace carma::monitor
} // End namespace carma

#endif
