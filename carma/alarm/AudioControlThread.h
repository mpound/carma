
/*
 * $Id: AudioControlThread.h,v 1.6 2012/01/20 17:04:50 iws Exp $
 */

#ifndef CARMA_ALARM_AUDIOCONTROLTHREAD_H
#define CARMA_ALARM_AUDIOCONTROLTHREAD_H

#include <string>
#include <canberra.h>

#include <carma/alarm/SoundsTable.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/ConcurrentQueue.h>

namespace carma {
namespace alarm {

    class AudioControlThread2
    {
      public:
        /* constructor / destructor */
        AudioControlThread2(const std::string &devName, const bool emulate, const std::string &soundsTab);
        ~AudioControlThread2();

        /* change various properties */
        void setState(const bool alarmOn);
        void setSound(const std::string &newSound);
        void setRepeat(const bool repeat);

        /* thread entry point */
        static void audioThreadEP(AudioControlThread2 &This);
        void exitThread();

        /* libcanberra audio state machine */
        void alarmStart();
        void alarmStop();
        void alarmCallback();

      private:

        /* Alarm Sounds Configuration */
        SoundsTable table;

        /* libcanberra audio control */
        ca_context *ctx;
        ca_proplist *prop;

        /* constants from the constructor */
        const std::string deviceName;
        const bool emulate;

        /* protection for members */
        mutable carma::util::PthreadMutex mutex;

        /* audio control */
        std::string sound;
        bool repeat;

        carma::util::ConcurrentQueue<int> queue_;
    };

} // namespace alarm
} // namespace carma

#endif // CARMA_ALARM_AUDIOCONTROLTHREAD_H
