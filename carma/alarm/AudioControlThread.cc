
/*
 * $Id: AudioControlThread.cc,v 1.10 2012/06/05 23:06:22 abeard Exp $
 */

#include <iostream>
#include <stdexcept>
using namespace std;

#include <canberra.h>

#include <carma/util/loggingUtils.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/FileUtils.h>
#include <carma/util/Trace.h>
#include <carma/util/StartPthread.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/ScopedPthreadMutexLock.h>
using namespace carma::util;

#include <carma/alarm/Trace.h>
#include <carma/alarm/AudioControlThread.h>
#include <carma/alarm/SoundsTable.h>
using namespace carma::alarm;

static const int ALARM_START = 10;
static const int ALARM_STOP  = 20;
static const int ALARM_EXIT  = 30;

static const int ALARM_MEDIA_ID = 1;

/*----------------------------------------------------------------------------*/
/* The AudioControlThread2 Object                                             */
/*----------------------------------------------------------------------------*/

AudioControlThread2::AudioControlThread2(const std::string &devName,
                                         const bool emulate,
                                         const std::string &soundsTab)
    : table(soundsTab)
    , ctx(NULL)
    , prop(NULL)
    , deviceName(devName)
    , emulate(emulate)
    , mutex()
    , sound("alarm")
    , repeat(true)
    , queue_()
{
    std::string driver;
    int ret;

    /*
     * Create and setup the libcanberra audio context
     */

    // create the context
    ret = ca_context_create(&this->ctx);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to create context: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // set some useful properties
    ret = ca_context_change_props(this->ctx,
                                  CA_PROP_APPLICATION_NAME, "carmaAlarm",
                                  CA_PROP_APPLICATION_ID, "pvt.carma.alarm",
                                  NULL);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to change properties: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // choose which sound driver to use: null or alsa
    driver = this->emulate ? "null" : "alsa";
    programLogInfoIfPossible("using sound driver: " + driver);

    ret = ca_context_set_driver(this->ctx, driver.c_str());
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to set sound driver " << driver << ": "
            << ca_strerror(ret);

        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // open the sound device
    ret = ca_context_open(this->ctx);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to open sound device: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    /*
     * Lookup the default sound file name
     */
    const std::string soundFile = this->table.getSoundFullPathFileByName(this->sound);

    /*
     * Create and setup the libcanberra sound property
     */

    // allocate the property
    ret = ca_proplist_create(&this->prop);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to create proplist: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    // setup necessary properties
    ca_proplist_sets(this->prop, CA_PROP_EVENT_ID, "CARMA Alarm");
    ca_proplist_sets(this->prop, CA_PROP_MEDIA_FILENAME, soundFile.c_str());
    ca_proplist_sets(this->prop, CA_PROP_MEDIA_NAME, "CARMA Alarm");
    ca_proplist_sets(this->prop, CA_PROP_MEDIA_LANGUAGE, "en_EN");
}

AudioControlThread2::~AudioControlThread2()
{
    int ret;

    // close the sound device
    ret = ca_context_destroy(this->ctx);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to destroy context: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
    }

    // destroy the sound property
    ret = ca_proplist_destroy(this->prop);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to destroy proplist: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
    }
}

/*
 * Change the alarm state
 *
 * This turns the alarm on or off. It can be called multiple times
 * with the same state, and any currently playing audio will not
 * be effected. (No audio skips will happen).
 */
void AudioControlThread2::setState(const bool alarmOn)
{
    const ScopedPthreadMutexLock lock(this->mutex);

    /* push the new state into the queue */
    int state = alarmOn ? ALARM_START : ALARM_STOP;
    this->queue_.push(state);
}

/*
 * Change the sound to play
 *
 * This will update the libcanberra sound property. If the alarm is sounding
 * in repeat mode, the sound will change on the next repeat. The code makes
 * no attempt to stop a currently playing sound on sound change.
 */
void AudioControlThread2::setSound(const std::string &newSound)
{
    const ScopedPthreadMutexLock lock(this->mutex);
    std::string soundFile;
    bool exists = false;

    /* look up the requested sound file */
    soundFile = this->table.getSoundFullPathFileByName(newSound);

    /* check if the sound file actually exists */
    exists = FileUtils::exists(soundFile);
    if (!exists) {
        std::ostringstream oss;

        oss << "sound file does not exist: " << soundFile;
        programLogErrorIfPossible(oss.str());

        /* try and lookup the default sound */
        soundFile = this->table.getSoundFullPathFileByName("default");
    }

    /* log the sound file we are actually attempting to play (if different) */
    if ( soundFile != this->sound ) {
        std::ostringstream oss;
        oss << "using sound file: " << soundFile;
        programLogInfoIfPossible(oss.str());
    }

    this->sound = soundFile;
    ca_proplist_sets(this->prop, CA_PROP_MEDIA_FILENAME, soundFile.c_str());
}

/*
 * Change the repeat mode
 *
 * If a sound is currently playing and you set repeat mode, it will repeat
 * forever until it is explicitly told to stop.
 */
void AudioControlThread2::setRepeat(const bool repeat)
{
    const ScopedPthreadMutexLock lock(this->mutex);
    this->repeat = repeat;
}

void AudioControlThread2::audioThreadEP(AudioControlThread2 &This)
{
    /* run the mainloop forever */
    while (true) {
        try {

            /* wait for a state change to come through the queue */
            int state = ALARM_EXIT;
            This.queue_.wait_and_pop(state);

            switch (state) {
            case ALARM_START:
                This.alarmStart();
                break;
            case ALARM_STOP:
                This.alarmStop();
                break;
            case ALARM_EXIT:
                programLogDebugIfPossible("audio control thread quit requested");
                return;
            default:
                programLogErrorIfPossible("unknown state reached");
                break;
            }

        } catch (...) {
            std::ostringstream oss;

            oss << "ERROR: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());
        }
    }
}

void AudioControlThread2::exitThread()
{
    const ScopedPthreadMutexLock lock(this->mutex);

    /* push the new state into the queue */
    this->queue_.push(ALARM_EXIT);
}

/*
 * This is a C-compatible callback function
 *
 * It is called when each libcanberra audio playback is finished or canceled.
 * We use it get ourselves back inside the AudioControlThread2 object,
 * where we have access to all members.
 */
static void callback2(ca_context *ctx, uint32_t id, int err, void *data)
{
    AudioControlThread2 *This = static_cast<AudioControlThread2 *>(data);
    const ScopedLogNdc ndc("callback2");

    /* the sound was canceled -- therefore we are stopped */
    if (err == CA_ERROR_CANCELED)
        return;

    /* call the AudioControlThread2 internal function */
    This->alarmCallback();
}

void AudioControlThread2::alarmCallback()
{
    const ScopedPthreadMutexLock lock(this->mutex);
    const ScopedLogNdc ndc("alarmCallback");

    /*
     * The current audio track has finished or been cancelled
     *
     * If we're supposed to be repeating, then we need to tell
     * the audio engine to play the sound again. Otherwise, we
     * don't need to do anything. The audio engine will have
     * already stopped playback.
     */
    if (this->repeat) {
        CPTRACE6("repeat mode: playing the audio file again!");
        this->queue_.push(ALARM_START);
    }
}

void AudioControlThread2::alarmStart()
{
    const ScopedPthreadMutexLock lock(this->mutex);
    const ScopedLogNdc ndc("alarmStart");
    int ret, playing;

    /* check if the sound is still playing */
    ret = ca_context_playing(this->ctx, ALARM_MEDIA_ID, &playing);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to check playing state: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
    }

    /* if the sound is already playing, we don't have anything to do */
    if (playing)
        return;

    /* start the sound playing */
    ret = ca_context_play_full(this->ctx, ALARM_MEDIA_ID, this->prop, callback2, this);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to play sound: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
    }
}

void AudioControlThread2::alarmStop()
{
    const ScopedPthreadMutexLock lock(this->mutex);
    const ScopedLogNdc ndc("alarmStop");
    int ret;

    // cancel the sound, if it is playing
    ret = ca_context_cancel(this->ctx, ALARM_MEDIA_ID);
    if (ret < 0) {
        std::ostringstream oss;
        oss << "unable to cancel sound: " << ca_strerror(ret);
        programLogErrorIfPossible(oss.str());
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
