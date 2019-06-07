#ifndef clocksync_h
#define clocksync_h

/*
 * The UDP port on which the SZA clocksync daemon listens for time updates.
 */
#define SZA_TIME_PORT 5435

/*
 * The following parameter tells the tracker how often to send time
 * updates.
 */
#define TIME_UPDATE_INTERVAL 10  /* (integer seconds) */

/*
 * The following parameter tells the clocksync program how many time
 * updates to accumulate before computing a median clock offset and
 * using it to adjust the clock. The time between clock updates will
 * thus be (TIME_UPDATE_INTERVAL * TIME_UPDATE_COUNT) seconds.  Since
 * the clock doesn't drift very quickly, the time between clock
 * updates should probably be around an hour. Note that this sets the
 * size of an array that is sorted to determine the median offset, so
 * beware of making it too large.
 */
#define TIME_UPDATE_COUNT 360

/*
 * If the time offset computed from an individual time update, is
 * greater than the following number of seconds, reject it.
 */
#define MAX_CLOCK_OFFSET 600   /* Integer seconds */

/*
 * Specify the length of time that the tracker task should wait for
 * the time code reader to phase-lock to the GPS clock before sending
 * its first time update.
 */
#define TIME_START_DELAY 180   /* Integer seconds */

#endif
