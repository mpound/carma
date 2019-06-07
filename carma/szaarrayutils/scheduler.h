#ifndef scheduler_h
#define scheduler_h

#include "astrom.h"
#include "hash.h"

#include "carma/szaarrayutils/control.h"

/*
 * Warning: The facilities in this file are only for use by functions
 * called by the scheduler thread. They can not be safely called by
 * other threads. See szacontrol.h for thread-safe scheduler facitities.
 */

/*
 * Compile a script.
 */
Script *sch_compile_schedule(Scheduler *sch, char *dir, char *filename,
			     InputStream *arguments);
/*
 * Queue a schedule for subsequent execution.
 */
int sch_queue_schedule(Scheduler *sch, Script *sc);

/*
 * Abort the currently running schedule.
 */
int sch_abort_schedule(Scheduler *sch);

/*
 * Temporarily suspend execution of the currently running schedule.
 */
int sch_suspend_schedule(Scheduler *sch);

/*
 * Resume execution of the currently running schedule, assuming that
 * it has previously been suspended.
 */
int sch_resume_schedule(Scheduler *sch);

/*
 * Move the schedule at position 'n' in the schedule queue to
 * queue position n+dn.
 */
int sch_move_schedule(Scheduler *sch, unsigned n, int dn);

/*
 * Remove and discard the schedule at position 'n' in the
 * schedule queue.
 */
int sch_remove_schedule(Scheduler *sch, unsigned n);


/*
 * When a schedule is no longer needed, call this function to discard
 * it. When actually be returned to the schedule pool until all users have
 * finished with it.
 */
Script *sch_discard_schedule(Scheduler *sch, Script *sc);

/*
 * When a script pointer is newly recorded for subsequent use, the following
 * function should be called to make sure that another user doesn't
 * delete it when calling sch_discard_script(). It increments a reference
 * count which later gets decremented by sch_discard_script().
 */
Script *sch_reference_schedule(Script *sc);

/*
 * Specify a schedule to be executed whenever the real-time controller
 * is restarted.
 */
int sch_change_init_script(Scheduler *sch, Script *sc);

/*
 * Get a readonly alias to the site configuration object.
 */
sza::array::Site *sch_Site(Scheduler *sch);

/*
 * The following calls allow a schedule to wait until an interactive
 * user send a trigger command. The interactive user calls a command
 * that invokes sch_add_trigger(), while the schedule polls on
 * calls to sch_read_trigger() until the trigger count is non-zero.
 * Note that each call to sch_add_trigger() increments a trigger counter,
 * whereas each call to sch_read_trigger() decrements this counter except
 * when it is zero.
 */
int sch_add_trigger(Scheduler *sch);
int sch_read_trigger(Scheduler *sch);

// The following command tells the real-time controller that the
// initialization script has ended.

int sch_init_ended_send(Scheduler *sch);

// The following functions allow one to command new phase-shifter
// positions, then check on their progress.  Both sch_pshifter_send
// and sch_polwalsh_send will generate a NET_PHASE_DONE message in
// response.

int sch_pshifter_send(Scheduler *sch, unsigned antennas, int half,
		      unsigned posn);
int sch_polwalsh_send(Scheduler *sch, unsigned antennas, int half,
		      unsigned walshstep);
/*
 * The following functions allow one to command new frame from the frame grabber
 * then check on it's reception status by the grabber thread
 */
int sch_grab_send(Scheduler *sch);
unsigned int sch_grab_done(Scheduler *sch);
/*
 * The following functions allow one to send long-duration commands
 * to the channelizer, and check on their combined completion status.
 */
int sch_chzr_send_power(Scheduler *sch, unsigned bands, unsigned antennas,
			double power);
int sch_chzr_send_zero(Scheduler *sch, unsigned bands, unsigned antennas);
unsigned int sch_chzr_done(Scheduler *sch);

/*
 * CalTert positioning commands require a sequence number. This should
 * be obtained from the return value of sch_next_caltert_seq().  The
 * sch_caltert_done() function returns a bitmask in which the bit for
 * the corresponding antennas will be non-zero when the caltert
 * command for which a sequence number was obtained has completed.
 */
unsigned sch_next_caltert_seq(Script* sc, Scheduler *sch);
unsigned sch_caltert_done(Scheduler *sch);

/*
 * The newFrame command requires a sequence number
 */
unsigned sch_next_frame_seq(Script* sc, Scheduler *sch);
unsigned sch_frame_done(Scheduler *sch);

/**
 * Noise source commands require a sequence number
 */
unsigned sch_next_noise_seq(Script* sc, Scheduler *sch);
unsigned sch_noise_done(Scheduler *sch);

/**
 * Some antenna CAN commands require a sequence number
 */
unsigned sch_next_can_seq(Script* sc, Scheduler *sch);
unsigned sch_can_done(Scheduler *sch);

/**
 * Some antenna IF commands require a sequence number
 */
unsigned sch_next_IFMod_seq(Script* sc, Scheduler *sch);
unsigned sch_IFMod_done(Scheduler *sch);

/*
 * Many tracker commands require a sequence number. This should be
 * obtained from the return value of sch_next_pmac_seq().
 * The sch_pmac_done() function returns non-zero whenever the last
 * tracker command for which a sequence number was obtained has
 * completed.
 */
unsigned sch_next_pmac_seq(Script* sc, Scheduler *sch, unsigned antennas);
unsigned sch_pmac_done(Scheduler *sch);

/*
 * The sch_tv_offset_done() function returns non-zero whenever the last
 * tracker command for which a sequence number was obtained has
 * completed.
 */
unsigned sch_next_tv_offset_seq(Script* sc, Scheduler *sch);
unsigned int sch_tv_offset_done(Scheduler *sch);

/*
 * When a mark command is sent to the scanner, it is accompanied by
 * a uniq sequence number, returned by sch_next_pmac_seq(). When
 * the effects of this command have been seen by the archiver and
 * recorded in the archive, the archiver tells the scheduler that
 * the transaction has been completed, and thereafter, unless another
 * mark command has been issued, sch_pmac_done() returns true.
 */
unsigned sch_next_mark_seq(Script* sc, Scheduler *sch);
unsigned int sch_mark_done(Scheduler *sch);

/*
 * When a setreg command is sent to the probe, it is accompanied by
 * a uniq sequence number, returned by sch_next_setreg_seq(), and
 * forwarded to the scanner. When the effects of this command have been seen 
 * by the archiver and recorded in the archive, the archiver tells the 
 * scheduler that the transaction has been completed, and thereafter, unless 
 * another setreg command has been issued, sch_setreg_done() returns true.
 */
unsigned sch_next_setreg_seq(Script* sc, Scheduler *sch);
unsigned int sch_setreg_done(Scheduler *sch);

/*
 * When a tv_offset command is issued, it is accompanied by a unique
 * sequence number, returned by sch_next_offset_seq(), and forwarded
 * to the tracker. When the effects of this command have been seen by
 * the archiver and recorded in the archive, the archiver tells the
 * scheduler that the transaction has been completed, and thereafter,
 * unless another setreg command has been issued, sch_offset_done()
 * returns true.  */
unsigned sch_next_offset_seq(Script* sc, Scheduler *sch);
unsigned int sch_offset_done(Scheduler *sch);
/*
 * When a grab command is sent to the scanner, it is accompanied by
 * a uniq sequence number, returned by sch_next_grab_seq(). When
 * the effects of this command have been seen by the grabber
 * the transaction has been completed, and thereafter, unless another
 * mark command has been issued, sch_grab_done() returns true.
 */
unsigned sch_next_grab_seq(Script* sc, Scheduler *sch);
unsigned int sch_grab_done(Scheduler *sch);

/*
 * Set/get the default elevation of the local horizon.
 */
int sch_set_horizon(Scheduler *sch, double angle);
double sch_get_horizon(Scheduler *sch);

/* Add to the list of signal names recognized by the scheduler. */

Symbol *sch_add_signal(Scheduler *sch, char *name);

/* Send a signal to the currently running schedule */

int sch_signal_schedule(Scheduler *sch, Symbol *sig);

/* Lookup the symbol table entry of a given signal */

Symbol *sch_lookup_signal(Scheduler *sch, char *name);

/*
 * In order for szascript to be able to implement functions that
 * query the last values that were given to pertinent configuration
 * parameters, the scheduler uses a container of the following type
 * to cache the last values of these parameters.
 */
typedef struct {
  struct {             /* Archive command parameters */
    unsigned combine;  /* The last value of "archive combine=" */
    unsigned filter;   /* True if archive filtering is turned on */
  } archive;
} SchedCache;

SchedCache *sch_sched_cache(Scheduler *sch);

int pack_scheduler_auto_dir(SchedulerMessage *msg, char *dir);
int pack_scheduler_auto_poll(SchedulerMessage *msg, long ms);
int pack_scheduler_auto_state(SchedulerMessage *msg, int on);

int sch_send_paging_state(Scheduler *sch, int allow, 
			  ListNode *client = 0, 
			  unsigned mask = sza::array::PAGE_ENABLE, 
			  char* msg=0);

int sch_send_antenna_selection(Scheduler *sch, ListNode *client);

int sch_sendPagerCondition(Scheduler *sch, ListNode *client, 
			   unsigned mode, sza::util::PagerMonitor::RegSpec* regSpec);

int sch_sendCmdTimeoutConfiguration(Scheduler *sch, ListNode *client, 
				    unsigned seconds);

int sch_sendCmdTimeoutConfiguration(Scheduler *sch, ListNode *client, 
				    bool enable);

int sch_sendArrayConfiguration(Scheduler* sch, ListNode* node,
			       unsigned mode, unsigned array, unsigned config);

int sch_sendAddArrayAntenna(Scheduler* sch, ListNode* client, 
			    unsigned array, unsigned iPad, 
			    unsigned antType, int iAnt);

int sch_sendRemArrayAntenna(Scheduler* sch, ListNode* client, 
			    unsigned array, unsigned iPad);

#endif

