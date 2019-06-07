/***********************************************************************/
/*
 *  History:
 *    27dec93 jm  Original code.
 *    06jan94 jm  Added include of setups.h and usage of those subs.
 *    24jan94 jm  Added doc section to comments.
 *    08mar94 jm  Changed for getTask/Name modifications in setups.c.
 *    12sep94 jm  Modified doLoop() to return immediately on an ERROR.
 *    20sep94 jm  Modified for changes to calling syntax of Message().
 *    22sep94 jm  Removed modification of 12sep94.
 *    14apr95 jm  Fixed cave/save values to units of minutes.  Added
 *                a check for times that are too extreme (>2 x ave).
 *    20jul95 jm  Rather than ignore extreme time values, I have chosen
 *                to just reduce their impact by taking the mean of the
 *                extreme time value and the average.
 *    20may96 jm  Modified Add/SubTime to reflect that more than one
 *                source may be in the srcsetup.  Added documentation
 *                for thresh and debug keywords.  Changed firstGuess
 *                so it no longer averages the guess over the number
 *                of sources AND'd together and to return a guess as
 *                to the number of sources AND'd together.
 */
/***********************************************************************/
/*= Loop -- Alternate observations between source and calibrator setups. */
/*& jm */
/*: observing */
/*+ */
/*
   Loop is a command designed to simplify the observing process.  It
   calls the scan command repetitively for the user but also checks
   that the stop time is not overrun and that the source is bracketted
   by calibrator setup observations.

   The contents of the setup files for the source and calibrator
   indicate how each object is to be observed.  See help on the 'scan'
   command for further details on how a setup works; see help on the
   command 'setup' for details on how to define a setup.

   The logic of how each setup is observed is simple.  First, the
   calibrator setup is executed and this is followed by the source
   setup.  This process is repeated until time is almost up and then
   a final calibrator setup is executed.  Time is monitored carefully
   to insure that the final calibrator setup is able to be run.  There
   are limitations to this however which might inhibit the final
   calibrator from being observed.

   The loop command returns 0 if executed successfully.  It returns
   a status of 1 if there is an error in the keywords entered or if
   some error state is reached within the program.
 */
/*@ srcsetup
   The name of the source setup.  This can be a simple (single) setup
   or can be a group of setup names separated by logic delimiters.
   The actual contents of the setup are described in the scan command.
   If this keyword is not present, then the calibrator setup will just
   be repeated.  It is an error if both the source AND calibrator
   setups are not provided.
 */
/*@ calsetup
   The name of the calibrator setup.  This can be a simple (single)
   setup or can be a group of setup names separated by logic delimiters.
   The actual contents of the setup are described in the scan command.
   If this keyword is not present, then the source setup will just
   be repeated.  It is an error if both the source AND calibrator
   setups are not provided.
 */
/*@ start
   The earliest time to start looping observations.  If the current
   LST time is earlier than this input time, the program will WAIT
   until that time is reached.  If this keyword is not present, or
   the time is earlier than the current LST, the program will begin
   immediately.
 */
/*@ stop
   The time to stop looping.  The loop command will terminate at
   the earliest of either this input time or the project stop time.
   If a calibrator setup is input, enough time will be reserved to
   insure that the last observation is the calibrator (this is done
   to make sure that the calibrator is run both before and after the
   source setup).  If this keyword is not present, the loop will
   terminate when the project stop time is reached.
 */
/*@ thresh
   The shortest amount of time (in minutes) to spend on the source
   setup.  The source setup (srcsetup) will not be truncated to fewer
   minutes than the value of thresh.  If this keyword is not present,
   then the shorter of: A) the running average of the calibrator setup
   (calsetup); or B) 50% of the running average of the source setup
   (srcsetup) will be used.
 */
/*@ debug
   A non-zero value will provide additional messages which are output
   and may help to debug scripts.
 */
/*--*/

#include "setups.h"

#define MAXAVE     5

int AppDebug = 0;                          /* Initially, no debugging. */

/***********************************************************************/
#ifdef __STDC__
static unsigned long int timeMarker(void)
#else
static unsigned long int timeMarker()
#endif /*__STDC__*/
/*  This returns the current LST time; 0 if the time can not be found. */
{
    if (setCurrentLST())
      return(0);

    return(getCurrentLST());
}

/***********************************************************************/
#ifdef __STDC__
static int checkLoopSrcTime(unsigned long int src, unsigned long int cal, unsigned long int thresh, int nsrcs)
#else
static int checkLoopSrcTime(src, cal, thresh, nsrcs)
unsigned long int src;
unsigned long int cal;
unsigned long int thresh;
int nsrcs;
#endif /*__STDC__*/
/*
 *  This is similar to the checkTime() routine except that it is used
 *  by the loop command to test the source integration time.  As loop
 *  reaches the end of its cycle, it is unlikely that there will be
 *  exactly (src + cal) minutes left.  Either the integration time of
 *  the source will need to be truncated or expanded.  This routine
 *  returns 1 if (a) the LST could not be retrieved; (b) the STOPFLAG
 *  common variable has been set; or (c) the current (LST + cal) is
 *  "past" the assigned stop time.  Otherwise, the routine returns 0.
 *
 *  The threshold specifies (in minutes) the shortest time to spend on
 *  the source scan.  The src and cal arguments specify (in minutes)
 *  the duration of a typical scan on the source and calibrator setups,
 *  respectively.
 */
{
    int count;
    unsigned long int test;

    count = 0;
    test = (2 * cal) + src + thresh;
    if (checkTime(test)) {
      if (checkTime(cal + src) == 0) {
        count = nsrcs;
      } else if (checkTime(cal + thresh) == 0) {
        count = nsrcs;
      } else {
        return(1);
      }
    }
    setLoopSrcTime(count, cal);

    return(0);
}

/***********************************************************************/
#ifdef __STDC__
static long int setupGuess(const char *setup, const char *project)
#else
static long int setupGuess(setup, project)
const char *setup;
const char *project;
#endif /*__STDC__*/
{
    char *task;
    char *begin, *end;
    long int start, stop, setupTime;

    freeVariables();

    if (loadVariables(setup, project, 0) != 0) {
      Warning(stderr, "Trouble accessing setup file [%s].\n", setup);
      return(0);
    }

    task = getTaskName("task");

    if ((begin = getVariable(task, "start")) != (char *)NULL)
      start = getTime(begin, 0);
    else
      start = getCurrentLST();

    stop = start;
    if ((end = getVariable(task, "stop")) != (char *)NULL)
      stop = getTime(end, stop);

    setupTime = stop - start;
    if (setupTime < 0)
      setupTime += FULLDAY;
    if (setupTime >= (FULLDAY / 2))
      setupTime = 0;

    if (AppDebug > 0)
      (void)printf("Setup [%s] evaluates to [%ld] minutes by guesser.\n",
        setup, (setupTime / ONEMINUTE));

    return(setupTime);
}

/***********************************************************************/
#ifdef __STDC__
static long int firstGuess(const char *setup, const char *project, int *nsrcs)
#else
static long int firstGuess(setup, project, nsrcs)
const char *setup;
const char *project;
int *nsrcs;
#endif /*__STDC__*/
/*
 *  This routine makes a first guess at the time it takes to observe
 *  the input setup.  For a simple setup (setup='A'), this is just
 *  the difference of the setup's stop and start times.  For more
 *  complicated arrangements, this will be a function of which setup
 *  groups are observed.  Since this is a first guess and is only
 *  attempted at the beginning of the observation, the method used
 *  here is to assume all sources are up and observable.  Setups OR'd
 *  will contribute their average and AND'd groups will contribute to
 *  the overall sum.  If both the start or stop keywords of a setup
 *  are missing, a value of 0 minutes is used.
 */
{
    char *ptr, *next, *token;
    char op;
    char group[MAXBUF];
    register int j;
    int srcount, count, total;
    long int ave, sum, retTime;

    if (AppDebug > 0)
      (void)printf("Setup [%s] called by firstGuess.\n", setup);

    ave = sum = 0;
    count = total = 0;
    ptr = (char *)setup;
    while ((next = getGroup(ptr, group)) != (char *)NULL) {
      token = group;
      j = strlen(token) - 1;
      if ((*token == '(') && (token[j] == ')')) {
        token[j] = Null;
        token++;
        retTime = firstGuess(token, project, &srcount);
      } else {
        retTime = setupGuess(token, project);
        srcount = 1;
      }
      /*
       *  After skipping leading white space, get the next delimiter
       *  character.  It is an error if the first non-white space
       *  character is not a delimiter and there is still text that
       *  follows.  The pointer "ptr" is set to the next character
       *  after the delimiter (or the incorrect character).
       */
      ptr = skipLeading(next);
      op = Null;
      if (isdelimiter(*ptr)) {
        op = *ptr++;
        ptr = skipLeading(ptr);
      }

      /*
       *  If there is more in the string to process, then a delimiter
       *  must be present.  If not, then an error has occurred.  The
       *  delimiter and the returned state will determine if anymore
       *  processing will take place within this group of setups.
       */
      if (*ptr != Null) {
        switch(op) {
          case ORSYMBOL:           /* Add this to the running average. */
            if (retTime > 0) {
              ave += retTime;
              count++;
            }
            break;
          case ANDSYMBOL:                /* Add this to the grand sum. */
            if (retTime > 0) {
              if (count > 0) {    /* A running average already exists. */
                ave += retTime;
                count++;
                retTime = ave / count;
                ave = count = 0;
              }
              sum += retTime;
              total += srcount;
            }
            break;
          default:
            Warning(stderr, "An operator was expected here [%s]\n", next);
            return(0);
        }
      }
    }

    if (retTime > 0) {
      if (count > 0) {
        ave += retTime;
        count++;
        retTime = ave / count;
      }
      sum += retTime;
      total += srcount;
    }

    if (AppDebug > 0)
      (void)printf("First guess is [%ld] minutes; [%d] sources.\n",
        (sum / ONEMINUTE), total);

    *nsrcs = total;

    return(sum);
}

/***********************************************************************/
#ifdef __STDC__
static STATUS doLoop(const char *srcs, const char *cals, long int threshold,
                     FILE *fd, const char *project)
#else
static STATUS doLoop(srcs, cals, threshold, fd, project)
const char *srcs;
const char *cals;
long int threshold;
FILE *fd;
const char *project;
#endif /*__STDC__*/
{
    char *ssetup, *csetup;
    char sources[MAXBUF];
    char calibs[MAXBUF];
    register int j;
    register int ical, isrc;
    int ncals, nsrcs;
    int calmissed, srcmissed;
    long int ctime[MAXAVE], stime[MAXAVE];
    long int cave, save;
    unsigned long int thresh;
    unsigned long int start, stop, time;
    unsigned long int first, last;
    STATUS calstate, srcstate, retstate;

    /*
     * Make a first guess at stop-start times for both cals and srcs.
     * Fill arrays and averages with value and initialize pointer to 1st.
     */

    retstate = NONE;

    ical = isrc = 0;
    cave = save = 0;

    if (cals != (const char *)NULL) {
      csetup = strcpy(calibs, cals);
      cave = firstGuess(csetup, project, &ncals);
      if (cave <= 0) cave = (30 * ONEMINUTE);
    }
    if (srcs != (const char *)NULL) {
      ssetup = strcpy(sources, srcs);
      save = firstGuess(ssetup, project, &nsrcs);
      if (save <= 0) save = (30 * ONEMINUTE);
    }

    for (j = 0; j < MAXAVE; j++) {
      ctime[j] = cave;
      stime[j] = save;
    }

    /*
     *  If, initially, there is not enough time to do even a single
     *  calibrator (a time test as: (lstnow + cave) > stop fails),
     *  then just drop (ignore the MAIN LOOP) immediately to the
     *  final calibrator (which will be automatically truncated).
     *
     *  MAIN LOOP:
     *  CAL:
     *    If cals exists...
     *    Check time:
     *      If (lstnow + cave) > stop, then goto FINAL CAL:
     *    Set start marker; set missed cal marker to 0; scan cal;
     *    Set stop marker;
     *    If the return status is an error or missed, notify user and
     *      set the missed cal marker to 1.
     *    Replace average element by stop-start, increment index, and
     *      recompute the running calibrator average.
     *
     *  SRC:
     *    If srcs exists...
     *    If a threshold was not entered, use MIN(cave,save/2).
     *    Check time as: (lstnow + 2*cave + save + thresh) > stop:
     *      If that succeeds, do the same steps as CAL: except use src.
     *      If that fails, test time as: (lstnow + cave + save) > stop:
     *        If that succeeds, add (stop - lstnow - cave - save) to
     *          this particular source's integration time and do the
     *          same steps as CAL: except use src.
     *        Otherwise, test time as: (cave + thresh) > stop:
     *          If that succeeds, add (stop - lstnow - cave - save) to
     *          this particular source's integration time and reduce
     *          the (temporary) new stop time to be (stop - cave) and
     *          do the same steps as CAL: except use src (integration
     *          time will be automatically truncated).
     *        If that time test fails, goto FINAL CAL:
     *  return to MAIN LOOP:
     *
     *  FINAL CAL:
     *    If cals exists...
     *      If lstnow > stop, exit.
     *      Scan cal;
     *      If the return status is an error or missed, notify the user.
     *
     *  --- What to do about missed markers? ---
     *  If both are missed, then break out of main loop.  When the
     *  loop is entered, both are set to be missed.  For each setup,
     *  if the setup exists, the state is reversed to be not missed
     *  and then it is only set when it is not actually observed (missed).
     */

    time = (cave > 0) ? cave : save;
    calmissed = srcmissed = checkTime(time);

    while ((calmissed == 0) || (srcmissed == 0)) {
      /*
       *  The next statement is for the case when there is only
       *  a src or a cal setups but not both.  Whenever a setup
       *  exists, the marker is reinitialized to 0.
       */
      calmissed = srcmissed = 1;

      if (cals != (const char *)NULL) {
        calmissed = 0;
        csetup = strcpy(calibs, cals);
        if (checkTime((unsigned int)cave))
          break;
        start = timeMarker();
        calstate = doScan(csetup, fd, project, &ncals);
        stop = timeMarker();

        if (AppDebug > 0) {
          first = start / ONEMINUTE;
          last = stop / ONEMINUTE;
          (void)printf("Setup [%s] ran [%2.2ld:%2.2ld-%2.2ld:%2.2ld] [%ld] ",
            cals, ((first / 60) % 24), (first % 60),
                  ((last  / 60) % 24), (last  % 60), cave);
          (void)printf("in main loop and returned [%s].\n",
            ((calstate == ERROR) ? "ERROR" :
             (calstate ==  NONE) ? "NONE"  : "OKAY"));
        }

        if (calstate == ERROR) {
          Warning(fd, "%s Setup [%s] returned to LOOP with an error state.\n",
            formatTime(), cals);
          calmissed = 1;
        } else if (calstate == NONE) {
          Message(fd, 1, "%s Setup [%s] returned to LOOP without observing.\n",
            formatTime(), cals);
          calmissed = 1;
        } else if (calstate == OKAY) {
          if (retstate != OKAY)
            retstate = calstate;
          ctime[ical] = stop - start;
          if (ctime[ical] < 0) ctime[ical] += FULLDAY;
          if (ctime[ical] > (2 * cave)) ctime[ical] = (ctime[ical] + cave) / 2;
          ical = (ical + 1) % MAXAVE;
          cave = 0;
          for (j = 0; j < MAXAVE; j++) cave += ctime[j]; cave /= MAXAVE;
          cave = 0.5 + ((double)cave / ONEMINUTE); cave *= ONEMINUTE;
        }
      }

      if (srcs != (const char *)NULL) {
        srcmissed = 0;
        ssetup = strcpy(sources, srcs);
        if (threshold < 0) {
          thresh = (cave < (save / 2)) ? cave : (save / 2);
        } else {
          thresh = threshold;
        }
        if (thresh > (save / 2)) {
          Message(fd, 1,
            "%s Setup [%s] threshold [%d] larger than 50%% the ave src time [%d].\n",
            formatTime(), srcs, (thresh / ONEMINUTE), (save / ONEMINUTE));
        }
        if (checkLoopSrcTime(save, cave, thresh, nsrcs))
          break;
        start = timeMarker();
        srcstate = doScan(ssetup, fd, project, &nsrcs);
        stop = timeMarker();
        setLoopSrcTime(0, 0);

        if (AppDebug > 0) {
          first = start / ONEMINUTE;
          last = stop / ONEMINUTE;
          (void)printf("Setup [%s] ran [%2.2ld:%2.2ld-%2.2ld:%2.2ld] [%ld] ",
            srcs, ((first / 60) % 24), (first % 60),
                  ((last  / 60) % 24), (last  % 60), save);
          (void)printf("in main loop and returned [%s].\n",
            ((srcstate == ERROR) ? "ERROR" :
             (srcstate ==  NONE) ? "NONE"  : "OKAY"));
        }

        if (srcstate == ERROR) {
          Warning(fd, "%s Setup [%s] returned to LOOP with an error state.\n",
            formatTime(), srcs);
          srcmissed = 1;
        } else if (srcstate == NONE) {
          Message(fd, 1, "%s Setup [%s] returned to LOOP without observing.\n",
            formatTime(), srcs);
          srcmissed = 1;
        } else if (srcstate == OKAY) {
          if (retstate != OKAY)
            retstate = srcstate;
          stime[isrc] = stop - start;
          if (stime[isrc] < 0) stime[isrc] += FULLDAY;
          if (stime[isrc] > (2 * save)) stime[isrc] = (stime[isrc] + save) / 2;
          isrc = (isrc + 1) % MAXAVE;
          save = 0;
          for (j = 0; j < MAXAVE; j++) save += stime[j]; save /= MAXAVE;
          save = 0.5 + ((double)save / ONEMINUTE); save *= ONEMINUTE;
        }
      }
    }
    /*
     *  FINAL CAL:
     *    If cals exists...
     *      If lstnow > stop, exit.
     *      Scan cal;
     *      If the return status is an error or missed, notify the user.
     */
    if (cals != (const char *)NULL) {
      csetup = strcpy(calibs, cals);
      if (checkTime(0) == 0) {
        calstate = doScan(csetup, fd, project, &ncals);

        if (AppDebug > 0) {
          (void)printf(
            "Setup [%s] ran in the final calib step and returned [%s].\n",
            cals, ((calstate == ERROR) ? "ERROR" :
                   (calstate ==  NONE) ? "NONE"  : "OKAY"));
        }

        if (calstate == ERROR) {
          Warning(fd, "%s Setup [%s] returned to LOOP with an error state.\n",
            formatTime(), cals);
        } else if (calstate == NONE) {
          Message(fd, 1, "%s Setup [%s] returned to LOOP without observing.\n",
            formatTime(), cals);
        } else if (calstate == OKAY) {
          if (retstate != OKAY)
            retstate = calstate;
        }
      }
    }

    return(retstate);
}

/***********************************************************************/
#ifdef __STDC__
int main(int argc, char *argv[])
#else
int main(argc, argv)
int argc;
char *argv[];
#endif /*__STDC__*/
{
    char *project;
    char *history;
    char *starttime;
    char *stoptime;
    char *srcs;
    char *cals;
    char filename[MAXBUF];
    char projectPath[MAXBUF];
    register int j;
    int state;
    long int thresh;
    FILE *fd;
    STATUS status;

    project = PROJECT;
    history = "history";
    starttime = (char *)NULL;
    stoptime = (char *)NULL;
    srcs = (char *)NULL;
    cals = (char *)NULL;
    thresh = -1;

    setProgram("LOOP: ");
    for (j = 1; j < argc; j++) {
      if (strncmp("srcsetup=", argv[j], 6) == 0) {
        srcs = strchr(argv[j], '=') + 1;
      } else if (strncmp("calsetup=", argv[j], 6) == 0) {
        cals = strchr(argv[j], '=') + 1;
      } else if (strncmp("stop=", argv[j], 5) == 0) {
        stoptime = strchr(argv[j], '=') + 1;
      } else if (strncmp("start=", argv[j], 6) == 0) {
        starttime = strchr(argv[j], '=') + 1;
      } else if (strncmp("thresh=", argv[j], 7) == 0) {
        thresh = atol(strchr(argv[j], '=') + 1);
        thresh *= ONEMINUTE;
      } else if (strncmp("project=", argv[j], 8) == 0) {
        project = strchr(argv[j], '=') + 1;
      } else if (strncmp("history=", argv[j], 8) == 0) {
        history = strchr(argv[j], '=') + 1;
      } else if (strncmp("debug=", argv[j], 6) == 0) {
        AppDebug = atoi(strchr(argv[j], '=') + 1);
      } else if (strcmp("-debug", argv[j]) == 0) {        /* Obsolete! */
        AppDebug = 1;
      } else {
        Warning(stderr, "Input keyword [%s] ignored.\n", argv[j]);
      }
    }

    /* Check that the "setup" keyword was input correctly. */
    if ((srcs == (char *)NULL) && (cals == (char *)NULL)) {
      Warning(stderr,
        "Either the 'srcsetup' or 'calsetup' keyword must be specified.\n");
      exit(1);
    }
    if (srcs != (char *)NULL) srcs = skipLeading(srcs);
    if (cals != (char *)NULL) cals = skipLeading(cals);

    /*  Check that the current time is in the proper range. */
    if (setTimes(starttime, stoptime))
      exit(0);

    /* Set up the name of the default project directory. */
    (void)strcpy(projectPath, project);
    if (projectPath[strlen(projectPath)-1] != '/')
      (void)strcat(projectPath, "/");

    (void)strcpy(filename, projectPath);
    (void)strcat(filename, history);
    if ((fd = fopen(filename, "a")) == (FILE *)NULL)
      fd = stdout;

    if (srcs != (char *)NULL)
      Message(fd, 0, "%s Source setup to be executed: [%s].\n",
        formatTime(), srcs);
    else
      Message(fd, 0, "%s No source setup to be executed.\n", formatTime());

    if (cals != (char *)NULL)
      Message(fd, 0, "%s Calibrator setup to be executed: [%s].\n",
        formatTime(), cals);
    else
      Message(fd, 0, "%s No calibrator setup to be executed.\n", formatTime());

    status = doLoop(srcs, cals, thresh, fd, projectPath);

    if (fd != stdout)
      (void)fclose(fd);

    state = ((status != OKAY) && (status != NONE));
    return(state);
}
