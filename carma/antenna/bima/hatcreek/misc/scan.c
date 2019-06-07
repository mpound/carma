/***********************************************************************/
/*
 *  History:
 *    01oct93 jm  Original code.
 *    20dec93 jm  Modified setup reader to allow setup=a,b,c...
 *                and also added svalue calls to test against maxobs.
 *    06jan94 jm  Extracted subroutines and added include of setups.h.
 *    24jan94 jm  Added doc comments.
 *    08mar94 jm  Updated doc comments and log output messages.
 *    20sep94 jm  Modified for changes to calling syntax of Message().
 *    20may96 jm  Cleaned up argument parsing and doc sections.
 */
/***********************************************************************/
/*= Scan -- Observe based on the contents of the input setup. */
/*& jm */
/*: observing */
/*+ */
/*
   Scan is a command used to simplify the observing process.  It
   performs the same actions an observer usually performs but does so
   in a way as to change the thought process from arguments of
   individual observing commands to elements of a setup file for a
   source.  The commands that are internally called are:
     message*
     setalarm*
     lstsymbol*
     alter*
     value*
     svalue*
     cksrc
     setcorr
     setfreq
     mint
   Commands with an asterisk after their name are called as part of
   the observing process and are not influenced by the observer's
   setup contents.  For example, lstsymbol is used to monitor how the
   current LST compares with the stop time and if the task has expired.

   The contents of the setup file indicate how a source is to be
   observed.  Each entry in the setup file is a characteristic of
   how this particular source should be observed.  If the number of
   required keywords for a particular application in a setup file
   is insufficient, then that command is not called for that setup.

   The logic of how each setup is observed is as follows:
   1. The setup keyword may be a single setup or multiple setups
      separated by logical operators.  Setups separated by OR's (the
      character '|') are only executed if the preceeding setups could
      not be successfully observed.  Setups separated by AND's (',')
      are always run.

   2. Each run setup will first be tested to see if the source is "up"
      based on the criteria of LST time ranges and elevation ranges.
      If the setup does not contain the source keyword, then this test
      is skipped.  If the setup is tested and is not visible, no
      further processing of the setup is done.

   3. Next, if the proper keywords exist and are different from the
      current settings, tuning is done and the correlator is set.

   4. Finally, if (at least) the "source" and "vis" keywords are present,
      the observing task is executed.  If no observing task is specified
      in the setup, then "mint" is run.

   Keywords for scan include:
    maxobs - Specifies the maximum number of times this setup can
             be executed.  If missing or zero, no limitations are
             placed on the number of times this setup can be executed.
    setup  - Inserts the contents of the specified setup into the
             current contents of this setup.
    start  - The time to start the observing task.  If this is not
             specified, the observing task will begin immediately.
    stop   - The time to stop the observing task.  This should be,
             in general, specified as a relative time and specifies
             the duration of this instance of the observing task.
             For more information, see help on 'keytime'.
    source - The name of the source to observe.  If this is missing,
             the correlator and frequency may still be adjusted but
             no observing will be done.
    catalog - The name of the catalog to identify the source.  If
             is missing, j2000.cat is used.
    ants   - The antennas to use in this setup.  This defaults to all
             available antennas.
    elevlim[2] - The minimum and maximum elevation limits of the source.
             Inside this range, the source is considered "up".
    lstrange[8] - LST time ranges to consider the source visible.
    cormode, coropt, corf[4], corbw[4], - Correlator settings.  See
             help on the command 'setcorr' for further details.
    freq, iffreq, obsline, restfqs[16], harm, options - Frequency and
             tuning parameters.  Set help on 'setfreq' for more info.
    dopsrc - The name of the source used for doppler tracking.
    dopcat - The name of the catalog used to identify dopsrc.
    itime, trakopt, nchan, grid, scale, nreps - Parameters used by
             the observing task "mint".  See mint for further details.
    vis    - The name of the output visibility file.  If this is not
             present (along with "source"), the observing task is not
             executed.

   Optional Control Keywords for Scan:

    task   - The name of the observing task to run.  This, currently,
             can only be one of "int", "mint", "qint", or "mint2".
    checktask - The name of the source checking routine task to run.
             This, currently, can only be "cksrc".
    tunetask - The name of the tuning task to run.  This, currently,
             can only be "setfreq".
    corrtask - The name of the correlator setup task to run.  This,
             currently, can only be "setcorr".

   Error handling:
   The program exits with a status of 1 on input (keyword) error or
   if there was an error with some process along the way.  The program
   exits with a status of 1 if none of the setups were observed.  It
   exits with a status of 0 if at least one setup was observed
   correctly.
 */
/*@ setup
   The name of the setup to process.  This can be a simple (single)
   setup or can be a group of setup names separated by logic delimiters.
   The actual contents of the setup are described above.  There
   is no default for this keyword and it must be provided.
 */
/*@ start
   The earliest time to start processing the setup.  If the current
   LST time is earlier than this input time, the program will WAIT
   until that time is reached.  If this keyword is not present, or
   the time is earlier than the current LST, the program will begin
   processing the setup immediately.
 */
/*@ stop
   The time to stop processing the input setup.  The scan command
   will terminate at the earliest of either this input time or the
   project stop time.  If this keyword is not present, then the
   project stop time is used.  This keyword is generally not needed
   as scan usually finishes processing the setup keyword before the
   stop time is reached.
 */
/*@ debug
   A non-zero value will provide additional messages which are output
   and may help to debug scripts.
 */
/*--*/

#include "setups.h"

int AppDebug = 0;                          /* Initially, no debugging. */

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
    char *setups;
    char filename[MAXBUF];
    char projectPath[MAXBUF];
    register int j;
    int nsrcs, state;
    FILE *fd;
    STATUS status;

    project = PROJECT;
    history = "history";
    starttime = (char *)NULL;
    stoptime = (char *)NULL;
    setups = (char *)NULL;

    setProgram("SCAN: ");
    for (j = 1; j < argc; j++) {
      if (strncmp("setup=", argv[j], 6) == 0) {
        setups = strchr(argv[j], '=') + 1;
      } else if (strncmp("stop=", argv[j], 5) == 0) {
        stoptime = strchr(argv[j], '=') + 1;
      } else if (strncmp("start=", argv[j], 6) == 0) {
        starttime = strchr(argv[j], '=') + 1;
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
    if (setups == (char *)NULL) {
      Warning(stderr, "The setup keyword must be specified.\n");
      exit(1);
    }
    setups = skipLeading(setups);
    if (*setups == Null) {
      Warning(stderr, "No value is associated with the keyword setup.\n");
      exit(1);
    }

    /* Check that time current time is in the proper range. */
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

    Message(fd, 0, "%s Setup to be executed: [%s].\n", formatTime(), setups);

    status = doScan(setups, fd, projectPath, &nsrcs);

    if (fd != stdout)
      (void)fclose(fd);

    state = ((status != OKAY) && (status != NONE));
    return(state);
}
