c***********************************************************************
c
c  History:
c    29oct93  jm  Original program.
c    03feb94  jm  Moved sleeping step responsibility to calling program.
c    10feb94  jm  Added call to Len1 to truncate output message.
c    08oct96  jm  Modified to permit (legal) 0000 time inputs.
c***********************************************************************
c= Timesup - Determines if the current time is in range.
c& jm
c: observing
c+
      program timesup
c
c  This is a program which tests whether or not the current time is
c  within the range specified by the inputs start=hhmm and stop=hhmm.
c  The program returns a status value of 1 if there is an error in
c  the inputs; otherwise, the status value is 0.  If the exit status
c  is zero, then the program will echo either an output string of
c  "start=hhmm stop=hhmm [sleepsec=secs]" or "quit".  If the string
c  is "quit", then the current time is out of range.  If the string
c  contains start and stop keywords, then these represent the proper
c  start and stop times in hhmm format (and properly wrapped around
c  24 hours).  If the start and stop keywords are output, there may
c  also be an sleepsec keyword which represents the number of seconds
c  that remain until the start time.
c
c  An example of usage is:
c
c    unset sleepsec
c    set times = "`timesup start=1300 stop=2000`"
c    if ($status == 1) then
c      echo "Error testing time ranges"
c      exit 1
c    endif
c    if ("$times" == "quit") then
c      echo "Time is up."
c      exit 0
c    endif
c    set $times
c    if ($?sleepsec) then
c      echo "Sleeping for $sleepsec seconds until the start time."
c      sleep $sleepsec
c    endif
c
c
c  The arguments to this program include:
c
c@ stop
c       This value represents the stoping time.  It is given in ``hhmm''
c       format.  If the ``hhmm'' string is proceeded by an explicit `+'
c       sign (for example: +0130), then the value is treated as an
c       offset time relative to the current LST.  The value may also
c       be the string ``stop'' (the default) which specifies the
c       project stop time.
c
c@ start
c       This value represents the starting time.  If this value is
c       negative, then the start time is set to the current LST.
c       If the value is proceeded by an explicit "+" sign, then the
c       value is treated as an offset time relative to the current
c       LST.  By default, start is equal to the LST.
c
c@ wait
c       If the current time has not reached the start time and the
c       current LST is within the window specified by the ``wait''
c       time, then the program will pause until the start time is
c       reached.  If the current LST time is not within the ``wait''
c       time of the start time, then the routine will return the
c       string ``quit''.  By default, wait is equal to 0 (no waiting).
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      character error*80, message*80, csecs*10
      integer i, seconds
      integer istart, istop, iwait, ilst, isleep
      integer hh0, mm0, hh1, mm1
      real start, stop, wait, lst, pstop
c
      integer Len1
c
      call mbusopen(error)
      if (error(1:2) .ne. 'OK') goto 99
c
      call keyini
      call keytime('start', start, 25.0, error)
      if (error(1:2) .ne. 'OK') goto 99
      call keytime('stop',   stop, 25.0, error)
      if (error(1:2) .ne. 'OK') goto 99
      call keytime('wait',   wait, 0.0, error)
      if (error(1:2) .ne. 'OK') goto 99
      call keyfin
c
c  Get the current time and the project stop time.
c
      call strtime('lst', lst, error)
      if (error(1:2) .ne. 'OK') goto 99
      call strtime('stop', pstop, error)
      if (error(1:2) .ne. 'OK') goto 99
c
c  Since keytime limits input time to be between [0, 24) hours,
c  any value greater than 24 implies the default was used.
c
      if (start .gt. 24.0) start = lst
      if (stop  .gt. 24.0) stop = pstop
c
      ilst = lst * 60
      iwait = wait * 60
      istop = stop * 60
      istart = start * 60
c
      isleep = mod((1440 + istart - ilst), 1440)
c
c  If the start time has not been reached and is within the window
c  of the value of wait, then wait.  If the stop time has past,
c  then just quietly exit.
c
      seconds = 0
      if (istart .lt. istop) then
        if ((ilst .lt. istart) .or. (ilst .ge. istop)) then
          if (isleep .le. iwait) then
            if (isleep .gt. 0) seconds = isleep * 60
          else
            write(message, '(A)') 'quit'
            goto 99
          endif
        endif
      else
        if ((ilst .lt. istart) .and. (ilst .ge. istop)) then
          if (isleep .le. iwait) then
            if (isleep .gt. 0) seconds = isleep * 60
          else
            write(message, '(A)') 'quit'
            goto 99
          endif
        endif
      endif
c
      hh0 = mod((istart / 60), 24)
      mm0 = mod(istart, 60)
      hh1 = mod((istop / 60), 24)
      mm1 = mod(istop, 60)
c
      if (seconds .gt. 0) then
        write(csecs, '(I10)') seconds
        i = 1
  111   if (csecs(i:i) .eq. ' ') then
          i = i + 1
          goto 111
        endif
        write (message, '(A, 2I2.2, X, A, 2I2.2, A, A)')
     *    'start=', hh0, mm0, 'stop=', hh1, mm1, ' sleepsec=', csecs(i:)
      else
        write (message, '(A, 2I2.2, X, A, 2I2.2)')
     *    'start=', hh0, mm0, 'stop=', hh1, mm1
      endif
c
   99 continue
      if (error(1:2) .ne. 'OK') call bug('f', error)
      i = Len1(message)
      call output(message(1:i))
      call exit(0)
      end
