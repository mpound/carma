c************************************************************************
c= Tomin
c& jm
c: Tools, Utility
c+
       program tomin
c
c  tomin converts "hhmm" time format into integer minutes.  This would
c  be trivial except for the special terms allowed (like "stop" and
c  "+30" and "stop-10").  The integer value is written to standard out.
c
c@ time
c  Specifies the time in the special "hhmm" format.
c
c--
c------------------------------------------------------------------------
      character error*80
      real value
c
      character itoaf*80
      integer Len1
c
      call mbusopen(error)
      if (error(1:2) .eq. 'OK') then
        call keyini
        call keytime('time', value, 0.0, error)
        call keyfin
      endif
      if (error(1:2) .ne. 'OK') call bug('f', error)
      error = itoaf(int(value * 60))
      call output(error(1:Len1(error)))
      end
