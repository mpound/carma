c***********************************************************************
c= CKSRC - Determines if the source is visible.
c& jm
c: observing
c+
      program cksrc
c
c     CKSRC calculates the source elevation and determines if it
c     is visible within an elevation range and a LST time range for
c     a particular epoch and LST.  This is useful for command files
c     which need to check if a source is visible at the current time
c     or some later time.  Note that is determines the source elevation
c     and not the antenna elevation.
c
c     NOTE:  NAIF (the program which searches the catalog) saves the
c     last source/catalog found so when looking up a source twice in
c     succession, the second time repeats the first.  Look up a
c     different source to force a new catalog lookup.
c
c@ source
c     Source name [maximum of 8 characters] from catalog or a planet.
c     Other objects on the NAIF approved list can be accessed as
c     "_xxxxxx" where "xxxxxx" is the NAIF number.  For example, comet
c     shoemaker-levy has number 1000130.  There is no default for
c     this keyword and at least one source name must be supplied.
c     Multiple sources may be specified but only the first is tested.
c
c@ catalog
c     Catalog name.  First "catalog" is searched (this can be a full
c     path and not just a filename).  If this is not found, then the
c     path is stripped off and $HATCAT/filename is searched.  By
c     default, $HATCAT/j2000.cat is used.
c     Multiple catalog entries may be specified but only the first
c     is used.
c
c@ epoch
c     Julian Day number of the observation.  The default of 0 means
c     the present epoch.  A format like 'YYMMMDD:[HH:MM...]' is
c     converted to a julian day number (compatible with the output
c     from the command "date -u +%y%h%d:%H:%M:%S").   A number in
c     the range -1000 to 1000 means delta days from the present epoch.
c
c@ elevlim
c     Specifies the lower and the upper elevation limit in degrees.
c     The first value is the lower elevation limit (defaults to 10
c     degrees) and the second limit, the upper extreme (defaults to
c     90 degrees).  (Realize that adding 180 degrees to the azimuth
c     switches the elevation of the source to (180 - elevation).)
c
c@ lst
c     Specifies the source testing time.  This can be an absolute time
c     or a time relative to the current LST or the project stop time.
c     The keyword should have the form [+|-]hhmm[+hhmm|-hhmm ...]
c     (where the terms in brackets are optional).  The default value
c     is the current LST.
c
c@ lstrange
c     Specifies time ranges over which a source is considered valid.
c     If the source is at the proper elevation but the LST is not
c     within the window specified by this keyword, the source is not
c     considered visible.  Up to 4 time ranges can be entered as:
c       lstrange=lstmin1,lstmax1,lstmin2,lstmax2,lstmin3,....
c     The time format is the same as for the lst keyword.  The default
c     time value for the minimum time range is one hour before the lst
c     and the default for the maximum time range is two hours after the
c     corresponding minimum time range.
c
c@ latitude
c     The latitude of the observatory in degrees.
c     The default for Hat Creek is 40.81736111 (40 49 02.5).
c
c--
c  History:
c    03nov93  jm   Major revision of checksource to handle the special
c                  case and enhancement of the keyword setme=visible.
c    20dec93  jm   Modified to compute only the source elevation.
c    07feb94  jm   Fixed wrap around problem for default LST values.
c    16oct95  jm   Changed maximum elevation default to 85 degrees.
c    22may96  jm   Modified to permit multiple source/catalog entries.
c                  Changed call for keyword epoch from keya to keyepoch.
c    01jul96  jm   Removed 16oct95 change.  Added a warning message.
c
c-----------------------------------------------------------------------
c
      integer MAXTIMES
      parameter (MAXTIMES = 8)
      integer MAXSRCS
      parameter (MAXSRCS = 20)
      real PI
      parameter (PI = 3.14159265358979323846)
c
      character*12 src(MAXSRCS)
      character*100 cat(MAXSRCS)
      character*80 error
      integer i, nsrcs, ncats
      integer nrange
      real lstdef
      real lst, lat, ha
      real cosz, elevsrc
      real elev(2), lstrange(MAXTIMES)
      double precision epoch
      double precision ra0, dec0
      logical visible
c
      logical keyprsnt
c
c-----------------------------------------------------------------------
c
      call mbusopen(error)
      if (error(1:2) .ne. 'OK') goto 99
c
      call keyini
      call mkeya('source', src, MAXSRCS, nsrcs)
      call mkeya('catalog', cat, MAXSRCS, ncats)
      call keyepoch('epoch', epoch, 0.0d0)
      call keyr('elevlim', elev(1), 10.0)
      call keyr('elevlim', elev(2), 90.0)
      call keyr('latitude', lat, 40.81736111)
      call keytime('lst', lst, -1.0, error)
      if (error(1:2) .ne. 'OK') goto 99
c
      nrange = 0
   20 continue
        lstdef = lst - 1.0
        if (lstdef .le. 0.0) lstdef = lstdef + 24.0
        nrange = nrange + 1
        call keytime('lstrange', lstrange(nrange), lstdef, error)
        if (error(1:2) .ne. 'OK') goto 99
        lstdef = lstrange(nrange) + 2.0
        if (lstdef .gt. 24.0) lstdef = lstdef - 24.0
        nrange = nrange + 1
        call keytime('lstrange', lstrange(nrange), lstdef, error)
        if (error(1:2) .ne. 'OK') goto 99
      if ((keyprsnt('lstrange')) .and. (nrange .lt. MAXTIMES)) goto 20
c
      call keyfin
c
      if (elev(1) .gt. elev(2)) then
        elevsrc = elev(1)
        elev(1) = elev(2)
        elev(2) = elevsrc
      endif
c
c-----------------------------------------------------------------------
c  Check that valid keyword values were entered.
c
      if ((nsrcs .lt. 1) .or. (src(1) .eq. ' ')) then
        error = 'A source name must be present.'
        goto 99
      endif
      if (elev(1) .eq. elev(2)) then
        error = 'Elevation ranges were incorrectly specified.'
        goto 99
      endif
      if (elev(1) .lt. 0.0) then
        write (error, '(A, F5.2, A)')
     *    'Elevation lower limit too small: [', elev(1), ' < 0].'
        goto 99
      endif
      if (elev(2) .gt. 90.0) then
        write (error, '(A, F5.2, A)')
     *    'Elevation upper limit too large: [', elev(2), ' > 90].'
        goto 99
      endif
c
      do 30 i = ncats + 1, nsrcs
        cat(i) = ' '
   30 continue
      ncats = nsrcs
c
c-----------------------------------------------------------------------
c  Test the LST time range.
c
      error = 'OK'
      visible = .FALSE.
c
      do 40 i = 1, nrange, 2
        if (lstrange(i) .lt. lstrange(i+1)) then
          if ((lstrange(i) .le. lst) .and. (lst .le. lstrange(i+1)))
     *      visible = .TRUE.
        else
          if ((lstrange(i) .le. lst) .or. (lst .le. lstrange(i+1)))
     *      visible = .TRUE.
        endif
   40 continue
      if (.not. visible) goto 99
c
c  Get the source's RA and Dec.
c
      call getradec(src(1), cat(1), epoch, ra0, dec0, error)
      if (error(1:2) .ne. 'OK') goto 99
c
c  Compute the source's elevation.  Other antenna azimuthal values
c  (+/- 180 degrees) will produce antenna elevations that are the
c  flip mode of the solved value (i.e. 180 - elevation).
c
      lat = lat * PI / 180.0
      ha = (lst * PI / 12.0) - ra0
      if (ha .gt. PI) ha = ha - (2.0 * PI)
      cosz = (sin(lat) * sin(dec0)) + (cos(lat) * cos(dec0) * cos(ha))
      elevsrc = asin(cosz) * 180.0 / PI
      if ((elevsrc .lt. elev(1)) .or. (elevsrc .gt. elev(2)))
     *  visible = .FALSE.
c
      if (elevsrc .gt. 85.0)
     *  call bug('w', 'Elevation >85 degs; expect lost observing time.')
c
   99 continue
      if (error(1:2) .ne. 'OK') call bug('f', error)
      if (visible) then
        call output('y')
      else
        call output('n')
      endif
      end
c***********************************************************************
      subroutine getradec(source, catalog, epoch, ra, dec, error)
      character source*(*), catalog*(*), error*(*)
      double precision epoch, ra, dec
c
c  Sends message to ephemeris to precess a source without locks.
c  This version does not fill in common variables.
c
c-----------------------------------------------------------------------
c
      include '../inc/ephem.inc'
      record /asource/ s
      integer ticks, mon, day, year
      double precision freq, tjd
      double precision ut, lst
c
      double precision julian
c
c-----------------------------------------------------------------------
c
      call ucase(source)
c
c  No action necessary if the source is TEST.
c
      error = 'OK'
      ra = 0.0D0
      dec = 0.0D0
      if (source(1:4) .eq. 'TEST') return
c
c  Otherwise get position from the ephemeris routines.
c
      call comgetd('FREQ', freq, 1, error)
      if ((freq .lt. 50.0D0) .or. (error(1:2) .ne. 'OK'))  freq = 90.0D0
c
      tjd = epoch
      if (abs(epoch) .le. 1000.0D0) then
        call get_times(ticks, 1, mon, day, year, ut, lst, error)
        if (error(1:2) .ne. 'OK')  return
        tjd = julian(day, mon, year, ut) + epoch
      endif
c
c  Retrieve the source information.
c
      call ephemeris(source, catalog, tjd, freq, s, error)
      if (error(1:2) .eq. 'OK') then
        ra = s.ra0
        dec = s.dec0
      endif
c
      return
      end
