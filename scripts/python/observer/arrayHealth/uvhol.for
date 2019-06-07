c**********************************************************************c
      program uvhol
      implicit none
c
c= UVHOL - Scan a uvdata file and extract data for holography analysis.
c& jwl
c: uv analysis, checking
c+
c       UVHOL is a Miriad program which scans a uvdata file and extracts
c       the data required for holography data analysis. It has two main
c       functions controlled by the option keyword, to get information
c       about antenna offsets, frequencies, etc., or to get the
c       visibility data. The task was designed to be run from the python
c       holography reduction script, so the output format is carefully
c       controlled.
c
c       Since it has a more flexible output than uvlist, it can be used
c       for other purposes, but it is not guaranteed to do exactly
c       what you want. Flagged data are written out by default with no
c       indication that they are flagged. The 'unflag' option can be
c       invoked to suppress output of flagged data.
c
c@ vis
c       The input visibility file. No default.
c
c@ log
c       The output log file. Default is the terminal.
c
c@ options
c       This controls what is listed
c         "data"    Correlation data (default, observing parameters).
c         "time"    UT time instead of visibility no.
c         "unflag"  Output only unflagged data. The default is to
c                   output all data with no indication of flagging.
c
c@ select
c       This selects the data to be processed, using the standard
c       uvselect format. Default is all data.
c
c@ line
c       For options=data, this gives the linetype that is printed, in
c       the form:
c         type,nchan,start,width,step
c       where type can be `channel' (default), `wide' or `velocity'.
c       The default is to print all the raw channel data (no averaging,
c       etc). If options=brief, a maximum of only 6 channels will be
c       printed.
c
c--
c
c  History:
c    jwl  01aug08  Initial version.
c    jwl  08dec08  Changed printout to allow 16 visibilities (for SZA
c                  data or a window of 15 channels). Added UT option
c    jwl  06feb09  Added option to suppress flagged data. Tidied up
c                  code and improved comments.
c    jwl  09feb09  Fixed bug where sense of flag was inverted
c----------------------------------------------------------------------c
      include 'mirconst.h'
      include 'maxdim.h'
      character*(*) version
      integer MAXSRC, MAXFREQ, MAXSPECT, NUMANT
      parameter(MAXSRC = 5000, MAXFREQ = 32, MAXSPECT = MAXWIDE,
     *          NUMANT = 15)
      parameter(version = 'UVHOL: version 06-Feb-09')
      logical more
      integer maxsels
      parameter(maxsels = 1024)
c
      real sels(maxsels)
      real start, step, width
      integer lIn, i, j, j1, nants
      character vis*256, logf*80, outf*80, line*256, linetype*20, last*1
      integer isrc, nsrc, vsource
      logical newsrc
      integer num
      character sources(MAXSRC)*16, prevsrc*16
      integer indx(MAXSRC)
c
      integer ifreq, nfreq, vfreq
      logical newfreq
      integer nwide(MAXFREQ), nchan(MAXFREQ), nspect(MAXFREQ)
      integer nschan(MAXSPECT, MAXFREQ)
      real wfreqs(MAXSPECT, 3, MAXFREQ)
      double precision sfreqs(MAXSPECT, 3, MAXFREQ)
      double precision visno
      integer vpoint, refant
      real azeloff(2, NUMANT)
      logical movants(NUMANT), dodata, dotime, doflag
c
      complex data(maxchan)
      logical flags(maxchan)
      integer numchan, totflag
      double precision uin, vin, timein, basein, preamble(4)
      common/preamb/uin, vin, timein, basein
c
c  Externals
c
      integer uvscan
      character itoaf*8
c
c  Get the parameters given by the user.
c
      call output(version)
      call keyini
      call keyf('vis', vis, ' ')
      if (vis .eq. ' ') then
          call bug('f', 'Input file must be given (vis= )')
      end if
      call keya('log', logf, ' ')
      call keya('out', outf, ' ')
      call keyi('refant', refant, 1)
      call GetOpt(dodata, dotime, doflag)
      call SelInput('select', sels, maxsels)
      call keyline(linetype, numchan, start, width, step)
      call keyfin
c
c  Check that the inputs are reasonable.
c
      if(vis .eq. ' ') then
          call bug('f', 'Input name must be given')
      end if
c
c  Initialise the source table.
c
      do i = 1, MAXSRC
          sources(i) = ' '
      end do
c
c  Open an existing visibility file, and apply selection criteria.
c
      call uvopen(lIn, vis, 'old')
      call SelApply(lIn, sels, .true.)
      if (linetype .ne. ' ') then
          call uvset(lIn, 'data', linetype, numchan, start, width, step)
      end if
      call uvset(lIn, 'preamble', 'uv/time/baseline', 0, 0.0, 0.0, 0.0)
c
c  Open log file and write title.
c
      if ((logf .eq. '') .and. (outf .ne. '')) then
          logf = outf
      end if
      call LogOpen(logf, ' ')
      if (logf .ne. ' ') then
          call LogWrite(version, more)
      end if
c
c  Set up a variable handle to track changes in the source.
c
      call uvvarini(lIn, vsource)
      call uvvarset(vsource, 'source')
c
c  Set up a variable handle to track changes in the correlator/freq
c  setup.
c
      call uvvarini(lIn, vfreq)
      call uvvarset(vfreq, 'nchan')
      call uvvarset(vfreq, 'nspect')
      call uvvarset(vfreq, 'nschan')
      call uvvarset(vfreq, 'sfreq')
      call uvvarset(vfreq, 'sdf')
      call uvvarset(vfreq, 'nwide')
      call uvvarset(vfreq, 'wfreq')
      call uvvarset(vfreq, 'wwidth')
c
c  Set up a variable handle to track changes in the Az/El pointing
c
      call uvvarini(lIn, vpoint)
      call uvvarset(vpoint, 'dazim')
      call uvvarset(vpoint, 'delev')
c
c  Initialise the counters.
c
      nsrc = 0
      isrc = 0
      ifreq = 0
      nfreq = 0
      do i = 1, NUMANT
          movants(i) = .false.
      end do
c
c Decide whether to get general information required for holography
c script or visibility data
c
      if (dodata) then
          call uvread(lIn, preamble, data, flags, maxchan, numchan)
          last = ' '
          num = 0
          totflag = 0
          do while (numchan .gt. 0)
              num = num + 1
c
c  List the header, if required.
c
              call uvinfo(lIn, 'visno', VisNo)
              uin    = preamble(1)
              vin    = preamble(2)
              timein = preamble(3)
              basein = preamble(4)
              call LongDat(lIn, last .ne. 'd', basein, nint(VisNo),
     *                     data, flags, totflag, numchan, refant,
     *                     dotime, timein, doflag)
              last = 'd'

c
c  Loop the loop.
c
              call uvread(lIn, preamble, data, flags, maxchan, numchan)
          end do
          if (doflag) then
              write(line, '(a, i9)')
     *          'Number of output lines flagged (included in output): ',
     *          totflag
          else
              write(line, '(a, i9)')
     *          'Number of output lines flagged (omitted): ', totflag
          end if
          call output(line)
c
      else
c
c  Scan through the uvdata noting when a number of things change.
c
          do while(uvscan(lIn, ' ') .eq. 0)
c
c  Determine if anything has changed, and update the records
c  accordingly.
c
              newsrc = .false.
              call GetSrc(lIn, newsrc, isrc, nsrc, sources, MAXSRC)
              call GetFreq(lIn, newfreq, ifreq, nfreq, nchan, nspect,
     *                 nschan, sfreqs, nwide, wfreqs, MAXFREQ, MAXSPECT)
              call GetPnt(lIn, azeloff, nants)
c
c  Process AzEl offsets
c
              do i = 1, nants
                  if ((azeloff(1, i) .ne. 0.0) .or.
     *                (azeloff(2, i) .ne. 0.0)) then
                  movants(i) = .true.
                  end if
              end do
          end do
c
c  Sort and list the sources.
c
          call LogWrite(' ', more)
          call LogWrite('[Sources]', more)
          call hsorta(MAXSRC, sources, indx)
          prevsrc = ' '
          do j = 1, MAXSRC
              j1 = indx(j)
              if(sources(j1) .ne. ' ')then
                  if(sources(j1) .ne. prevsrc)then
                      write(line, '(a)')sources(j1)
                      call LogWrite(line, more)
                  end if
              end if
              prevsrc = sources(j1)
          end do
          call LogWrite('#', more)
c
c  Frequency setup summary.
c
          call LogWrite(' ', more)
          call LogWrite('[Frequency]', more)
          do ifreq = 1, nfreq
              call LogWrite('Configuration no. '//itoaf(ifreq), more)
              if(nchan(ifreq) .gt. 0) then
                  call SpecSum(nspect(ifreq), nschan(1, ifreq),
     *                         sfreqs(1, 1, ifreq), MAXSPECT)
              end if
              if(nwide(ifreq) .gt. 0) then
                  call WideSum(nwide(ifreq), wfreqs(1, 1, ifreq),
     *                         MAXSPECT)
              end if
          end do
c
c  List antennas with non-zero offsets
c
          call LogWrite(' ', more)
          call LogWrite('[Moving Antennas]', more)
          do i = 1, nants
              if (movants(i)) then
                  write(line, '(i3)') i
                  call LogWrite(line, more)
              end if
          end do
          call LogWrite('#', more)
      end if

      call LogClose
c
      end
c***********************************************************************
      subroutine GetSrc(lIn, newsrc, isrc, nsrc, sources, MAXSRC)
c
      implicit none
      integer MAXSRC
      integer lIn, isrc, nsrc
      logical newsrc
      character sources(MAXSRC)*(*)
c
c  Determine whether we have a new source and pointing or not.
c
c  Input:
c    mosaic     Treat the observation as a mosaic.
c  Output:
c    newsrc     True if the source has changes.
c-----------------------------------------------------------------------
c
      character source*16, osource*16
      logical more, found
      integer hash, i
c
c  Externals.
c
      integer len1
c
c  Get source parameters.
c
      call uvrdvra(lIn, 'source', source, '-unknown-')
c
c  Is it a new source?
c
      if(nsrc .eq. 0)then
          osource = ' '
          newsrc = .true.
      else
          osource = sources(isrc)
          if(source .eq. osource)then
              newsrc = .false.
          else
              newsrc = .true.
          end if
      end if
c
c  Process a new source.
c
      if(newsrc)then
          hash = 0
          do i = 1, len1(source)
              hash = 3 * hash + ichar(source(i:i))
          end do
          isrc = mod(hash, MAXSRC) + 1
          more = .true.
          found = .false.
          do while(more)
              if (sources(isrc) .eq. source) then
                  found = .true.
                  more = .false.
              else if(sources(isrc) .eq. ' ') then
                  found = .false.
                  more = .false.
              end if
              if (more) then
                  isrc = isrc + 1
                  if(isrc .gt. MAXSRC) isrc = 1
              end if
          end do
c
c  Did we find this source?
c
          if (.not. found) then
              nsrc = nsrc + 1
              call assertigti(MAXSRC, nsrc, 'MAXSRC: too many sources')
            sources(isrc) = source
          end if
      end if
      end
c***********************************************************************
      subroutine GetFreq(lIn, newfreq, ifreq, nfreq, nchan, nspect,
     *           nschan, sfreqs, nwide, wfreqs, MAXFREQ, MAXSPECT)
c
      implicit none
      integer MAXFREQ, MAXSPECT
      integer lIn, ifreq, nfreq, nchan(MAXFREQ), nspect(MAXFREQ)
      integer nschan(MAXSPECT, MAXFREQ), nwide(MAXFREQ)
      double precision sfreqs(MAXSPECT, 3, MAXFREQ)
      real wfreqs(MAXSPECT, 3, MAXFREQ)
      logical newfreq
c
c  Keep track of frequency/correlator setups. Determine whether we
c  have a new correlator or freq setup.
c
c  The frequency setup arrays, sfreqs and wfreqs (spectral and wide
c  frequencies respectively) contain three values,
c       freqs(?,1,?) is the start frequency in the first record.
c       freqs(?,2,?) is the bandwidth or channel increment.
c       freqs(?,3,?) is the start frequency in the last record.
c  The start frequency in the first and last record can differ, owing to
c  slow changes in the frequency caused by Doppler tracking and the
c  like.
c-----------------------------------------------------------------------
      integer itmp,i
      logical more
c
c  Externals.
c
      logical FreqEq
c
      itmp = nfreq + 1
      call assertigei(MAXFREQ, itmp,
     *      'MAXFREQ: Frequency table overflow')
c
c  Load the current freq/correlator description.
c
      call uvrdvri(lIn, 'nchan', nchan(itmp), 0)
      call uvrdvri(lIn, 'nspect', nspect(itmp), 0)
      call assertigei(MAXSPECT, nspect(itmp),
     *      'MAXSPECT:  too many windows')
      call uvrdvri(lIn, 'nwide', nwide(itmp), 0)
      call assertigei(MAXSPECT, nwide(itmp),
     *      'MAXSPECT: too many wide channels')
      if (nchan(itmp) .gt. 0) then
          call uvgetvrd(lIn, 'sfreq', sfreqs(1, 1, itmp), nspect(itmp))
          call uvgetvrd(lIn, 'sdf',   sfreqs(1, 2, itmp), nspect(itmp))
          call uvgetvri(lIn, 'nschan', nschan(1, itmp),   nspect(itmp))
      end if
      if (nwide(itmp) .gt. 0) then
          call uvgetvrr(lIn, 'wfreq', wfreqs(1, 1, itmp), nwide(itmp))
          call uvgetvrr(lIn, 'wwidth', wfreqs(1, 2, itmp), nwide(itmp))
      end if
c
c  Is it a new frequency/correlator setup?
c
      if (nfreq .eq. 0) then
          newfreq = .true.
      else
          newfreq = .not.FreqEq(ifreq, itmp, nchan, nspect, nschan,
     *                         sfreqs, nwide, wfreqs, MAXSPECT, MAXFREQ)
      end if
c
c  Process a new frequency.
c
      if (newfreq) then
          ifreq = 1
          more = .true.
          do while(ifreq .le. nfreq .and. more)
            more = .not. FreqEq(ifreq, itmp, nchan, nspect, nschan,
     *                         sfreqs, nwide, wfreqs, MAXSPECT, MAXFREQ)
              if (more) then
                  ifreq = ifreq + 1
              end if
          end do
c
c  If it is a totally new frequency, complete the description of it.
c  Most of the description is already in the right place.
c
          if (more) then
              nfreq = nfreq + 1
          end if
      end if
c
c  Update the start frequency column of the last record.
c
      do i = 1, nspect(ifreq)
          sfreqs(i, 3, ifreq) = sfreqs(i, 1, itmp)
      end do
      do i = 1, nwide(ifreq)
          wfreqs(i, 3, ifreq) = wfreqs(i, 1, itmp)
      end do
c
      end
c***********************************************************************
      logical function FreqEq(i1, i2, nchan, nspect, nschan, sfreqs,
     *          nwide, wfreqs, MAXSPECT, MAXFREQ)
c
      implicit none
      integer MAXSPECT, MAXFREQ
      integer i1, i2, nchan(MAXFREQ), nspect(MAXFREQ)
      integer nschan(MAXSPECT, MAXFREQ), nwide(MAXFREQ)
      double precision sfreqs(MAXSPECT, 3, MAXFREQ)
      real wfreqs(MAXSPECT, 3, MAXFREQ)
c
c  Determine whether two correlator/frequency setups are the same.
c  For them to be the same, nchan, nwide, nspec, nschan have to
c  match exactly. wwidth and sdf has to match to 1%, and sfreq and
c  wfreq have to match to within half a channel.
c
c-----------------------------------------------------------------------
      integer i
      real w
c
      FreqEq = .false.
      if (nchan(i1) .ne. nchan(i2).or.
     *    nspect(i1) .ne. nspect(i2).or.
     *    nwide(i1) .ne. nwide(i2)) then
          return
      end if
c
       do i = 1, nspect(i1)
          if (nschan(i, i1) .ne. nschan(i, i2)) then
              return
          end if
          w = abs(sfreqs(i, 2, i1))
          if (abs(sfreqs(i, 2, i1) - sfreqs(i, 2, i2)) .gt. 0.01 * w)
     *        then
              return
          end if
          if (abs(sfreqs(i, 3, i1) - sfreqs(i, 1, i2)) .gt. 0.5 * w)
     *        then
              return
          end if
      end do
c
      do i = 1, nwide(i1)
          w = abs(wfreqs(i, 2, i1))
          if (abs(wfreqs(i, 2, i1) - wfreqs(i, 2, i2)) .gt. 0.01 * w)
     *        then
              return
          end if
          if (abs(wfreqs(i, 3, i1) - wfreqs(i, 1, i2)) .gt. 0.5 * w)
     *        then
              return
          end if
      end do
c
      FreqEq = .true.
      end
c***********************************************************************
      subroutine SpecSum(nspect, nschan, sfreqs, MAXSPECT)
c
      implicit none
      integer nspect, nschan(nspect), MAXSPECT
      double precision sfreqs(MAXSPECT, 3)
c
c  Write a summary about this spectral correlator configuration.
c  Assume Doppler tracking is being used if the difference between
c  the first and last start frequencies is more than 0.1 of a channel.
c-----------------------------------------------------------------------
      character line*80
      integer i
      logical more
      double precision favg
c
      write(line, '(a, i4)')'No. spectral bands ', nspect
      call LogWrite(line, more)
      call LogWrite('Freq(avg) Increment     Channels', more)
      do i = 1, nspect
          favg = 0.5d0 * (sfreqs(i, 1) + sfreqs(i, 3) + sfreqs(i, 2) *
     *    nschan(i))
          write(line, '(f10.5, f12.6, a, i5)')
     *          favg, sfreqs(i, 2), ' GHz', nschan(i)
          call LogWrite(line, more)
      end do
      call LogWrite('#', more)
      end
c***********************************************************************
      subroutine WideSum(nwide, wfreqs, MAXSPECT)
c
      implicit none
      integer nwide, MAXSPECT
      real wfreqs(MAXSPECT, 3)
c
c  Write a summary about this wideband correlator configuration.
c-----------------------------------------------------------------------
      character line*80
      integer i
      logical more
c
      write(line, '(a, i4)')'No. wide bands ', nwide
      call LogWrite(line, more)
      call LogWrite('Frequency       Bandwidth', more)
      do i = 1, nwide
          write(line, '(f10.5, f12.6, a)')
     *          wfreqs(i, 1), wfreqs(i, 2), ' GHz'
          call LogWrite(line, more)
      end do
      call LogWrite('#', more)
      end
c***********************************************************************
      subroutine getPnt(lIn, azeloff, nants)
      implicit none
      integer lIn, nants, i
      include 'maxdim.h'
      include 'mirconst.h'
      real azeloff(2, MAXANT)
c
      double precision dazim(MAXANT), delev(MAXANT)
c
      call uvrdvri(lIn, 'nants', nants, 0)
      call uvgetvrd(lIn, 'dazim', dazim, nants)
      call uvgetvrd(lIn, 'delev', delev, nants)
c
      do i = 1, nants
          azeloff(1, i) = dazim(i) * 180 * 60 / PI
          azeloff(2, i) = delev(i) * 180 * 60 / PI
      end do
      end
c***********************************************************************
      subroutine GetOpt(dodata, dotime, doflag)
c
      implicit none
      logical dodata, dotime, doflag
c-----------------------------------------------------------------------
      integer NOPTS
      parameter(NOPTS = 3)
      character opts(NOPTS)*8
      logical present(NOPTS)
      data opts/'data  ', 'time  ', 'unflag'/
c
      call options('options', opts, present, NOPTS)
      dodata    = present(1)
      dotime    = present(2)
      doflag = .not. present(3)
      end
c********1*********2*********3*********4*********5*********6*********7**
      subroutine LongDat(lIn, needhd, basein, VisNo, data, flags,
     *                    totflag, numchan, refant, dotime, timein,
     *                    doflag)
      implicit none
      integer lIn, numchan, VisNo, refant, totflag
      logical needhd, flags(numchan), dotime, doflag
      complex data(numchan)
      double precision timein, basein
      include 'mirconst.h'
      include 'maxdim.h'
c
c  Do a full listing of the data.
c
c  Input:
c    needhd     If true, give a heading line.
c    VisNo      Visibility number.
c    data       The correlation data.
c    flags      The data flags.
c    numchan    The number of channels.
c    refant     Reference antenna to get daz, del for
c    dotime     Flag to output time rather than visibility no.
c    timein     UT time to print out, if required
c-----------------------------------------------------------------------
      real rtoh, rtod
      integer mchan
      parameter(rtoh = 12 / pi, rtod = 180 / pi)
      parameter(mchan = 16)
      character line*2000
      real amp(mchan), phas(mchan)
      logical more, ok, recflag
      integer i, j, ant1, ant2, nchan, nants
      character type*1
      double precision dazim(MAXANT)
      double precision delev(MAXANT)
      integer length
c
c  Externals.
c
c
      if(needhd)then
          if(dotime)then
            call LogWrite('[Vis]', more)
            line = '          UT time #  A1  A2      dAz      dEl ' //
     *             'Win        Amp      Phase  Win...'
            call LogWrite(line, more)
          else
            call LogWrite('[Vis]', more)
            line = '  Vis #  A1  A2      dAz      dEl ' //
     *             'Win        Amp      Phase  Win...'
            call LogWrite(line, more)
          end if
      end if
c
c  Get the preamble.
c
      call basant(basein, ant1, ant2)
c
c  List the channel data.
c
      recflag = .false.
      do i = 1, numchan, mchan
          nchan = min(numchan - i + 1, mchan)
          do j = 1, nchan
              recflag = recflag .or. (.not. flags(i + j - 1))
              call amphase(data(i + j - 1), amp(j), phas(j))
          end do
          if (recflag) then
              totflag = totflag + 1
          end if
c
c Following are specifically for input to holography script
c
          if (doflag .or. (.not. recflag)) then
              call uvrdvri(lIn, 'nants', nants, 0)
              call uvprobvr (lIn, 'dazim', type, length, ok)
              if (type .eq. 'd') then
                  call uvgetvrd(lIn, 'dazim', dazim, nants)
              end if
              call uvprobvr (lIn, 'delev', type, length, ok)
              if (type .eq. 'd') then
                  call uvgetvrd(lIn, 'delev', delev, nants)
              end if
              if (dotime) then
                  write(line, '(d19.13, 1x, i3, 1x, i3, 1x, f8.3,' //
     *               '1x, f8.3, 1x, 100(1x, i2, 1x, e10.4, 1x, f10.3))')
     *                timein, ant1, ant2,
     *                dazim(refant) * rtod * 60,
     *                delev(refant) * rtod * 60,
     *                (i + j - 1, amp(j), phas(j), j = 1, nchan)
              else
                  write(line, '(i7, 1x, i3, 1x, i3, 1x, f8.3, 1x,' //
     *                'f8.3, 1x, 100(1x, i2, 1x, e10.4, 1x, f10.3))')
     *                Visno, ant1, ant2,
     *                dazim(refant) * rtod * 60,
     *                delev(refant) * rtod * 60,
     *                (i + j - 1, amp(j), phas(j), j = 1, nchan)
              end if
              call LogWrite(line, more)
          end if
      end do
c
      end
c********1*********2*********3*********4*********5*********6*********7**
