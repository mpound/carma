c***********************************************************************
c= SVALUE - (Re)sets or gets a project's (named) value.
c& jm
c: observing
c+
      program svalue
c
c     SVALUE sets (or gets) the named variable of a setup to a
c     specified value.  This routine retrieves a setup variable
c     by name and, perhaps, sets or resets this value.  At the
c     end of the program, the sum of the values of all the named
c     setups is returned.  The variables are kept distinct by
c     forming the variable as "setup/name".  Currently, the
c     variables are stored in the file "project/variables".
c
c     NOTE: The commands scan and loop only use svalue for
c           setups that have the keyword "maxobs" set.
c
c@ setup
c     The name of a single setup or a list of setups.  Up to 10
c     setups may be included; there is no logic applied to the
c     setups in this program.  If this keyword is not present,
c     the default is to not prepend a setup to the variable name.
c
c@ name
c     The name of the internal variable.  If this keyword is not
c     present, then it defaults to 'obscnt'.
c
c@ value
c     The integer value that the named variable should be set to.
c     If this keyword is not present, then the program just returns
c     the current value.  If it is present, it is applied to each
c     setup.  This has a special integer format in that relative
c     values may be specified by prepending a literal sign to the
c     integer value.  For example, if value=+1, then each setup
c     will have the variable identified by name incremented by 1.
c
c--
c  History:
c    20dec93  jm   Original version.
c
c-----------------------------------------------------------------------
c
      character*(*) FILE
      parameter (FILE = 'project/variables')
      integer MAXCNT
      parameter (MAXCNT = 10)
c
      character*256 setup(MAXCNT)
      character*80 name, svalue
      character*256 variable
      integer i, j, k
      integer lu, iostat
      integer nsetups, lname
      integer vsign, newval, sumval
      integer value(MAXCNT)
      logical doset, relative
      logical exists
c
      character itoaf*10
      integer len1
      logical keyprsnt
c
c-----------------------------------------------------------------------
c
      call keyini
      call mkeya('setup', setup, MAXCNT, nsetups)
      call keya('name', name, 'obscnt')
      doset = keyprsnt('value')
      call keya('value', svalue, ' ')
      call keyfin
c
      lname = len1(name)
c
c-----------------------------------------------------------------------
c  Set up the default for the case when no setup is provided.
c
      if (nsetups .le. 0) then
        setup(1) = ' '
        nsetups = 1
      endif
c
c-----------------------------------------------------------------------
c  Decode the input value.
c
      if (doset) then
        vsign = +1
        relative = .FALSE.
        newval = 0
        i = 1
        j = len1(svalue)
        do while ((svalue(i:i) .le. ' ') .and. (i .le. j))
          i = i + 1
        enddo
        if (i .gt. j) call bug('f', 'Incorrectly formatted value.')
        if (svalue(i:i) .eq. '+') then
          i = i + 1
          relative = .TRUE.
        else if (svalue(i:i) .eq. '-') then
          i = i + 1
          vsign = -1
          relative = .TRUE.
        endif
        do while (i .le. j)
          k = ichar(svalue(i:i))
          if ((k .lt. ichar('0')) .and. (k .gt. ichar('9'))) then
            call bug('f', 'Incorrectly formatted value.')
          endif
          newval = (newval * 10) + k - ichar('0')
          i = i + 1
        enddo
        newval = vsign * newval
      endif
c
c-----------------------------------------------------------------------
c  Initialize the sum and the value array.
c
      sumval = 0
      do i = 1, nsetups
        value(i) = 0
      enddo
c
c-----------------------------------------------------------------------
c  Open the file (if it already exists) for reading and then retrieve
c  the current value for each named setup.
c
      inquire(file=FILE, exist=exists)
      if (exists) then
        call txtopen(lu, FILE, 'old', iostat)
        if (iostat .ne. 0) call bugno('f', iostat)
        call getval(lu, setup, name(1:lname), value, nsetups)
        call txtclose(lu)
      endif
c
      do i = 1, nsetups
        if (doset) then
          if (relative) then
            value(i) = value(i) + newval
          else
            value(i) = newval
          endif
        endif
        sumval = sumval + value(i)
      enddo
c
c-----------------------------------------------------------------------
c  Now, open the file for writing (if necessary), and write out the
c  new values.
c
      if (doset) then
        call txtopen(lu, FILE, 'append', iostat)
        if (iostat .ne. 0) then
          call bugno('w', iostat)
          call bug('w', 'Variable file will not be updated.')
        else
          do i = 1, nsetups
            if (setup(i) .ne. ' ') then
              j = len1(setup(i))
              variable = setup(i)(1:j) // '/' // name(1:lname)
              j = j + 1 + lname
            else
              variable = name(1:lname)
              j = lname
            endif
            variable(j+1:) = ' = ' // itoaf(value(i))
            j = len1(variable)
            call txtwrite(lu, variable, j, iostat)
          enddo
          call txtclose(lu)
        endif
      endif
c
      variable = itoaf(sumval)
      call output(variable(1:len1(variable)))
      end
c***********************************************************************
      subroutine getval(lu, setup, name, value, nsetups)
      integer lu, nsetups
      integer value(nsetups)
      character name*(*)
      character setup(nsetups)*(*)
c
c     This routine will read the entire file identified by the handle
c     LU in a fashion similar to the key routines.  This will insure
c     that the latest value is the retrieved and that the setup name
c     always overrides any global value.  If setup is a blank string,
c     then the global value is retrieved for name.  A value of 0 is
c     returned if no match is made.
c
c  Input:
c    lu      The handle of the text file.
c    setup   An array of setup names to associate with the variable.
c    name    The name of the variable to retrieve.
c    nsetups The number of setups present.
c
c  Output:
c    value   An array of returned values; each is 0 if not matched.
c
c--
c------------------------------------------------------------------------
c
      character string*256
      character key*256, thisname*256
      integer i, j
      integer length, iostat
      integer thisval
      logical exists
c
      integer len1
c
      call txtread(lu, string, length, iostat)
      do while (iostat .eq. 0)
        if (length .gt. 0) then
          call getnv(string, length, thisname, thisval, exists)
          if (exists) then
            do i = 1, nsetups
              if (setup(i) .ne. ' ') then
                j = len1(setup(i))
                key = setup(i)(1:j) // '/' // name
              else
                key = name
              endif
              if (key .eq. thisname) value(i) = thisval
            enddo
          endif
        endif
        call txtread(lu, string, length, iostat)
      enddo
c
      return
      end
c***********************************************************************
      subroutine getnv(string, strlen, name, value, exists)
      character string*(*), name*(*)
      integer strlen, value
      logical exists
c
c     This routine gets the name and value of the variable from the
c     input string.  This is done in a fashion similar to the key
c     routines.  A value of 0 is returned if no match is made.
c
c  Input:
c    string  The string to decode.
c    strlen  The length of the input string.
c
c  Output:
c    name    The name of the variable retrieved.
c    value   The associated value; 0 if not matched.
c    exists  TRUE if both a name and value were found.
c
c--
c------------------------------------------------------------------------
c
      character strval*256
      integer next, equals
      integer lfield
      double precision dval
c
      exists = .FALSE.
      next = 1
      equals = 1
      call scanchar(string, equals, strlen, '=')
      call getfield(string, next, equals, name, lfield)
      if ((lfield .le. 0) .or. (lfield .gt. len(name))) return
c
      call spanchar(string, next, strlen, ' ')
      call spanchar(string, next, strlen, '=')
      call spanchar(string, next, strlen, ' ')
      if ((strlen - next + 1) .le. 0) return
c
      call getfield(string, next, strlen, strval, lfield)
      if (lfield .le. 0) return
      call atoif(strval(1:lfield), value, exists)
      if (exists) return
      call atodf(strval(1:lfield), dval, exists)
      if (exists) value = nint(dval)
c
      return
      end
