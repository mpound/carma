c
c= UNLOCK  reset all software locks
c: observing
c+
	PROGRAM UNLOCK

C  UNLOCK:	just unlocks all of the common block locks:
c		LOCKANTS, LOCKCOR, LOCKFREQ, LOCKRCVR, LOCKSRC
c		note: this is dangerous to do while observing. 
c
c--
c  jan 92 wh
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='UNLOCK (version 1.0 7-jan-92)')

	include '../inc/constants.inc'
	integer locks(12) /12*0/
	character*80 error

	call output(version)
	call computi('LOCKANT',locks,nants,error)
	 if(error(1:2).ne.'OK') type *,error
	call computi('LOCKCOR',locks,1,error)
	 if(error(1:2).ne.'OK') type *,error
	call computi('LOCKFREQ',locks,1,error)
	 if(error(1:2).ne.'OK') type *,error
	call computi('LOCKRCVR',locks,1,error)
	 if(error(1:2).ne.'OK') type *,error
	call computi('LOCKSRC',locks,1,error)
	 type *,error

	end
