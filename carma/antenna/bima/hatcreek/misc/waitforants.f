c
c
c
c
      PROGRAM waitforants
c   
c    waitforants : Callable routine that will wait until the
c                  antennas are not slewing. Basiclly just
c                  calls the subroutine wait_for_ants
c
      character  error*80
      integer ants
      call keyini
      call keyants(ants,0,error)
      call keyfin
      call wait_for_ants(ants,14,error)
      end
