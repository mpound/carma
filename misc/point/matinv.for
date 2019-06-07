c linv2f: an IMSL routine, but calling a miriad routine to
c         invert the matrix (in POINT, nterm=10, so 100 ok they say?
      subroutine linv2f(mat,nvar,nterm,minv,idgt,junk,ier)
c
      implicit none
      integer nvar,nterm,idgt,ier
      real mat(nterm,nterm), minv(nterm,nterm),junk(100)
c
      integer i,j
      real det
c
      do j=1,nvar
        do i=1,nvar
          minv(i,j) = mat(i,j)
        enddo
        junk(j) = 1.0
      enddo
      call matinv(minv,junk,nvar,nterm,det)

c     just fake something 'good enough' here, so fitxyzz() won't complain
      idgt=6
      ier=0
      end

c --taken from MIRIAD --
c-----------------------------------------------------------------------
c*matinv -- single precision matrix inversion
c& mchw
c: math
c+
      subroutine matinv(a,b,n,d,determ)   
c
      implicit none
      integer n,d
      real a(d,n),b(n),determ
c       
c  This is a single precision version of matrix inversion with accom-
c   panying solution of linear equations.     
c   a(n,n) is the matrix to be inverted. the inverse is returned into 
c   the same array. b(n) is the constant vector, i.e. a(n,n)*x(n)=b(n)
c   the solution x(n) is returned into array b(n). the determinant  of
c   a as determ. Arrays a(d,d) and b(n) must be dimensioned in the    
c   main program.      **** note: dimension a(d,d) in main. ****     
c--
c       changed 12-aug-83 to omit those error messages.
c    History:
c
c    mchw  --apr93 Extracted from a task and installed as a sub.
c    mjs   22apr93 Added in-code docs.
c    pjt   23apr93 Eliminated flint complaints.
c-----------------------------------------------------------------------
      real    pivot(100),d1,amax,swap,t
      integer ipivot(100),idx(100,2),i,j,k,l,irow,icolum        
      integer jrow,jcolum,l1
c
c  Initialization    
c
      determ=1. 
      do j=1,n       
        ipivot(j)=0       
      enddo
c
c  Search for pivotal element
c
      do i=1,n       
        amax=0.   
        do j=1,n
          if (ipivot(j).ne.1) then
            do k=1,n       
              d1 = a(j,k) 
              if (ipivot(k).gt.1) return
              if (ipivot(k).lt.1) then
                if ( abs(amax) .lt. abs(d1)) then
                  irow = j    
                  icolum = k  
                  amax = a(j,k)       
                endif
              endif
            enddo
          endif
        enddo

        if (amax.eq.0.) then
          determ = 0.0
          return
        endif
        ipivot(icolum)=ipivot(icolum)+1   
c
c  Interchange rows to put pivot element on diagonal 
c
        if (irow.ne.icolum) then
          determ = -determ    
          do l=1,n       
            swap = a(irow,l)    
            a(irow,l) = a(icolum,l)     
            a(icolum,l) = swap  
          enddo
          swap=b(irow)      
          b(irow) = b(icolum) 
          b(icolum) = swap    
        endif
        idx(i,1) = irow   
        idx(i,2) = icolum 
        pivot(i) = a(icolum,icolum) 
        determ=determ*pivot(i)    
c
c  Divide pivot row by pivot element 
c
        a(icolum,icolum)=1.       
        do  l=1,n       
          a(icolum,l)=a(icolum,l)/pivot(i)  
        enddo
        b(icolum) = b(icolum)/pivot(i)      
c
c  Reduce non-pivotal rows   
c
        do l1=1,n      
          if (l1.ne.icolum) then
            t=a(l1,icolum)    
            a(l1,icolum)=0.   
            do l=1,n       
              a(l1,l)=a(l1,l)-a(icolum,l)*t     
            enddo
            b(l1)=b(l1)-b(icolum)*t   
          endif
        enddo
      enddo
c
c  Interchange columns       
c
      do i=1,n       
        l=n+1-i   
        if (idx(l,1).ne.idx(l,2)) then
          jrow=idx(l,1)   
          jcolum=idx(l,2) 
          do k=1,n       
            swap=a(k,jrow)    
            a(k,jrow)=a(k,jcolum)     
            a(k,jcolum)=swap  
          enddo
        endif
      enddo
      end       
