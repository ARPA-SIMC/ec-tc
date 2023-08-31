!omstart ward-s-algorithm

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:37:49

!vedi    complete-linkage-algorithm   cluster-statistics   cluster-representative-member   esempio

SUBROUTINE wards(n,m,x,w,dx,ixc,nxc,nc)

! Description:
!     Ward's hierarchical scheme for clustering

! Method:
!     Merge two clusters who have the lowest variance increment

! Current Code Owner: Fabrizio Nerozzi

! History:
! Version   Date     Comment
! -------   ----     -------
! 1.00      31.07.01 Original code. F.Nerozzi

! Code Description:
!   Language:           Fortran 77.
!   Software Standards:
!   "European Standards for Writing and
!     Documenting Exchangeable Fortran 90 Code".

! Declarations:

! Array  arguments with intent(in):

implicit none

INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: m
REAL, INTENT(IN OUT)                     :: x(n,m)
REAL, INTENT(IN)                         :: w(m)
REAL, INTENT(OUT)                        :: dx(n,m)
INTEGER, INTENT(OUT)                     :: ixc(n,n)
INTEGER, INTENT(OUT)                     :: nxc(n)
INTEGER, INTENT(IN OUT)                  :: nc

INTEGER :: i,j,k,ic,jc,kc,icmin,jcmin
REAL    :: dsup,var,varmin
! Array  arguments with intent(inout):



! Array  arguments with intent(out):



!omend


!----------------------------------------------------------------------
!  [1.0] Initialize:
!----------------------------------------------------------------------

! initialise index matrix
kc=n
DO ic=1,kc
  DO i=1,n
    ixc(i,ic)=0
  END DO
  ixc(1,ic)=ic
  nxc(ic)=1
END DO

! calculate distances between members
DO i=1,n
  dx(i,i)=0.
  DO j=i+1,n
    dx(i,j)=0.
    DO k=1,m
      dx(i,j)=dx(i,j)+w(k)*(x(i,k)-x(j,k))**2
    END DO
    dx(i,j)=SQRT(dx(i,j))
    dx(j,i)=dx(i,j)
  END DO
END DO

! find out the largest distance
dsup=dx(1,1)
DO i=1,n-1
  DO j=i+1,n
    IF(dsup < dx(i,j))THEN
      dsup=dx(i,j)
    END IF
  END DO
END DO


!----------------------------------------------------------------------
!  [2.0] Hierarchical scheme:
!----------------------------------------------------------------------

DO WHILE(kc > nc)
  
! find the two closest clusters
  varmin=dsup**2
  DO ic=1,kc-1
    DO jc=ic+1,kc
      var=REAL(nxc(ic)*nxc(jc))/REAL(nxc(ic)+nxc(jc)) *dx(ic,jc)**2
      IF(var < varmin)THEN
        icmin=ic
        jcmin=jc
        varmin=var
      END IF
    END DO
  END DO
  
! compute the mass centre of the new cluster
  DO k=1,m
    x(icmin,k)=(nxc(icmin)*x(icmin,k)+nxc(jcmin)*x(jcmin,k))  &
        /(nxc(icmin)+nxc(jcmin))
    DO ic=jcmin,kc-1
      x(ic,k)=x(ic+1,k)
    END DO
  END DO
  
! shrank distance matrix
  DO ic=1,kc
    DO jc=jcmin,kc-1
      dx(ic,jc)=dx(ic,jc+1)
    END DO
  END DO
  DO ic=jcmin,kc-1
    DO jc=1,kc-1
      dx(ic,jc)=dx(ic+1,jc)
    END DO
  END DO
  
! recompute distances
  DO jc=1,kc-1
    dx(icmin,jc)=0.
    DO k=1,m
      dx(icmin,jc)=dx(icmin,jc)+w(k)*(x(icmin,k)-x(jc,k))**2
    END DO
    dx(icmin,jc)=SQRT(dx(icmin,jc))
    dx(jc,icmin)=dx(icmin,jc)
  END DO
  
! merge two clusters
  DO jc=1,nxc(jcmin)
    ic=jc+nxc(icmin)
    ixc(ic,icmin)=ixc(jc,jcmin)
    ixc(jc,jcmin)=0
  END DO
  nxc(icmin)=nxc(icmin)+nxc(jcmin)
  nxc(jcmin)=0
  DO jc=jcmin,kc-1
    DO j=1,nxc(jc+1)
      ixc(j,jc)=ixc(j,jc+1)
      ixc(j,jc+1)=0
    END DO
    nxc(jc)=nxc(jc+1)
    nxc(jc+1)=0
  END DO
  kc=kc-1
END DO

RETURN
END SUBROUTINE wards
