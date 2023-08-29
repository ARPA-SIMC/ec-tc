!omstart complete-linkage-algorithm

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:00

!vedi    ward-s-algorithm   cluster-statistics   cluster-representative-member   esempio

SUBROUTINE clink(n,m,x,w,dx,ixc,nxc,nc)
!!!!!implicit none
! Description:
!     Complete linkage hierarchical scheme for clustering

! Method:
!     Merge two clusters who have the lowest maximum distance among members

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


INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: m
REAL, INTENT(IN)                         :: x(m,n)
REAL, INTENT(IN)                         :: w(m)
REAL, INTENT(OUT)                        :: dx(n,n)
INTEGER, INTENT(OUT)                     :: ixc(n,n)
INTEGER, INTENT(OUT)                     :: nxc(n)
INTEGER, INTENT(IN OUT)                  :: nc

INTEGER :: kc,i,ic,k,jc,j,icmin,jcmin
REAL    :: dsup, dmax, dmin
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
!ccc               dx(i,j)=dx(i,j)+w(k)*(x(i,k)-x(j,k))**2
      dx(i,j)=dx(i,j)+w(k)*(x(k,i)-x(k,j))**2
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
  dmin=dsup
  DO ic=1,kc-1
    DO jc=ic+1,kc
      dmax=0.
      DO i=1,nxc(ic)
        DO j=1,nxc(jc)
          IF(dx(ixc(i,ic),ixc(j,jc)) > dmax)THEN
            dmax=dx(ixc(i,ic),ixc(j,jc))
          END IF
        END DO
      END DO
      IF(dmax < dmin)THEN
        icmin=ic
        jcmin=jc
        dmin=dmax
      END IF
    END DO
  END DO
  
! merge two clusters
  DO j=1,nxc(jcmin)
    i=j+nxc(icmin)
    ixc(i,icmin)=ixc(j,jcmin)
    ixc(j,jcmin)=0
  END DO
  nxc(icmin)=nxc(icmin)+nxc(jcmin)
  nxc(jcmin)=0
  
! shrink matrix
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
END SUBROUTINE clink
