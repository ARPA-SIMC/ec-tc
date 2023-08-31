!omstart cluster-statistics

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:15

!vedi    complete-linkage-algorithm   ward-s-algorithm   cluster-representative-member   esempio

SUBROUTINE cstat(n,m,x,w,ixc,nxc,nc,var)

! Description:
!     Calculate global and internal variances

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
REAL, INTENT(IN)                         :: x(n,m)
REAL, INTENT(IN)                         :: w(m)
INTEGER, INTENT(IN OUT)                  :: ixc(n,n)
INTEGER, INTENT(IN)                      :: nxc(n)
INTEGER, INTENT(IN)                      :: nc
REAL, INTENT(OUT)                        :: var(nc+1)

INTEGER :: i,j,ic
REAL    :: x0


!omend


!----------------------------------------------------------------------
!  [1.0] Calculate global and internal variances:
!----------------------------------------------------------------------

! compute global variance of the ensemble
var(nc+1)=0.
DO j=1,m
  x0=x(1,j)
  DO i=2,n
    x0=x0+x(i,j)
  END DO
  x0=x0/REAL(MAX(1,n))
  DO i=1,n
    var(nc+1)=var(nc+1)+w(j)*(x(i,j)-x0)**2
  END DO
END DO

! compute internal variance of clusters
DO ic=1,nc
  var(ic)=0.
  DO j=1,m
    x0=x(ixc(1,ic),j)
    DO i=2,nxc(ic)
      x0=x0+x(ixc(i,ic),j)
    END DO
    x0=x0/REAL(MAX(1,nxc(ic)))
    DO i=1,nxc(ic)
      var(ic)=var(ic)+w(j)*(x(ixc(i,ic),j)-x0)**2
    END DO
  END DO
END DO

RETURN
END SUBROUTINE cstat
