!-----------------------------------------------------------------------

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:59

!     Area:        Area Modellistica
!     Group:       Ensemble Prediction System
!     Author:      Fabrizio Nerozzi
!     Last Update: 3.12.2001

!     Standardize variable

!-----------------------------------------------------------------------

SUBROUTINE stdv(n,x,z)

implicit none

INTEGER, INTENT(IN)                      :: n
REAL, INTENT(IN OUT)                     :: x(n)
REAL, INTENT(OUT)                        :: z(n)

INTEGER :: i
REAL    :: xstd, xavg, fstd, favg

xstd=fstd(n,x)
IF(xstd > 0.)THEN
  xavg=favg(n,x)
  DO i=1,n
    z(i)=(x(i)-xavg)/xstd
  END DO
ELSE
  DO i=1,n
    z(i)=0.
  END DO
END IF
RETURN
END SUBROUTINE stdv
