!-----------------------------------------------------------------------

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:39

!     Area:        Area Modellistica
!     Group:       Ensemble Prediction System
!     Author:      Fabrizio Nerozzi
!     Last Update: 3.12.2001

!     Compute standard deviation of an array

!-----------------------------------------------------------------------

FUNCTION fstd(n,x)


INTEGER, INTENT(IN)                      :: n
REAL, INTENT(IN OUT)                     :: x(n)

INTEGER :: i
REAL    :: fstd

xavg=favg(n,x)
fstd=(x(1)-xavg)**2
DO i=2,n
  fstd=fstd+(x(i)-xavg)**2
END DO
fstd=SQRT(fstd/REAL(MAX(1,n-1)))
RETURN
END FUNCTION fstd
