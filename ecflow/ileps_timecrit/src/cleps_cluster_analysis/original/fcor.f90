!-----------------------------------------------------------------------

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:35

!     Area:        Area Modellistica
!     Group:       Ensemble Prediction System
!     Author:      Fabrizio Nerozzi
!     Last Update: 3.12.2001

!     Compute Correlation Coefficient

!-----------------------------------------------------------------------

FUNCTION fcor(n,x,y)

implicit none


INTEGER, INTENT(IN)                      :: n
REAL*4, INTENT(IN OUT)                   :: x(n)
REAL*4, INTENT(IN OUT)                   :: y(n)

INTEGER :: i
REAL    :: fcor, xstd, ystd, fstd, xavg, yavg, favg


fcor=0.
xstd=fstd(n,x)
IF(xstd > 0.)THEN
  ystd=fstd(n,y)
  IF(ystd > 0.)THEN
    xavg=favg(n,x)
    yavg=favg(n,y)
    fcor=(x(1)-xavg)*(y(1)-yavg)
    DO i=2,n
      fcor=fcor+(x(i)-xavg)*(y(i)-yavg)
    END DO
    fcor=fcor/(xstd*ystd)/REAL(MAX(1,n-1))
  END IF
END IF
RETURN
END FUNCTION fcor

