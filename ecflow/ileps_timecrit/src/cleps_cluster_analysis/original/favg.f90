!-----------------------------------------------------------------------

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:32

!     Area:        Area Modellistica
!     Group:       Ensemble Prediction System
!     Author:      Fabrizio Nerozzi
!     Last Update: 3.12.2001

!     Compute average of an array

!-----------------------------------------------------------------------

FUNCTION favg(n,x)

implicit none

INTEGER, INTENT(IN)                      :: n
REAL, INTENT(IN)                         :: x(n)

INTEGER :: i
REAL    :: favg

favg=x(1)
DO i=2,n
  favg=favg+x(i)
END DO
favg=favg/REAL(MAX(1,n))
RETURN
END FUNCTION favg
