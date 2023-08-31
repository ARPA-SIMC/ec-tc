MODULE cla_stats
IMPLICIT NONE

CONTAINS

SUBROUTINE standardize(x)
REAL,INTENT(inout) :: x(:)

REAL :: xstd, xavg

xstd = fstd(x)
IF(xstd > 0.)THEN
  xavg = favg(x)
  x = (x-xavg)/xstd
ELSE
  x = 0.
ENDIF

END SUBROUTINE standardize


FUNCTION fstd(x)
REAL,INTENT(in) :: x(:)
REAL :: fstd

REAL :: xavg
INTEGER :: i

fstd = 0.
IF (SIZE(x) <=1 ) RETURN

xavg = favg(x)
DO i = 1, SIZE(x)
  fstd = fstd + (x(i)-xavg)**2
ENDDO

fstd = SQRT(fstd/REAL(SIZE(x)-1))

END FUNCTION fstd


FUNCTION favg(x)
REAL,INTENT(in) :: x(:)
REAL :: favg

IF (SIZE(x) <=0 ) THEN
  favg = 0.
  RETURN
ENDIF

favg = SUM(x)/SIZE(x)

END FUNCTION favg

END MODULE cla_stats
