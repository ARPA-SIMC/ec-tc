!omstart cluster-representative-member

! Code converted using TO_F90 by Alan Miller
! Date: 2009-06-03  Time: 14:38:55

!vedi    ward-s-algorithm   cluster-statistics   complete-linkage-algorithm   esempio
!idx     selezione dell'elemento rappresentativo per ogni cluster

!        subroutine selrm(n,m,x,w,nc,ixc,nxc,irc,mode)

!        integer ixc(n,nc), nxc(nc), irc(nc)
!        real*4  x(n,m), w(m), dx(n,n)

!        Author:      Fabrizio Nerozzi
!        Area:        Area Modellistica
!        Group:       Ensemble Prediction System
!        Last Update: 31.7.2001

!omend

SUBROUTINE selrm(n,m,x,w,dx,nc,ixc,nxc,irc,mode) !!! added dx

implicit none

INTEGER, INTENT(IN)                      :: n
INTEGER, INTENT(IN)                      :: m
REAL*4, INTENT(IN OUT)                   :: x(n,m)
REAL*4, INTENT(IN OUT)                   :: w(m)
REAL*4, INTENT(IN)                       :: dx(n,n)
INTEGER, INTENT(IN)                      :: nc
INTEGER, INTENT(IN)                      :: ixc(n,nc)
INTEGER, INTENT(IN)                      :: nxc(nc)
INTEGER, INTENT(OUT)                     :: irc(nc)
INTEGER, INTENT(IN OUT)                  :: mode

INTEGER :: i,j,k,ic,jc
REAL    :: dsup, dinf, dmax, dmin, dint, dext, rmin,ratio

irc = 0 ! to avoid undefined if it is longer than required
! compute distances between ensemble members
DO i=1,n
!         dx(i,i)=0.
  DO j=i+1,n
!            dx(i,j)=0.
    DO k=1,m
!               dx(i,j)=dx(i,j)+w(k)*(x(i,k)-x(j,k))**2
    END DO
!            dx(i,j)=sqrt(dx(i,j))
!            dx(j,i)=dx(i,j)
  END DO
END DO
! find out the largest distance
dsup=dx(1,1)
dinf=dx(1,2)
DO i=1,n-1
  DO j=i+1,n
    IF(dsup < dx(i,j))THEN
      dsup=dx(i,j)
    ELSE IF(dinf > dx(i,j))THEN
      dinf=dx(i,j)
    END IF
  END DO
END DO
! find out the minimum internal distance
IF(mode == 1)THEN
  DO ic=1,nc
    dmin=dsup
    DO i=1,nxc(ic)
      DINT=0.
      DO j=1,nxc(ic)
        DINT=DINT+dx(ixc(i,ic),ixc(j,ic))
      END DO
      DINT=DINT/REAL(MAX(1,nxc(ic)))
      IF(DINT < dmin)THEN
        irc(ic)=ixc(i,ic)
        dmin=DINT
      END IF
    END DO
  END DO
! find out the largest external distance
ELSE IF(mode == 2)THEN
  DO ic=1,nc
    dmax=dinf
    DO i=1,nxc(ic)
      dext=0.
      DO jc=1,nc
        IF(jc /= ic)THEN
          DO j=1,nxc(jc)
            dext=dext+dx(ixc(i,ic),ixc(j,jc))
          END DO
        END IF
      END DO
      dext=dext/REAL(n-nxc(ic))
      IF(dext > dmax)THEN
        irc(ic)=ixc(i,ic)
        dmax=dext
      END IF
    END DO
  END DO
! find out the minimum ratio
ELSE IF(mode == 3)THEN
  DO ic=1,nc
    rmin=dsup/dinf
    DO i=1,nxc(ic)
      DINT=0.
      DO j=1,nxc(ic)
        DINT=DINT+dx(ixc(i,ic),ixc(j,ic))
      END DO
      DINT=DINT/REAL(MAX(1,nxc(ic)))
      dext=0.
      DO jc=1,nc
        IF(jc /= ic)THEN
          DO j=1,nxc(jc)
            dext=dext+dx(ixc(i,ic),ixc(j,jc))
          END DO
        END IF
      END DO
      dext=dext/REAL(MAX(1,(n-nxc(ic))))
      IF(dext > 0.)THEN
        ratio=DINT/dext
      ELSE
        ratio=DINT
      END IF
      IF(ratio < rmin)THEN
        irc(ic)=ixc(i,ic)
        rmin=ratio
      END IF
    END DO
  END DO
END IF
RETURN
END SUBROUTINE selrm


