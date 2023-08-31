MODULE cla_algorithms
IMPLICIT NONE

CONTAINS

SUBROUTINE clink(x, w, dx, ixc, nxc, nc)
! Description:
!     Complete linkage hierarchical scheme for clustering

! Method:
!     Merge two clusters who have the lowest maximum distance among members

! Current Code Owner: Fabrizio Nerozzi

! History:
! Version   Date     Comment
! -------   ----     -------
! 1.00      31.07.01 Original code. F.Nerozzi

REAL,INTENT(in) :: x(:,:), w(:)
REAL,INTENT(out) :: dx(:,:)
INTEGER,INTENT(out) :: ixc(:,:), nxc(:)
INTEGER,INTENT(in) :: nc

INTEGER :: n,m
INTEGER :: kc,i,ic,k,jc,j,icmin,jcmin
REAL    :: dsup, dmax, dmin

!----------------------------------------------------------------------
!  [1.0] Initialize:
!----------------------------------------------------------------------

m=SIZE(x,1)
n=SIZE(x,2)

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

END SUBROUTINE clink


SUBROUTINE wards(x, w, dx, ixc, nxc, nc)
! Description:
!     Ward's hierarchical scheme for clustering

! Method:
!     Merge two clusters who have the lowest variance increment

! Current Code Owner: Fabrizio Nerozzi

! History:
! Version   Date     Comment
! -------   ----     -------
! 1.00      31.07.01 Original code. F.Nerozzi

REAL,INTENT(inout) :: x(:,:)
REAL,INTENT(in) :: w(:)
REAL,INTENT(out) :: dx(:,:)
INTEGER,INTENT(out) :: ixc(:,:), nxc(:)
INTEGER,INTENT(in) :: nc

INTEGER :: n,m
INTEGER :: i,j,k,ic,jc,kc,icmin,jcmin
REAL    :: dsup,var,varmin

!----------------------------------------------------------------------
!  [1.0] Initialize:
!----------------------------------------------------------------------

n=SIZE(x,1)
m=SIZE(x,2)

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

END SUBROUTINE wards


SUBROUTINE cstat(x, w, ixc, nxc, nc, var)
! Description:
!     Calculate global and internal variances

! Current Code Owner: Fabrizio Nerozzi

! History:
! Version   Date     Comment
! -------   ----     -------
! 1.00      31.07.01 Original code. F.Nerozzi

REAL,INTENT(in) :: x(:,:), w(:)
INTEGER,INTENT(in) :: ixc(:,:), nxc(:)
INTEGER,INTENT(in) :: nc
REAL,INTENT(out) :: var(:)

INTEGER :: n, m
INTEGER :: i,j,ic
REAL    :: x0

!----------------------------------------------------------------------
!  [1.0] Calculate global and internal variances:
!----------------------------------------------------------------------

n=SIZE(x,1)
m=SIZE(x,2)

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

END SUBROUTINE cstat


SUBROUTINE selrm(dx, nc, ixc, nxc, irc, mode) !!! added dx
!idx     selezione dell'elemento rappresentativo per ogni cluster
!        Author:      Fabrizio Nerozzi
!        Area:        Area Modellistica
!        Group:       Ensemble Prediction System
!        Last Update: 31.7.2001

REAL,INTENT(in) :: dx(:,:)
INTEGER,INTENT(in) :: nc, ixc(:,:), nxc(:)
INTEGER,INTENT(out) ::  irc(:)
INTEGER,INTENT(in) :: mode

INTEGER :: n
INTEGER :: i,j,k,ic,jc
REAL    :: dsup, dinf, dmax, dmin, dint, dext, rmin,ratio


n = SIZE(dx,1)
irc = 0 ! to avoid undefined if it is longer than required

! find out the largest distance
dsup=dx(1,1)
dinf=dx(1,1) ! was (1,2)
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

END SUBROUTINE selrm


END MODULE cla_algorithms
