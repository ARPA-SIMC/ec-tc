PROGRAM cleps_cluster_analysis
USE cla_inputdata
USE cla_stats
USE cla_algorithms
IMPLICIT NONE

INTEGER :: i, j, k, ic, igp, iswp
REAL,ALLOCATABLE :: statbuff(:)
REAL,POINTER :: x3(:,:,:), x2(:,:)

REAL :: r0, rd, wt

CALL cla_read_data('cluster_analysis.naml')

ALLOCATE(statbuff(gridsize*nmember*nens))
DO i = 1, SIZE(varlist)
  DO j = 1, SIZE(levlist)
    DO k = 1, nstep
      statbuff = RESHAPE(x(:,k,j,i,:,:), SHAPE(statbuff))
      CALL standardize(statbuff)
      x(:,k,j,i,:,:) = RESHAPE(statbuff, (/gridsize,nmember,nens/))
    ENDDO
  ENDDO
ENDDO

! create a degenerated 2-d view (variable,ensemble-member) of the
! array by using a pointer to an array, Fortran ~2008 feature
x2(1:gridsize*nstep*SIZE(levlist)*SIZE(varlist),1:nmember*nens) => x
! create a transposed version of the aforementioned array, since it is
! more efficient for some subsequent algorithms
x2t = TRANSPOSE(x2)

! compute meridional weights
r0 = lat_first*(ASIN(1.)/90.)
rd = delta_j*(ASIN(1.)/90.)
wt=0.
DO j=1,jpoints
   DO i=1,ipoints
      igp=i+(j-1)*ipoints
      w(igp)=COS(r0-rd*(j-1))
   END DO
   wt=wt+COS(r0-rd*(j-1))
END DO
! normalize weights
DO j=1,jpoints
   DO i=1,ipoints
      igp=i+(j-1)*ipoints
      w(igp)=w(igp)/wt
   END DO
 END DO
! extend to all array, can we avoid this somehow (cache-inefficient)?
DO i = 2, SIZE(varlist)*SIZE(levlist)*nstep
  w((i-1)*gridsize+1:i*gridsize)=w(1:gridsize)
END DO

IF (clmethod == 1) THEN
  CALL wards(x2t, w, dx, ixc, nxc, nclust)
! restore x2t since it has been overwritten in wards
  x2t = TRANSPOSE(x2)
ELSE
  CALL clink(x2, w, dx, ixc, nxc, nclust)
ENDIF

CALL cstat(x2t, w, ixc, nxc, nclust, var)
WRITE(71)x2t
WRITE(72)w
WRITE(73)dx
WRITE(74)ixc
WRITE(75)nxc

CALL selrm(dx, nclust, ixc, nxc, irc, rmmethod)
WRITE(76,*)irc

DO ic=1,nclust
   DO i=1,nxc(ic)-1
      DO j=i+1,nxc(ic)
         IF(ixc(i,ic) > ixc(j,ic))THEN
            iswp=ixc(i,ic)
            ixc(i,ic)=ixc(j,ic)
            ixc(j,ic)=iswp
         END IF
      END DO
   END DO
END DO
WRITE(77)ixc

CALL cla_write_results()


END PROGRAM cleps_cluster_analysis
