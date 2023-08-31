!
!!!  per compilaz:
!!! module load eccodes
!!!gfortran  -fbounds-check   *.f90  ${ECCODES_INCLUDE} ${ECCODES_LIB}   -o f90_eccodes.exe
!!! ./f90_eccodes.exe

! Code converted using TO_F90 by Alan Miller
! Date: 2012-10-02  Time: 15:00:36

! for compilation:
! xlf  -O3 -qstrict -qarch=auto -qtune=auto -qnohot  *.f  $GRIB_API_INCLUDE $GRIB_API_LIB -o ~/0001/cluster_00/grib_api/test_clst_gribapi.ex
!cc  -o aaa.exe


PROGRAM main

use eccodes
implicit none

! Imported Array Variables with intent (in):

!      include      'gribio.inc'

INTEGER :: ngp, nvar, nlev, nstep, nstepmax, nem, nens, ngrib, ndim, nclmax, method, mode
INTEGER :: ncl, iens, igp
INTEGER :: ilev, jlev, klev, istep, jstep, kstep, ivar, jvar, kvar
INTEGER :: iug, igrib1, iret1, mgrib, numberOfValues, lag, iem, i, j, ic, igrib, iswp, indt, ind
INTEGER :: oparamId, ostep, olevel, omember, ipoints, jpoints, delta_j
INTEGER :: ipippo, iiimax, iline

! Local parameters:  30351
PARAMETER    ( ngp    = 30351 , nvar   =    4  &
    , nlev   =    3 , nstep  =    2  &
    , nstepmax  =    2 , nem    =   51  &
    , nens   =    2 , ngrib  =  nvar*nlev*nstepmax  &
    , ndim   =  ngp*nvar*nlev*nstepmax*nem*nens  &
    , nclmax =    51 , method =    2  &
    , mode   =    3 )

! Local scalars:
CHARACTER (LEN=90) :: ctrlfile
CHARACTER (LEN=90) :: FILE
CHARACTER (LEN=90) :: pertfile
CHARACTER (LEN=90) :: chmeth (2)
CHARACTER (LEN=90) :: chmode (3)

DATA         chmeth /" WARD'S ALGORITHM", ' COMPLETE LINKAGE'/
DATA         chmode /' MINIMIZE INTERNAL DISTANCE',  &
    ' MAXIMIZE EXTERNAL DISTANCE', ' MINIMIZE INT/EXT RATIO    '/

! Local arrays:
INTEGER :: iyear  ( nens )
INTEGER :: imonth ( nens )
INTEGER :: iday   ( nens )
INTEGER :: itime  ( nens )
INTEGER :: ilead  ( nens )

INTEGER :: lvar   ( nvar )
INTEGER :: llev   ( nlev )
INTEGER :: lstep  ( nstep, nens )

DATA         llev   / 850, 700, 500 /
DATA         lvar   / 129, 131, 132, 133 /

INTEGER :: ixc    ( nem*nens, nem*nens )
INTEGER :: nxc    ( nem*nens )
INTEGER :: irc    ( nem*nens )


REAL, dimension(:), allocatable    ::  values
REAL, dimension(:), allocatable    ::  x,xt,z
!!!REAL :: x      ( ndim )
!!!REAL :: xt     ( ndim )
!!!REAL :: z      ( ndim )
REAL :: w      ( ngp*ngrib )
REAL :: dx     ( nem*nens, nem*nens )
REAL :: var    ( nclmax+1 )
REAL :: r0, rd, wt, rlat_first,rlat_end,rlon_first,rlon_end,varexp


!----------------------------------------------------------------------
!  [1.0] Get ensemble forecasts
!----------------------------------------------------------------------
allocate (x(ndim),xt(ndim),z(ndim))

! read parameters
!!!!OPEN(99,STATUS="old")

!cc first of all, read number of clusters
READ(99,*) ncl
IF (ncl <= 0.OR.ncl > nclmax) THEN
   PRINT *, 'erroraccio su NCL!'
   GO TO 9100
END IF
! then, read the ensemble info
DO iens=1,nens
   READ(99,'(a)')ctrlfile
   READ(99,'(a)')pertfile
!cc
!cc  modifications so that the program runs whatever the path is
!cc
   FILE=ctrlfile
   DO WHILE (INDEX(FILE,'/') /= 0)
      FILE=FILE(INDEX(FILE,'/')+1:)
   END DO
  
   READ(FILE,'(3X,i4,i2,i2,i2)') iyear(iens)  &
        ,imonth(iens) ,iday(iens)  &
        ,itime(iens)
  
   DO istep=1,nstep
      READ(99,'(i3)')lstep(istep,iens)
   END DO
   READ(99,'(i2)')ilead(iens)
  
! set the number of levels, time steps, parameters
   klev=0
   DO ilev=1,nlev
      IF(llev(ilev) /= -1)klev=klev+1
   END DO
   kstep=0
   DO istep=1,nstep
      IF(lstep(istep,iens) /= -1)kstep=kstep+1
   END DO
   kvar=0
   DO ivar=1,nvar
      IF(lvar(ivar) /= -1)kvar=kvar+1
   END DO
   mgrib=kstep*klev*kvar
   IF(mgrib == 0)GO TO 500
  
! read ensemble **************** (control forecasts) *************
  
   WRITE (*,*) 'open ctrlfile of ens', iens
   CALL codes_open_file(iug, ctrlfile,'r')
!          a new grib message is loaded from file
!          iug is the grib id to be used in subsequent calls
   DO igrib=1,ngrib
      CALL codes_grib_new_from_file(iug,igrib1, iret1)
      CALL codes_get(igrib1,'paramId',oparamid)
      CALL codes_get(igrib1,'level',olevel)
      CALL codes_get(igrib1,'step',ostep)
! get also info about the grid, lat and lon (will be used later on)
      CALL codes_get(igrib1, 'latitudeOfFirstGridPointInDegrees' , rlat_first)
      CALL codes_get(igrib1, 'latitudeOfLastGridPointInDegrees' , rlat_end  )
      CALL codes_get(igrib1, 'longitudeOfFirstGridPointInDegrees' , rlon_first)
      CALL codes_get(igrib1, 'longitudeOfLastGridPointInDegrees'  , rlon_end  )
      CALL codes_get(igrib1, 'jDirectionIncrement' , delta_j)
      CALL codes_get(igrib1, 'numberOfPointsAlongAMeridian' , jpoints)
      CALL codes_get(igrib1, 'numberOfPointsAlongAParallel' , ipoints)
!!!            write (*,*)'rlat_first et al', int_rlat_first,
!!!     .           int_rlat_end, int_delta_j, int_jpoints, int_ipoints
      CALL codes_get_size(igrib1,'values',numberOfvalues) ! get array size
!!!      write(*,*) 'numberOfValues=',int_numberOfValues
      allocate(values(numberOfValues), stat=iret1)
      CALL codes_get(igrib1,'values',values) ! get array data values
      DO ivar=1,nvar
         IF(lvar(ivar) == oparamid)THEN
            jvar=ivar
            DO ilev=1,nlev
               IF(llev(ilev) == olevel)THEN
                  jlev=ilev
                  DO istep=1,nstep
!cc                         write (*,*) 'loop on NSTEP', istep,ilev,ivar
                     IF(lstep(istep,iens) == ostep)THEN
                        jstep=istep
                        GO TO 100
                     END IF
                  END DO
                  GO TO 200
               END IF
            END DO
            GO TO 200
         END IF
      END DO
      GO TO 200
100   CONTINUE
      lag=(jstep-1)*numberOfValues +(jlev-1)*kstep*numberOfValues  &
           +(jvar-1)*klev*kstep*numberOfValues +(iens-1)*nem*kvar*klev*kstep*numberOfValues
      DO igp=1,numberOfValues
         x(igp+lag)=values(igp)
      END DO
!cc            write (*,*) 'test of values', x(100), x(2991+100)
200   CONTINUE
   END DO
   CALL codes_release(igrib1)
   CALL codes_close_file(iug) !!! close ctrlfile
   deallocate(values)
   WRITE (*,*) 'end of read ctrlfile of ens', iens
  
! read ensemble (perturbed forecasts) ********* (perturbed forecasts) *********
   
   WRITE (*,*) 'open pertfile of ens', iens
  
   CALL codes_open_file(iug, pertfile,'r')
  
   DO iem=2,nem
      DO igrib=1,ngrib
         CALL codes_grib_new_from_file(iug,igrib1, iret1)
         CALL codes_get(igrib1,'paramId',oparamid)
         CALL codes_get(igrib1,'level',olevel)
         CALL codes_get(igrib1,'step',ostep)
         CALL codes_get(igrib1,'perturbationNumber',omember)
         CALL codes_get_size(igrib1,'values',numberOfValues)
         allocate(values(numberOfValues), stat=iret1)
         CALL codes_get(igrib1,'values',values)
         DO ivar=1,nvar
            IF(lvar(ivar) == oparamid)THEN
               jvar=ivar
               DO ilev=1,nlev
                  IF(llev(ilev) == olevel)THEN
                     jlev=ilev
                     DO istep=1,nstep
                        IF(lstep(istep,iens) == ostep)THEN
                           jstep=istep
                           GO TO 300
                        END IF
                     END DO
                     GO TO 400
                  END IF
               END DO
               GO TO 400
            END IF
         END DO
         GO TO 400
300      CONTINUE
         lag=(jstep-1)*numberOfValues +(jlev-1)*kstep*numberOfValues  &
              +(jvar-1)*klev*kstep*numberOfValues +omember*kvar*klev*kstep*numberOfValues  &
              +(iens-1)*nem*kvar*klev*kstep*numberOfValues
         DO igp=1,numberOfValues
            x(igp+lag)=values(igp)
         END DO
400      CONTINUE
      END DO
   END DO
   CALL codes_release(igrib1)
   CALL codes_close_file(iug) !!! close pertlfile
   deallocate(values)
   WRITE (*,*) 'end of read pertfile of ens', iens
END DO
!!!!!CLOSE(99)

WRITE (*,*) ' I have read all grib files!, nvalues= ', numberOfValues

!----------------------------------------------------------------------
! [2.0] Standardisation of variables
!----------------------------------------------------------------------
!
DO ivar=1,kvar
   DO ilev=1,klev
      DO istep=1,kstep
         DO iem=1,nem*nens
            lag=(istep-1)*numberOfValues +(ilev-1)*kstep*numberOfValues  &
                 +(ivar-1)*klev*kstep*numberOfValues +(iem-1)*kvar*klev*kstep*numberOfValues
            DO igp=1,numberOfValues
               xt(igp+(iem-1)*numberOfValues)=x(igp+lag)
            END DO
         END DO
         CALL stdv(numberOfValues*nem*nens,xt,z)
         DO iem=1,nem*nens
            lag=(istep-1)*numberOfValues +(ilev-1)*kstep*numberOfValues  &
                 +(ivar-1)*klev*kstep*numberOfValues +(iem-1)*kvar*klev*kstep*numberOfValues
            DO igp=1,numberOfValues
               x(igp+lag)=z(igp+(iem-1)*numberOfValues)
            END DO
         END DO
      END DO
   END DO
END DO


!----------------------------------------------------------------------
!  [3.0] Cluster analysis
!----------------------------------------------------------------------
WRITE (*,*) 'clst ana'
WRITE (*,*) 'rlat and delta ', rlat_first, delta_j
!!! ksec2(2)  points along a parallel   ipoints
!!! ksec2(3)  point along meridian      jpoints
!!! ksec2(4)  latitude  1st grid point   rlat_first
!!! ksec2(5)  longitude 1st grid point   rlon_first
!!! ksec2(7)  latitude  last grid point  rlat_end
!!! ksec2(8)  longitude last grid point  rlon_end
!!! ksec2(10) j direction increment     delta_j
!
! compute meridional weights
r0=REAL(rlat_first)*(ASIN(1.)/90.)
rd=REAL(delta_j)/1000.*(ASIN(1.)/90.)
write(*,*)'checks on r0,rd and ksec, f90, grib_api '  &
     , r0, rd, ipoints,jpoints,rlat_first*1000., delta_j
!r0=REAL(ksec2( 4))/1000.*(ASIN(1.)/90.)
!rd=REAL(ksec2(10))/1000.*(ASIN(1.)/90.)
wt=0.
DO j=1,jpoints
   DO i=1,ipoints
      igp=i+(j-1)*ipoints
      w(igp)=COS(r0-rd*(j-1))
   END DO
   wt=wt+COS(r0-rd*(j-1))
END DO
!!!write (*,*) 'ciao1 f90, grib_api, wt,w(100) ',wt, w(100)
DO j=1,jpoints
   DO i=1,ipoints
      igp=i+(j-1)*ipoints
      w(igp)=w(igp)/wt
   END DO
END DO
DO igrib=2,mgrib
   lag=(igrib-1)*numberOfValues
   DO igp=1,numberOfValues
      w(igp+lag)=w(igp)
   END DO
END DO
!!!write (*,*) 'ciao2 f90, grib_api, wt,w(100) ',wt, w(100)

! transpose of x
DO iem=1,nem*nens
   DO igp=1,numberOfValues*mgrib
      ind=igp+(iem-1)*numberOfValues*mgrib
      indt=iem+(igp-1)*nem*nens
      xt(indt)=x(ind)
   END DO
END DO
!!!write (*,*) 'ciao3 f77, grib_api, xt(100) ' , xt(100)

! hierarchical routine of clustering
IF(method == 1)THEN
   CALL wards(nem*nens,numberOfValues*mgrib,xt,w,dx, ixc,nxc,ncl)
   DO iem=1,nem*nens
      DO igp=1,numberOfValues*mgrib
         ind=igp+(iem-1)*numberOfValues*mgrib
         indt=iem+(igp-1)*nem*nens
         xt(indt)=x(ind)
      END DO
   END DO
ELSE IF(method == 2)THEN
   CALL clink(nem*nens,numberOfValues*mgrib,x,w,dx, ixc,nxc,ncl)      !!! it was xt
END IF

! statistics of clusters
CALL cstat(nem*nens,numberOfValues*mgrib,xt,w, ixc,nxc,ncl,var)
!WRITE(88)var(1:ncl+1)
WRITE(81)xt
WRITE(82)w
WRITE(83)dx
WRITE(84)ixc
WRITE(85)nxc

! selection of representative members
CALL selrm(nem*nens,numberOfValues*mgrib,x,w,dx, ncl,ixc,nxc,irc,mode)

WRITE(86,*)irc

! sort of elements
DO ic=1,ncl
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

WRITE(87)ixc

deallocate (x,xt,z)
!----------------------------------------------------------------------
!  [4.0] Report of results
!----------------------------------------------------------------------

PRINT 1000, chmeth(method)
PRINT 1010, chmode(mode)

DO iens=1,nens
   PRINT 1015, iens
   PRINT 1020, iyear(iens),imonth(iens),iday(iens),itime(iens)
   PRINT 1030, (lstep(istep,iens),istep=1,nstep,nstep-1)
END DO

PRINT 1040, REAL(rlat_first),REAL(rlat_end)  &
     ,  REAL(rlon_first),REAL(rlon_end)
PRINT 1050, ncl
varexp=var(ncl+1)
DO ic=1,ncl
   varexp=varexp-var(ic)
END DO
PRINT 1060, 100.*varexp/var(ncl+1)
PRINT 1070, (ic,ic=1,ncl)
PRINT 1080, (nxc(ic),ic=1,ncl)
PRINT 1090, (100.*var(ic)/var(ncl+1),ic=1,ncl)
PRINT 1100, (SQRT(var(ic)/REAL(MAX(1,nxc(ic)))),ic=1,ncl)
DO ic=1,ncl
   PRINT 1110,ic,irc(ic)-1,(ixc(i,ic)-1,i=1,MIN(10,nxc(ic)))
   DO iline=2,1+(nxc(ic)-1)/10
      PRINT 1120, (ixc(i,ic)-1,i=1+(iline-1)*10,MIN(iline*10,nxc(ic)))
   END DO
END DO

PRINT 1130
DO ic=1,ncl
   ipippo=irc(ic)-1
   IF(ipippo <= 50) THEN
      ipippo=ipippo
      iens=1
   ELSE IF(ipippo > 50.AND.ipippo <= 101) THEN
      ipippo=ipippo-51
      iens=2
   ELSE IF(ipippo > 101) THEN
      ipippo=ipippo-102
      iens=3
   END IF
!cccc         iens=irc(ic)/NEM+1
   PRINT 1140,ic,nxc(ic),irc(ic)-1-(iens-1)*nem,iyear(iens)  &
        ,imonth(iens),iday(iens),itime(iens),ilead(iens)
  
! added by Andrea Montani (17/7/2002) to have output file (fort.90)
  
   WRITE (90,'(a5,I2,a8,I3, a6,i3,1X,i4,i2.2,i2.2,1X,i2.2, a6,i2.2)')  &
        ' Cl. ',ic,' pop: ',nxc(ic), ' RM: ',ipippo,  &
        iyear(iens),imonth(iens),iday(iens),itime(iens), ' lag: ',ilead(iens)
  
! added by Andrea Montani (12/5/2005) to have another output file
! with all the cluster details (fort.95)
  
! the format 93(1x,I3) is needed because 93 is the maximum possible
! cluster population with 10 clusters (one cluster with 93 elements,
! the other 9 clusters with 1 element so as to have in total
! 102 elements)
  
   iiimax=nxc(ic)
   WRITE (95,'(a5,I2,a8,I3 ,a6,i3,1X,i4,i2.2,i2.2,1X,i2.2 ,a6,i2.2,a8,93(1X,i3))') &
         ' Cl. '  ,ic,' pop: ',nxc(ic),  &
         ' RM: '  ,ipippo ,iyear(iens),imonth(iens),iday(iens),itime(iens),  &
         ' lag: ',ilead(iens) ,' elem: ',(ixc(i,ic)-1,i=1,iiimax)
END DO


1000 FORMAT (/' Clustering method  ----->',2X,a17)
1010 FORMAT (' Selection mode  -------->',2X,a27)
1015 FORMAT (' Ensemble  -------------->',6X,i2)
1020 FORMAT (' Initial Date  ---------->',4X,i4,3(1X,i2),' UTC')
1030 FORMAT (' Forecast range (hours) ->',5X,i3,' - ',i3)
1040 FORMAT (' Area Limits (N/S/W/E) -->',4(3X,f5.1))
1050 FORMAT (' Number of clusters  ---->',5X,i3)
1060 FORMAT (' Explained Variance(%)  ->',3X,f5.1)
1070 FORMAT (' Cluster  --------------->',10(2X,i6))
1080 FORMAT (' Size  ------------------>',10(2X,i6))
1090 FORMAT (' Internal variance(%)  -->',10(3X,f5.1))
1100 FORMAT (' Radius ----------------->',10(1X,f7.1))
1110 FORMAT (/' CL ',i2,':',1X,'(',i3,')',10(1X,i3))
1120 FORMAT (13X,10(1X,i3))
1130 FORMAT (//7X,' Size   RM          Date Time     Lag'/)
1140 FORMAT (' CL ',i2,':',2X,i3,2X,i3,4X,i4,'/',i2.2,'/',i2.2, 3X,i2,6X,i2)


500 CONTINUE
STOP

9100 WRITE(6,*)'ERROR: Forecast has a different grib length'
STOP
END PROGRAM main
