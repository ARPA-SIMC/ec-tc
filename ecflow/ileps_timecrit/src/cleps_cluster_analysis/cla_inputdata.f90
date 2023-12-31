MODULE cla_inputdata
USE eccodes
IMPLICIT NONE


INTEGER,PARAMETER :: nstepmax=10
INTEGER :: levlist(3)=(/850, 700, 500/), varlist(4)=(/129, 131, 132, 133/)

INTEGER :: nclust=-1, nens=-1, nmember=-1, nstep=-1, steps(nstepmax), clmethod=2, rmmethod=3, &
 lag, gridsize
CHARACTER(len=512) :: outputdir='', input_ctrl, input_pert
CHARACTER(len=14) :: reftime_leps, reftime
TYPE ensrun_t
  CHARACTER(len=14) :: reftime
  INTEGER :: lag
END TYPE ensrun_t
TYPE(ensrun_t),ALLOCATABLE :: ensrun(:)

INTEGER :: ipoints, jpoints
REAL :: lat_first, delta_j
REAL,ALLOCATABLE,TARGET :: x(:,:,:,:,:,:), x2t(:,:), w(:)
REAL,ALLOCATABLE :: buff(:)
REAL,ALLOCATABLE :: dx(:,:), var(:)
INTEGER,ALLOCATABLE :: ixc(:,:), nxc(:), irc(:)

NAMELIST /setup/ reftime_leps, nclust, nens, nmember, nstep, outputdir, clmethod, rmmethod
NAMELIST /ensemble/ reftime, input_ctrl, input_pert, steps, lag

CONTAINS

SUBROUTINE cla_read_data(namlfile)
CHARACTER(len=*),INTENT(in) :: namlfile

INTEGER :: i, fid, gid

OPEN(10, file=namlfile, form='FORMATTED', status='OLD')
READ(10, setup)

ALLOCATE(ensrun(nens))
DO i = 1, nens
  WRITE(*,'(A,I0,A,I0)')'Reading ensemble ',i,' of ',nens
  READ(10, ensemble)
  ensrun(i)%reftime = reftime
  ensrun(i)%lag = lag

  IF (i == 1) THEN
    CALL codes_open_file(fid, TRIM(input_ctrl), 'r')
    CALL codes_grib_new_from_file(fid, gid)
! get once some general information about the dataset
    CALL codes_get(gid, 'numberOfPointsAlongAParallel', ipoints)
    CALL codes_get(gid, 'numberOfPointsAlongAMeridian', jpoints)
    CALL codes_get_size(gid, 'values', gridsize)
    CALL codes_get(gid, 'latitudeOfFirstGridPointInDegrees', lat_first)
    CALL codes_get(gid, 'jDirectionIncrementInDegrees', delta_j)
    CALL cla_alloc()
    CALL codes_release(gid)
    CALL codes_close_file(fid)
  ENDIF

  CALL cla_read_file(TRIM(input_ctrl), i, .TRUE.)
  CALL cla_read_file(TRIM(input_pert), i, .FALSE.)
END DO
CLOSE(10)

END SUBROUTINE cla_read_data


SUBROUTINE cla_alloc()

!ALLOCATE(x(nens*nmember*SIZE(varlist)*SIZE(levlist)*nstep*gridsize))
ALLOCATE(x(gridsize,nstep,SIZE(levlist),SIZE(varlist),nmember,nens))
ALLOCATE(w(gridsize*nstep*SIZE(levlist)*SIZE(varlist)))
ALLOCATE(buff(gridsize))
! after all the checks, irc should be dimensioned (nclust)
ALLOCATE(dx(nmember*nens,nmember*nens), ixc(nmember*nens,nmember*nens), &
 nxc(nmember*nens), irc(nclust))
ALLOCATE(var(nclust+1))

END SUBROUTINE cla_alloc


SUBROUTINE cla_read_file(name, ensn, ctrl)
CHARACTER(len=*),INTENT(in) :: name
INTEGER,INTENT(in) :: ensn
LOGICAL,INTENT(in) :: ctrl

INTEGER :: i, fid, gid, stat, offs
INTEGER :: paramid, level, step, memb, lsize, lnvar, lnlev, lnstep

CALL codes_open_file(fid, name, 'r')

DO WHILE(.TRUE.)
  CALL codes_grib_new_from_file(fid, gid, stat)
  IF (stat /= CODES_SUCCESS) EXIT
  
  CALL codes_get(gid, 'paramId', paramid)
  CALL codes_get(gid, 'level', level)
  CALL codes_get(gid, 'step', step)
  IF (ctrl) THEN
    memb = 0
  ELSE
    CALL codes_get(gid, 'perturbationNumber', memb)
  ENDIF
  CALL codes_get_size(gid ,'values', lsize)
  IF (lsize /= gridsize) EXIT ! error here
  
  lnvar = firsttrue(paramid == varlist)
  lnlev = firsttrue(level == levlist)
  lnstep = firsttrue(step == steps(1:nstep))

  IF (lnvar <= 0 .OR. lnlev <= 0 .OR. lnstep <=0 .OR. memb < 0 .OR. memb > nmember-1) EXIT ! message here

  CALL codes_get(gid ,'values', buff)
  x(:,lnstep,lnlev,lnvar,memb+1,ensn) = buff(:) ! can we do it without copy?
  CALL codes_release(gid)
ENDDO

CALL codes_close_file(fid)

END SUBROUTINE cla_read_file


SUBROUTINE cla_write_results()
INTEGER :: i, lnens, lnmemb
CHARACTER(len=512) :: outfile


i = LEN_TRIM(outputdir)
IF (i > 0) THEN
  IF (outputdir(i:i) /= '/') THEN
    outputdir(i+1:i+1) = '/'
  ENDIF
ENDIF

DO i = 1, nclust
  outfile = ''
  WRITE(outfile,'(a,''clust_ana_result_'',a,''.'',i0,''.sh'')') &
   TRIM(outputdir),TRIM(reftime_leps),i
! nmember = 51
! irc = 1 (control), 2 (member 1), ... 51 (member 50)
  lnens = (irc(i)-1)/nmember + 1
  lnmemb = MOD(irc(i)-1, nmember)

  OPEN(10, file=outfile)
  WRITE(10,'(''refdate='',a)')ensrun(lnens)%reftime(1:8)
  WRITE(10,'(''reftime='',a)')TRIM(ensrun(lnens)%reftime(9:))
  WRITE(10,'(''lag='',i0)')ensrun(lnens)%lag
  WRITE(10,'(''elem='',i0)')lnmemb
  CLOSE(10)
ENDDO

END SUBROUTINE cla_write_results


FUNCTION firsttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:) !< logical array to test
INTEGER :: i

DO i = 1, SIZE(v)
  IF (v(i)) RETURN
ENDDO
i = 0

END FUNCTION firsttrue

END MODULE cla_inputdata
