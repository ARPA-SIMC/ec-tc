PROGRAM grib_set_tranges
!--------------------------------------------------------------------------
! Legge un file con molti grib, e lo riscrive modificando i timerange
! programma specifico per correggere sospetto bug di Icon 2024-10:
! al restart, i campi eleborati hanno forecastTime e lenghtOfTimeRange
! a partire da lrestart, ma i valori non sono azzerati, e restano relativi
! all'inizio del run
!--------------------------------------------------------------------------

USE grib_api
IMPLICIT NONE

INTEGER :: ifin=0,ifout=0,igin=0,igout=0
CHARACTER(LEN=200) :: filein,fileout,chdum

INTEGER :: idp,kp,iret,kg,ft_in,lotr_in,ft_out,lotr_out

!--------------------------------------------------------------------------
! 1) Preliminari

! 1.1 Parametri da riga comando
idp = 0
DO kp = 1,HUGE(0)
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE IF (TRIM(chdum) == "-h") THEN
    CALL write_help
    STOP 1
  ELSE 
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      filein = chdum
    CASE (2)
      fileout = chdum
    CASE DEFAULT
      CALL write_help
      STOP 1
    END SELECT
  ENDIF
ENDDO
IF (idp /= 2) THEN
  CALL write_help
  STOP 1
ENDIF
 
! Apro i files
CALL grib_open_file(ifin,filein,"r",iret)
IF (iret /= GRIB_SUCCESS) GOTO 9997
CALL grib_open_file(ifout,fileout,"w")

!--------------------------------------------------------------------------
! 2) Esecuzione (ciclo sui grib)

DO kg = 1,HUGE(0)

! 2.1 Leggo il prossimo campo
  igin = -1
  CALL grib_new_from_file(ifin,igin,iret)
  IF (iret == GRIB_END_OF_FILE) EXIT
  IF (iret /= GRIB_SUCCESS) GOTO 9996

  CALL grib_get(igin,"forecastTime",ft_in)
  CALL grib_get(igin,"lengthOfTimeRange",lotr_in)

  ft_out = 0
  lotr_out = lotr_in + ft_in
  CALL grib_clone(igin,igout)
  CALL grib_set(igout,"forecastTime",ft_out)
  CALL grib_set(igout,"lengthOfTimeRange",lotr_out)
  CALL grib_write (igout,ifout)

  CALL grib_release(igin)
  CALL grib_release(igout)

ENDDO

!--------------------------------------------------------------------------
! 3) Conclusione

WRITE (*,*) "Elaborazioni completate: campi scritti ",kg-1

CALL grib_close_file(ifin)
CALL grib_close_file(ifout)
STOP

!--------------------------------------------------------------------------
! 4) Gestione errori

9997 CONTINUE
WRITE (*,*) "Errore aprendo ",TRIM(filein)
STOP 2

9996 CONTINUE
WRITE (*,*) "Errore leggendo ",TRIM(filein)," grib n.ro " ,kg
STOP 2

END PROGRAM grib_set_tranges

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

SUBROUTINE write_help
! Scrive a schermo l'help del programma

!            123456789012345678901234567890123456789012345678901234567890123456789012345
WRITE (*,*) "Uso: grib_set_trange.exe [-h] filein fileout"
WRITE (*,*) "Riscrive timerange per correggere bug restart icon 2024-10"
!            123456789012345678901234567890123456789012345678901234567890123456789012345

RETURN
END SUBROUTINE write_help

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
