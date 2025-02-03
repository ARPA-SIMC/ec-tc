function main(args)
*
* Legge un grib con un singolo campo (media/cumulata mensile) e disegna una mappa
*

file=subwrd(args,1)
label=subwrd(args,2)

'white'
'c'

'open 'file
'q file'
line=sublin(result,7)
var=subwrd(line,1)
'set gxout shaded'

* Contour levels
fmul=1.
fsum=0.

* TCC
if (var="tcdcsfc")                  
  clevs="0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9"
  fmul=0.01
  fsum=0.
endif
if (var="prmslmsl")                  
  clevs="1003 1006 1009 1012 1015 1018 1021 1024"
  fmul=0.01
  fsum=0.
endif
if (var="rh2m")                  
  clevs="10 20 30 40 50 60 70 80 90"
  fnorm=1.
  fsum=0.
endif
if (var="tmp2m")                  
  clevs="-5 0 5 10 15 20 25 30 35"
  fnorm=1.
  fsum=-273.15
endif
if (var="tpratesfc")                  
  clevs="5 10 20 50 100 200 500 1000 2000"
  fnorm=0.001
  fsum=0.
endif
if (var="wind10m")                  
  clevs="1 2 3 4 5 7.5 10"
  fnorm=1.
  fsum=0.
endif


'set clevs 'clevs
'd 'var'*'fmul'+'fsum
'cbarn_white'
'draw title 'label
'save_png 'label' med'
quit



