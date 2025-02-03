function main(args)
*
* Legge un grib con un singolo campo (rRMS difference mensile) e disegna una mappa
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
  clevs="0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4"
  fmul=0.01
  fsum=0.
endif
if (var="prmslmsl")                  
  clevs="0 0.2 0.4 0.6 0.8 1.0 1.2"
  fmul=0.01
  fsum=0.
endif
if (var="rh2m")                  
  clevs="2 4 6 8 10 12"
  fnorm=1.
  fsum=0.
endif
if (var="tmp2m")                  
  clevs="0.3 0.6 0.9 1.2 1.5 1.8 2.1 2.4"
  fnorm=1.
  fsum=0.
endif
if (var="tpratesfc")                  
  clevs="2 5 10 20 50 100 200 500"
  fnorm=0.001
  fsum=0.
endif
if (var="wind10m")                  
  clevs="0.25 0.5 0.75 1. 1.25 1.5 1.75 2."
  fnorm=1.
  fsum=0.
endif


'set clevs 'clevs
'd 'var'*'fmul'+'fsum
'cbarn_white'
'draw title 'label
'save_png 'label' med'
quit



