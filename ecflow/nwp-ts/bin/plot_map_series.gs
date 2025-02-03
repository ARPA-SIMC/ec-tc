function main(args)
*
* Legge un grib con i campi di un parametro, su diversi livelli per un solo istante
* oppure per diversi istanti su un solo livello
* Fa una mappa semplice per ogni campo
* uso: run plot_map_levs.gs filein label levs/times
*
file=subwrd(args,1)
label=subwrd(args,2)
index=subwrd(args,3)

'open 'file
'q file'
line=sublin(result,5)
nz=subwrd(line,9)
nt=subwrd(line,12)
line=sublin(result,7)
var=subwrd(line,1)

* Contour levels
if (var="rprs" | var="rhprs")
  clevs="10 20 30 40 50 60 70 80 90"
  clevst=clevs
endif
if (var="qprs" | var="spfhprs")
  clevs="0.0005 0.001 0.002 0.005 0.01 0.02"
  clevst=clevs
endif
if (var="tpratesfc" | var="cpratsfc" | var="apcpsfc" | var="tpsfc")
  clevs="2 5 10 25 50 75 100 150"
  clevst="10 25 50 75 100 150 250 400"
endif
if (var="dzdtprs" | var="wprs")
  clevs="-1.0 -0.5 -0.2 -0.1 0 0.1 0.2 0.5 1.0"
  clevst=clevs
endif

'white'
*'set mpdset hires'
*'set mpdset nil'
'c'
'set gxout shaded'

* Loop on levels
if (index="levs")
  iz=1
  while(iz<=nz)
    'c'
    'set z 'iz
    'q dims'
    line=sublin(result,4)
    level=subwrd(line,6)
    say level" "level

    if (var!="zprs")
      'set clevs 'clevs
    endif
    'd 'var
    'cbarn_white'
    'draw_shape regit'
    'draw title 'label' 'var' 'level
    'save_png 'label'_'var'_'level' med'
  
    iz=iz+1
  endwhile
endif

* Loop on times
if (index="times")
  it=1
  while(it<=nt)
    'c'
    'set t 'it
    'q dims'
    line=sublin(result,5)
    time=subwrd(line,6)
    say time" "time
  
    'set clevs 'clevs
    'd 'var
    'cbarn_white'
    'draw_shape regit'
    'draw title 'label' 'var' 'time
    'save_png 'label'_'var'_'time' med'
  
    it=it+1
  endwhile

* Final plot of cumulated or average
  'c'
  'set t 1'
  if (var="tpratesfc" | var="cpratsfc" | var="apcpsfc" | var="tpsfc")
    vtotal='sum('var',t=1,t='nt')'
    labelt="totalCumulated"
  else
    vtotal='ave('var',t=1,t='nt')'
    labelt="totalAverage"
  endif
  'set clevs 'clevst
  'd 'vtotal
  'cbarn_white'
  'draw_shape regit'
  'draw title 'label' 'var' 'labelt
  'save_png 'label'_'var'_'labelt' med'

endif

*return
quit
