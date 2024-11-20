#!/bin/bash
#set -x

dest_proj=S24xicon202410
src_proj=S24icon202410
#seg_lst="2021120100 2021120600 2021121100 2021121600 2021122100 2021122600 2021123100"
seg_lst="2021120100"

dest_root=/ec/res4/scratch/mck/nwptsH/${dest_proj}/bc_ic_00
src_root=/ec/res4/scratch/mck/nwptsH/${src_proj}/bc_ic_00

for seg in $seg_lst ; do
  mkdir -p ${dest_root}/${seg}_bcic
  cd ${dest_root}/${seg}_bcic
  file_lst=$(find ${src_root}/${seg}_bcic -name 'ifs_oper_T1279_*.grb' -exec basename {} \;| sort)
  nok=0
  for file in $file_lst ; do
    ln -s ${src_root}/${seg}_bcic/${file} .
    if [ $? -eq 0 ] ; then
      nok=$(($nok+1))
    fi
  done
  echo "segmento $seg creati $nok links"
done

