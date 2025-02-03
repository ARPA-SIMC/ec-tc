#!/bin/bash
#
# Rinomina i files di un esperimeto NWP-TS archiviati su ecfs
# Script semi-automatico, da modificare a mano ogni volta, usato in genere in caso di errori.
# I files all'interno dell'archivio tar (log e output su griglia regolare) non vengono rinominati
#
set -x
exp_old=S24icon202410
exp_new=S24yicon202410
dir_root=ec:/mck/nwp-ts/out/S24yicon202410

for mm in 07 12 ; do
  for dd in $(seq -f "%02g" 1 31) ; do
#    emove ${dir_root}/2021${mm}/ITS_${exp_old}_2021${mm}${dd}_basic.grb ${dir_root}/2021${mm}/ITS_${exp_new}_2021${mm}${dd}_basic.grb
#    emove ${dir_root}/2021${mm}/ITS_${exp_old}_2021${mm}${dd}_extra.grb ${dir_root}/2021${mm}/ITS_${exp_new}_2021${mm}${dd}_extr.grb
    emove ${dir_root}/2021${mm}/ITS_${exp_old}_2021${mm}${dd}_str.tar ${dir_root}/2021${mm}/ITS_${exp_new}_2021${mm}${dd}_str.tar
  done
#  for dd in $(seq -f "%02g" 1 5 31) ; do
#    emove ${dir_root}/2021${mm}/${exp_old}_2021${mm}${dd}_log.tar.gz ${dir_root}/2021${mm}/${exp_new}_2021${mm}${dd}_log.tar.gz
#  done
done
