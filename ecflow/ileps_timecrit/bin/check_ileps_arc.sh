#!/bin/bash
root_dir=/ec/ws1/tc/zcl2/tcwork/arc

echo "Count files"
for dir in input/DWD input/IFS output/DWD output/EFAS output/surf output/ff proc_ff log ; do
  nf=$(find -L ${root_dir}/${dir} -type f |wc -l)
  echo $dir": "$nf
done
