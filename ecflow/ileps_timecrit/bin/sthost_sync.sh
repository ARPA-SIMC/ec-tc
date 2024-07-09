#!/bin/bash

dirlist="bin const"
ws1=/ec/ws1/tc/$USER/tcwork
ws2=/ec/ws2/tc/$USER/tcwork
echo "Syncing ws1 to ws2"
for dir in $dirlist "$@"; do
    echo "Syncing $dir"
    rsync -aui $ws1/$dir $ws2
    rsync -aui $ws2/$dir $ws1
done
