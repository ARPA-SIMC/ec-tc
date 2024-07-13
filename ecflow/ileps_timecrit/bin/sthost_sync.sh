#!/bin/bash

do_sync() {
    if touch $2/.testfile; then
        if [ -d "$1" ]; then
            echo "Syncing $1 to $2"
            for dir in $dirlist; do
                echo "Syncing $dir"
                rsync -aui $1/$dir $2
            done
        fi
        rm -f touch $2/.testfile
    fi
}

dirlist="bin const $@"
ws1=/ec/ws1/tc/$USER/tcwork
ws2=/ec/ws2/tc/$USER/tcwork
do_sync $ws1 $ws2
do_sync $ws2 $ws1
