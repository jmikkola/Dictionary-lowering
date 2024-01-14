#!/usr/bin/env bash

inotifywait -r -e close_write,moved_to,create -m . | \
    while read -r directory events filename; do
        if echo "$filename" | grep -v "^\." | grep "\.py$" > /dev/null; then
            clear
            make test
        fi
    done
