#!/bin/bash

while inotifywait -e modify logs/status.txt; do
    if tail -n1 logs/status.txt | grep OK; then
        aplay sounds/oceanictrancer__click.wav
    fi
done
