#!/bin/bash
params="$@"

if [[ "$(podman images -q ic-owl 2> /dev/null)" == "" ]]; then
    podman build -t ic-owl . 
fi


echo "running with arguments $params"

if [ $# -eq 0  ]; 
then 
    podman run -v $(pwd):/ic-owl:z -i ic-owl
else 
    podman run --entrypoint /ic-owl/run.sh -v $(pwd):/ic-owl:z -i ic-owl $params; fi
