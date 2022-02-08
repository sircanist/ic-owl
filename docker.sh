#!/bin/bash
params="$@"
if [[ "$(docker images -q ic-owl 2> /dev/null)" == "" ]]; then
    docker build -t ic-owl . 
fi


echo "running with arguments $params"

if [ $# -eq 0  ]; 
then 
    docker run -v $(pwd):/ic-owl -i ic-owl
else 
    docker run --entrypoint /ic-owl/run.sh -v $(pwd):/ic-owl -i ic-owl $params; fi
