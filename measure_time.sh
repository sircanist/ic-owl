#!/bin/bash
base_path="/tmp/outputs"
n=97 # sample size
[ -d $base_path ] && read -p "remove output dir (y/n)" && [[ ${REPLY} == "y" ]] && rm -rf "$base_path"

function ex(){
    folder="$base_path/$@"
    mkdir -p "$folder"
    output_file="$folder/timing.csv" # output file"
    echo "$@" >> "$output_file"
    for i in $(seq 1 $n);do
        /usr/bin/time -a -o "$output_file" -f "%e" java -jar ic-owl.jar owls/cti2.owl "owls/cti2-$1.owl" "owls/cti2-$1-bk".owl owls/cti2-"$1"-policy.owl BFS "$2" "$3" owls/cti2-iri-mapper.txt
    done
}

#ex scenario1 noweaken 1
#ex scenario1 weaken 1

#ex scenario1 noweaken -1
#ex scenario1 weaken -1

#ex scenario2 noweaken 5
#ex scenario2 weaken 5

#ex scenario2 noweaken 1
#ex scenario2 weaken 1
