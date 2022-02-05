#!/bin/bash

run_scenario1() {
    bash run.sh /owls/cti2.owl /owls/cti2-scenario1.owl /owls/cti2-scenario1-bk.owl /owls/cti2-scenario1-policy.owl /owls/cti2-iri-mapper.txt
}

run_scenario2() {
    bash run.sh /owls/cti2.owl /owls/cti2-scenario2.owl /owls/cti2-scenario2-bk.owl /owls/cti2-scenario2-policy.owl /owls/cti2-iri-mapper.txt
}

echo "Enter 1 for scenario 1, Enter 2 for scenario 2"

read ans
printf "\n"
if [ $ans -eq 1 ]
then
	run_scenario1
elif [ $ans -eq 2 ]
then
	run_scenario2
fi
