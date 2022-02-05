#!/bin/bash
params="$@"
cd "$(dirname "$0")"
mvn exec:java -Dexec.mainClass=edu.hagenberg.CLI -DskipTests -Dexec.args="$params"
