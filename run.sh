#!/bin/bash
params="$@"
cd "$(dirname "$0")"
mkdir -p repair
mvn compile && mvn exec:java -Dexec.mainClass=edu.hagenberg.CLI -DskipTests -Dexec.args="$params"
