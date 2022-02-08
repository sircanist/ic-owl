#!/bin/bash
params="$@"
cd "$(dirname "$0")"
mvn compile && mvn exec:java -Dexec.mainClass=edu.hagenberg.CLI -DskipTests -Dexec.args="$params"
