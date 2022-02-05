#mvn package exec:java -Dexec.mainClass=edu.hagenberg.CLI "$@"
params="$@"
mvn exec:java -Dexec.mainClass=edu.hagenberg.CLI -DskipTests -Dexec.args="$params"
