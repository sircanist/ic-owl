# Arguments

## command arguments no weaken all

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario1.owl owls/cti2-scenario1-bk.owl owls/cti2-scenario1-policy.owl BFS noweaken -1 owls/cti2-iri-mapper.txt
````

## command arguments weaken all

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario1.owl owls/cti2-scenario1-bk.owl owls/cti2-scenario1-policy.owl BFS weaken -1 owls/cti2-iri-mapper.txt
````


## command arguments no weaken one

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario1.owl owls/cti2-scenario1-bk.owl owls/cti2-scenario1-policy.owl BFS noweaken 1 owls/cti2-iri-mapper.txt
````

## command arguments weaken one

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario1.owl owls/cti2-scenario1-bk.owl owls/cti2-scenario1-policy.owl BFS weaken 1 owls/cti2-iri-mapper.txt
````

# Log

````
min_length 4
max_length 10

noweaken all times - iterations 885
107.951837193
111.835344037
107.525445556

weaken all times - iterations 1007
293.109306936
306.571869709
316.731901194

noweaken one time
9.523882317
9.56553991
9.735661391

weaken one time
18.728927052
15.644457681
16.502777304
````

# Answers

Können Reparaturen gefunden werden? / ja
Welche minimale und maximale Reparaturen wurden bei der Ausführung ohne Abschwächungen in Bezug auf die Modifikationsschritte gefunden und wie sehen diese aus? / 4 / 10
Welche minimale und maximale Reparaturen wurden bei der Ausführung mit Abschwächungen in Bezug auf die Modifikationsschritte gefunden und wie sehen diese aus? / 4 / 10
Wie viele Reparaturen können ohne Abschwächungen gefunden werden? / 240
Wie viele Reparaturen können mit Abschwächungen gefunden werden? / 288
Wie lange dauert die Suche nach einer Reparatur ohne Abschwächungen?
Wie lange dauert die Suche nach einer Reparatur mit Abschwächungen?
Wie lange dauert die Suche nach allen Reparaturen ohne Abschwächungen?
Wie lange dauert die Suche nach allen Reparaturen mit Abschwächungen?
