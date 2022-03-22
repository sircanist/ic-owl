# Arguments

## command arguments no weaken all

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS noweaken -1 owls/cti2-iri-mapper.txt
````

## command arguments weaken all

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS weaken -1 owls/cti2-iri-mapper.txt
````

## command arguments no weaken 5

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS noweaken 5 owls/cti2-iri-mapper.txt
````

## command arguments weaken 5

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS weaken 5 owls/cti2-iri-mapper.txt
````


## command arguments no weaken one

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS noweaken 1 owls/cti2-iri-mapper.txt
````

## command arguments weaken one

````
java -Xms25000M -Xmx25000M -jar ic-owl.jar owls/cti2.owl owls/cti2-scenario2.owl owls/cti2-scenario2-bk.owl owls/cti2-scenario2-policy.owl BFS weaken 1 owls/cti2-iri-mapper.txt
````

# Log

````
min_length 4
max_length 10

noweaken all times - iterations 885

weaken all times - iterations 1007

noweaken one time
7.266808279
3.845418561
7.235264719

weaken one time
7.673925942
13.595528291
13.772856008
7.637032072


noweaken five time - iterations 96
54.934198577
64.546397488
58.788538109

weaken five time - iterations 104
97.244731538
91.430237332
75.471436379

````

# Answers

Können Reparaturen gefunden werden? / ja
Welche minimale und maximale Reparaturen wurden bei der Ausführung ohne Abschwächungen in Bezug auf die Modifikationsschritte gefunden und wie sehen diese aus? / 1 / 5
Welche minimale und maximale Reparaturen wurden bei der Ausführung mit Abschwächungen in Bezug auf die Modifikationsschritte gefunden und wie sehen diese aus? / 1 / 5
Wie viele Reparaturen können ohne Abschwächungen gefunden werden? / 240
Wie viele Reparaturen können mit Abschwächungen gefunden werden? / 288
Wie lange dauert die Suche nach einer Reparatur ohne Abschwächungen?
Wie lange dauert die Suche nach einer Reparatur mit Abschwächungen?
Wie lange dauert die Suche nach allen Reparaturen ohne Abschwächungen?
Wie lange dauert die Suche nach allen Reparaturen mit Abschwächungen?
