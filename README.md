# IC-OWL

This application generates repairs fro CTI-ontologies by using the Hitting-Set-Algorithmus.

# Setup

- Ensure that the submodule is cloned and checked out.


## Execution


## Demo

- Execute docker.sh and wait until the interactive console is shown
- You can choose from two demo scenarios by pressing 1 or 2 and then Enter.

## Repair your own files

- Place the owl files somewhere reachable from ic-owl (i.e. subfolder)
- Execute docker.sh with the following arguments

`docker.sh [relative-path-tbox] [relative-path-cti-instance] [relative-path-background-knowledge] [relative-path-policy] [DFS|BFS] [amount-of-repairs] [optional-iri-mapping]`

The arguments described:

- relative-path-tbox: The relative path to the OWL file describing the concepts and relationships
- relative-path-cti-instance: The relative path to the cti instance, which should be repaired
- relative-path-background-knowedge: The relative path to the background knowledge file, which describes concepts known to a possible threat actor
- relative-path-policy: The relative path to the policy
- DFS|BFS: Search-strategy Breath-First-Search or Depth-First-Search
- Amount-Of-Repairs: The amount of repair files to generate
- optional-iri-mapping: Optional relative path to a IRI-mapping-file, which maps IRIs to files in form for each line "iri filepath"

The policies must be defined as individuals only, where the concept-descriptions from the individuals are taken as policies (the naming of the individuals does not matter).

## Output

The generated repairs can be found in the repair folder.



