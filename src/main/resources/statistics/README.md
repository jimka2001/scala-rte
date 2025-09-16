# this directory stores statistics for measuring various things

# balanced.csv

The format of the `balanced.csv` file is
```
depth, node-count, state-count, transition-count, probability-binary
```
Each line of the csv is the result of a call to `randomTotallyBalancedRte(probability_binary, depth)`.
An Rte of a requested depth might have a range of node-count, (counting the internal and leaf nodes
of the Rte as AST).
This  call produces an Rte, we then convert the Rte to a Dfa and count the number of states and transitions.
