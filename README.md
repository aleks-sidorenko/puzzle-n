# Puzzle-n
## Intro
Solves generic puzzle-n problem with dimensions: [2, 5].

The most known dimension is 4: https://en.wikipedia.org/wiki/15_puzzle



## Approach
Used greedy algorithm which tries to find paths to goal board by taking moves which lead boards with less distance to goal board and with less amount of moves done before.

Some initial boards are not solvable, so we check this in code. 

## How to run
There is input folder `./input`, so to run app using predefined test case use:
```
cat input/case5-1 | sbt run
```

## How to test
The tests are implemented using property-based approach, to run them:
```
sbt test
```
