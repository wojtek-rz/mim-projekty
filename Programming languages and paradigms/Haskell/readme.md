Set and Graph Module
====================

Overview
--------
This Haskell project consists of two main modules: Set and Graph. These modules provide implementations for managing sets and creating/handling graphs respectively. The Set module defines a custom data type for sets and provides various operations on them, while the Graph module builds on the Set module to define and manipulate graph structures.

Set Module
----------
The Set module defines a custom set data structure using three constructors: `Empty`, `Singleton`, and `Union`. It provides various functions to work with sets, such as creating an empty set, checking if a set is empty, inserting elements, and performing set operations like `union`.

Graph Module
------------
The Graph module introduces two graph types: `Relation` and `Basic`. It implements a `Graph` typeclass to define basic graph operations, and instances of `Graph` for `Relation` and `Basic` types. The module also supports graph arithmetic by defining `Num` instances for graphs.

Usage
-----
```haskell
import Graph
import Set

-- Create a graph using Basic graph type
let g1 = vertex 1 `connect` vertex 2
let g2 = g1 `union` (vertex 2 `connect` (vertex 3 `union` vertex 4))
let g3 = g2 `connect` vertex 5 `union` vertex 17

-- Print the graph
print g3
-- Output: edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- Merge vertices 3 and 4 into 34
let g4 = mergeV 3 4 34 g3

-- Split vertex 34 back into 3 and 4
let g5 = splitV 34 3 4 g4

-- Convert graph to DOT format
putStrLn (todot g5)
-- Output: digraph {
-- 1 -> 2;
-- 2 -> 3;
-- 2 -> 4;
-- 3 -> 5;
-- 4 -> 5;
-- 17;
-- }
```