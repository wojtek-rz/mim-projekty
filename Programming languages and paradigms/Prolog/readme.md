Correctness of concurrent programs
==================================

This Prolog program verifies the correctness of concurrent programs, specifically checking if there is a possible interleaving of thread executions that leads to an unsafe state (e.g., multiple threads entering a critical section simultaneously). 

The program reads a concurrent program from a file, initializes the state with the given number of threads, and then explores possible execution paths using a breadth-first search (BFS) algorithm to find any unsafe interleaving.

Implementation details:
- State Representation: The program maintains the state of variables, arrays, and instruction pointers (IPs) for each thread.
- Breadth-First Search (BFS): The algorithm explores all possible interleavings of threads to find a sequence of steps that might lead to an unsafe state.
- Safety Check: The program ensures safety by checking if more than one thread is in a critical section.
- Instruction Execution: The program simulates the execution of instructions for each thread and evaluates arithmetic and boolean expressions.
- Difference lists: the queue operations are implemented using difference lists - strange technique in Prolog to efficiently manage lists, particularly when performing operations like adding elements to the end of a list

Usage
-----

Open prolog and load the program:
```
prolog wr438709.pl
```
or to open prolog type:
```
prolog
```
then to load the script:
```
consult('wr438709.pl')
```

The entry function of the script is `verify`:
```
verify(+N, +FileName) - Verifies if the concurrent program is correct.
% N - number of threads
% FileName - name of the file with the program
```

Example usage:
```
verify(2, 'peterson.txt')
```

If the program is safe (i.e., no unsafe interleaving exists), it will output:
```
Program jest poprawny (bezpieczny).
```
