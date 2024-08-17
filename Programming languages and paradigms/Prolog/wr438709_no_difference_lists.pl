% Wojciech Rzepliński (438709)
% Zadanie zaliczeniowe z Prologu - Analizator prostych programów

:- ensure_loaded(library(lists)).

% Infix "not equals" operator for programs.
:- op(700, 'xfx', '<>').

% Verifies if the concurrent program is correct.
% verify(+N, +FileName)
% N - number of threads
% FileName - name of the file with the program
verify(N, FileName) :-
    checkPositive(N),
    readProgram(FileName, Program),
    initState(Program, N, InitState),
    findPath(Program, N, InitState, [], _, SolutionPath),
    writeSolution(Program, N, SolutionPath), !.

checkPositive(N) :-
    (
        N > 0, integer(N) ->
        !;
        format("Error: parametr ~w powinien byc liczba > 0\n", [N]),
        fail
    ).

% readProgram(+FileName, -Program)
% Read a program from a file.
% If the file does not exist or has an invalid format, 
% the predicate fails.
readProgram(FileName, Program) :-
    set_prolog_flag(fileerrors, off),
    open(FileName, read, Stream),
    !,
    (
        read(Stream, variables(Vars)),
        read(Stream, arrays(Arrs)),
        read(Stream, program(Stmts)),
        close(Stream),
        Program = program(Vars, Arrs, Stmts) ->
        true;
        write("Error: nieprawidlowy format pliku\n")
    ).

readProgram(FileName, _) :-
    format('Error: brak pliku o nazwie - ~p~n', [FileName]), fail.


% ========================= Output =========================

% Write output message.
writeSolution(_, _, none) :-
    write('Program jest poprawny (bezpieczny).\n').

writeSolution(Program, N, SolutionPath) :-
    write('Program jest niepoprawny.\n'),
    write('Niepoprawny przeplot:\n'),
    writeSolutionPath(Program, N, SolutionPath).

% Write the interleave saved in SolutionPath.
writeSolutionPath(Program, N, SolutionPath) :- 
    initState(Program, N, InitState),
    writeSolutionPath(Program, N, SolutionPath, InitState).

writeSolutionPath(Program, N, [], state(_, _, IPs)) :-
    write('Procesy w sekcji: '),
    writeThreadsInSection(Program, N, IPs, 0).

writeSolutionPath(Program, N, [Tid | Tids], State) :-
    state(_, _, IPs) = State,
    nth0(Tid, IPs, IP),
    format('   Proces ~w: ~w~n', [Tid, IP]),
    step(Program, State, Tid, NewState),
    writeSolutionPath(Program, N, Tids, NewState).

writeThreadsInSection(program(_, _, Instrs), N, [IP | IPs], Id) :-
    nth1(IP, Instrs, sekcja) ->
        write(Id),
        NewId is Id + 1,
        (NewId = N -> write('.\n'); write(', ')),
        writeThreadsInSection(program(_, _, Instrs), N, IPs, NewId);

        NewId is Id + 1,
        writeThreadsInSection(program(_, _, Instrs), N, IPs, NewId).

writeThreadsInSection(_, _, [], _).


% ========================= Path finding =========================

% findPath(Program, N, Path, SolutionPath)
% find a interleaving path of the program
findPath(Program, N, State, Visited, VisitedOut, PathOut) :-
    (ensureSafe(Program, State) -> % if fist state is safe
        queue_empty(Queue),
        queue_add(Queue, step(begin, none, State), NewQueue),
        findPathBfs(Program, N, Visited, NewQueue, VisitedOut, Goal),
        Goal \= none,
        getPathOut(VisitedOut, Goal, PathOutRev),
        reverse(PathOutRev, PathOut)
        ; % else return the empty solution
        VisitedOut = Visited,
        PathOut = []
    ).

findPath(_, _, _, _, _, none).

% getPathOut(+Visited, +GoalState, -PathOut)
% Given the visited states and the goal state, return the path
% from the initial state to the goal state.
% Visited states have the form `step(PrevState, ThreadId, CurrentState)`.
getPathOut(Visited, GoalState, [ThreadId | PathOut]) :-
    member(step(PrevState, ThreadId, GoalState), Visited),
    state(_, _, _) = PrevState,
    getPathOut(Visited, PrevState, PathOut).

getPathOut(_, _, []).

% addStepsToQueue(+Program, +ThreadId, +N, +State, +Queue, -QueueOut)
% Add steps for all threads to the queue from the current state.
addStepsToQueue(Program, ThreadId, N, State, Queue, QueueOut) :-
    ThreadId < N, !,
    step(Program, State, ThreadId, NewState),
    queue_add(Queue, step(State, ThreadId, NewState), NewQueue),
    NewThreadId is ThreadId + 1,
    addStepsToQueue(Program, NewThreadId, N, State, NewQueue, QueueOut).

addStepsToQueue(_, N, N, _, Queue, Queue).

findPathBfs(Program, N, Visited, Queue, VisitedOut, PathOut) :-
    (   is_queue_empty(Queue) -> % if queue is empty
        VisitedOut = Visited,
        PathOut = none % no solution found
        ; % else
        queue_pop(Queue, step(State, ThreadId, CurrentState), NewQueue),
        (   member(step(_, _, CurrentState), Visited) -> % if state was visited
            findPathBfs(Program, N,  Visited, NewQueue,  VisitedOut, PathOut) % skip
            ; % else
            (   ensureSafe(Program, CurrentState) -> % if state is safe
                addStepsToQueue(Program, 0, N, CurrentState, NewQueue, NewQueue2),
                    % add steps for all threads to the queue
                findPathBfs(
                    Program, N, 
                    [step(State, ThreadId, CurrentState) | Visited], 
                    NewQueue2, VisitedOut, PathOut) % continue
                ; % else
                VisitedOut = [step(State, ThreadId, CurrentState) | Visited],
                PathOut = CurrentState % return the solution
            )
        )
    ).

% ========================= Initial state =========================

% State representation:
% state(Vars, Arrs, IPs)
% Vars - list of variables values, each element is term `var(Name, Value)`
% Arrs - list of arrays values, each element is term `array(Name, ValuesList)`
% IPs - list of instruction pointers for each thread, each
%       element is a number representing the current instruction
%       counting from 1.

% initState(+Program, +N, -State)
initState(program(VariablesNames, ArraysNames, _), N, ResState) :-
    maplist(initVar, VariablesNames, Vars),
    maplist(initArray(N), ArraysNames, Arrs),
    initListWithValues(N, 1, InstrPointers),
    ResState = state(Vars, Arrs, InstrPointers).

initVar(Name, var(Name, 0)).

initArray(N, Name, array(Name, Arr)) :-
    initListWithValues(N, 0, Arr).

% ========================= Program execution =========================

% step(+Program, +State, +ThreadId, -ResultState)
% Executes the next instruction for the thread with ThreadId.
step(program(_, _, Instructions), state(Vars, Arrs, InstrPointers), 
    Id, ResState) :-
    nth0(Id, InstrPointers, IntrPointer),
    nth1(IntrPointer, Instructions, Instr),
    execInstr(Instr, Id, state(Vars, Arrs, InstrPointers), ResState), !.

% Predicate is true if current state is safe (no more than one thread in sekcja).
ensureSafe(program(_, _, Instructions), ResState) :-
    countThreadsInSekcja(Instructions, ResState, ThreadsInSekcja),
    ThreadsInSekcja =< 1.

% countThreadsInSekcja(+Instructions, +State, -Count)
countThreadsInSekcja(Instructions, state(_, _, IPs), Count) :-
    maplist(getInstruction(Instructions), IPs, ThreadsInstructions),
    countOccurences(ThreadsInstructions, sekcja, Count).

% getInstruction(+Instructions, +IP, -Instr)
getInstruction(Instructions, IP, Instr) :-
    nth1(IP, Instructions, Instr).


% ========================= Instructions =========================

% evalSimpleExpr(+SimpleExpr, +ThreadId, +State, -Result)
evalSimpleExpr(pid, Tid, _, Tid) :- !.

evalSimpleExpr(Number, _, _, Number) :-
    number(Number), !.

evalSimpleExpr(array(Name, IdxArithmExpr), Tid, State, Res) :-
    evalArithmExpr(IdxArithmExpr, Tid, State, Idx),
    getArrayValue(Name, Idx, State, Res), !.

evalSimpleExpr(Value, _, State, Res) :-
    atom(Value), !,
    getVar(Value, State, Res).

% evalArithmExpr(+ArithmExpr, +ThreadId, +State, -Result)
evalArithmExpr(SimExpr1 + SimExpr2, Tid, State, Res) :-
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Res is Val1 + Val2, !.

evalArithmExpr(SimExpr1 - SimExpr2, Tid, State, Res) :-
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Res is Val1 - Val2, !.

evalArithmExpr(SimExpr1 * SimExpr2, Tid, State, Res) :-
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Res is Val1 * Val2, !.

evalArithmExpr(SimExpr1 / SimExpr2, Tid, State, Res) :-
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Res is Val1 / Val2, !.

evalArithmExpr(SimExpr, Tid, State, Res) :-
    evalSimpleExpr(SimExpr, Tid, State, Res), !.

% Evaluating boolean expressions.
% evalBoolExpr(+BoolExpr, +ThreadId, +State)
% Predicate is `true` if BoolExpr is true in State with ThreadId,
% `false` otherwise.
evalBoolExpr(=(SimExpr1, SimExpr2), Tid, State) :- !,
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Val1 = Val2.

evalBoolExpr(SimExpr1 < SimExpr2, Tid, State) :- !,
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Val1 < Val2.

evalBoolExpr(<>(SimExpr1, SimExpr2), Tid, State) :- !,
    evalSimpleExpr(SimExpr1, Tid, State, Val1),
    evalSimpleExpr(SimExpr2, Tid, State, Val2),
    Val1 \= Val2.

% Executing instructions.
% execInstr(+Instruction, +ThreadId, +State, -ResultState)
% Predicate executes Instruction in State with ThreadId and returns
% ResultState.
execInstr(assign(array(Name, ArithmIdx), ArithmExpr), Tid, State, ResState) :-
    evalArithmExpr(ArithmExpr, Tid, State, Expr),
    evalArithmExpr(ArithmIdx, Tid, State, Idx),
    setArrayValue(Name, Idx, Expr, State, TmpState),
    incrementInstrPointer(TmpState, Tid, ResState).

execInstr(assign(VarName, ArithmExpr), Tid, State, ResState) :-
    atom(VarName), !,
    evalArithmExpr(ArithmExpr, Tid, State, Val),
    setVar(VarName, Val, State, TmpState),
    incrementInstrPointer(TmpState, Tid, ResState).

execInstr(goto(NewIP), Tid, State, ResState) :-
    setIntrPointer(State, Tid, NewIP, ResState).

execInstr(condGoto(BoolExpr, NewIP), Tid, State, ResState) :-
    evalBoolExpr(BoolExpr, Tid, State) ->
        setIntrPointer(State, Tid, NewIP, ResState);
        incrementInstrPointer(State, Tid, ResState).

execInstr(sekcja, Tid, State, ResState) :-
    incrementInstrPointer(State, Tid, ResState).

% ========================= State operations =========================

% incrementInstrPointer(+State, +ThreadId, -NewState)
incrementInstrPointer(state(V, A, IPs), Id, state(V, A, NewIPs)) :-
    nth0(Id, IPs, IP),
    NewIP is IP + 1,
    setNth0(IPs, Id, NewIP, NewIPs).

% setIntrPointer(+State, +ThreadId, +NewIP, -NewState)
setIntrPointer(state(V, A, IPs), Id, NewIP, state(V, A, NewIPs)) :-
    setNth0(IPs, Id, NewIP, NewIPs).

% getVar(+VarName, +State, -Value)
getVar(Name, state(Vars, _, _), Val) :-
    member(var(Name, Val), Vars).

% setVar(+VarName, +Value, +State, -NewState)
setVar(Name, Val, state(Vars, Arrs, IPs), state(NewVars, Arrs, IPs)) :-
    replaceInList(Vars, var(Name, _), var(Name, Val), NewVars).

% getArrayValue(+ArrayName, +Index, +State, -Value)
getArrayValue(Name, Idx, state(_, Arrs, _), Val) :-
    member(array(Name, Arr), Arrs),
    nth0(Idx, Arr, Val).

% setArrayValue(+ArrayName, +Index, +Value, +State, -NewState)
setArrayValue(Name, Idx, Val, state(Vars, Arrs, IPs), 
    state(Vars, NewArrs, IPs)) :-
    member(array(Name, Arr), Arrs),
    setNth0(Arr, Idx, Val, NewArr),
    replaceInList(Arrs, array(Name, Arr), array(Name, NewArr), NewArrs).


% ========================= List operations =========================

% replaceInList(+List, +OldValue, +NewValue, -NewList)
replaceInList(List, OldValue, NewValue, NewList) :-
    select(OldValue, List, ListWithoutElem),
    append(ListWithoutElem, [NewValue], NewList), !.

% setNth0(+List, +Index, +Value, -NewList)
setNth0(List, Idx, Val, ResList) :-
    nth0(Idx, List, _, WithoutNth0),
    nth0(Idx, ResList, Val, WithoutNth0).

% setNth1(+List, +Index, +Value, -NewList)
setNth1(List, Idx, Val, ResList) :-
    nth1(Idx, List, _, WithoutNth1),
    nth1(Idx, ResList, Val, WithoutNth1).

% countOccurences(+List, +Elem, -Count)
countOccurences(List, Elem, Count) :-
    include(=(Elem), List, Occurences),
    length(Occurences, Count).

% initListWithValues(+N, +Val, -List)
initListWithValues(N, Val, List) :-
    length(List, N),
    maplist(=(Val), List).

% ========================= Queue operations =========================

queue_empty([]).

is_queue_empty([]).

queue_add(List, Elem, NewList) :-
    append(List, [Elem], NewList).

queue_pop([Elem | Rest], Elem, Rest).
