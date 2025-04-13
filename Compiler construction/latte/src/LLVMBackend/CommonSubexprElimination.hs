module LLVMBackend.CommonSubexprElimination where

import qualified CodeGenerator.IRCode as IR
import qualified Data.Map as Map
import LLVMBackend.SSA(removeAssign)
import Control.Monad.State
import LLVMBackend.Utils(replaceValueBasicBlock)
import Debug.Trace (traceM, trace)
import Data.Maybe (isJust)



data MapKey =
    BinOpKey IR.Arg IR.Arg IR.BinaryOperator |
    UnOpKey IR.Arg IR.UnaryOperator
    deriving (Eq, Ord)

type SubExprMap = Map.Map MapKey IR.Variable

data StateM
  = StateM
  { blocks :: Map.Map IR.Label IR.BasicBlock,
    visited :: Map.Map IR.Label Bool,
    changes :: Bool
  }


eliminateSubExpr :: IR.Program -> IR.Program
eliminateSubExpr (IR.Program functions g c) = IR.Program (Map.map eliminateSubExprFunc functions) g c

eliminateSubExprFunc :: IR.Function -> IR.Function
eliminateSubExprFunc function =  
    let (function', changed) = eliminateSubExprFuncOne function
        function'' = removeAssign function'
    in if changed then eliminateSubExprFunc function'' else function''


eliminateSubExprFuncOne :: IR.Function -> (IR.Function, Bool)
eliminateSubExprFuncOne (IR.Function name retType args _blocks symbols order) =
    let initialState = StateM _blocks Map.empty False
        startLabel = head order
        resState = execState (eliminateSubExprQueue [(Map.empty, startLabel)]) initialState
        newBlocks = blocks resState
    in (IR.Function name retType args newBlocks symbols order, changes resState)


eliminateSubExprQueue :: [(SubExprMap, IR.Label)] -> State StateM ()
eliminateSubExprQueue [] = return ()
eliminateSubExprQueue ((currentMap, label): queue) = do
    visitedMap <- gets visited
    case Map.lookup label visitedMap of
        Just True -> return ()
        _ -> do
            modify (\state -> state { visited = Map.insert label True visitedMap })
            -- traceM $ "Visiting label: " ++ label
            blockMap <- gets blocks
            let block = blockMap Map.! label
            let instrs = IR.instrs block
            let (subExprMap', newInstrs, newChanges) = eliminateSubExprInstrs currentMap instrs
            let newBlock = block { IR.instrs = newInstrs }
            modify (\state -> state { blocks = Map.insert label newBlock blockMap })
            modify (\state -> state { changes = newChanges || (changes state) })

            let successors = IR.successors block
                successorsQueue = map (\label' -> (subExprMap', label')) successors
            
            eliminateSubExprQueue (queue ++ successorsQueue)


eliminateSubExprInstrs :: SubExprMap -> [IR.Instr] -> (SubExprMap, [IR.Instr], Bool)
eliminateSubExprInstrs subExprMap instrs =
    let (subExprMap', newInstrs, changed) = foldl accSubExprElim (subExprMap, [], False) instrs
    in (subExprMap', reverse newInstrs, changed)
    where
        accSubExprElim :: (SubExprMap, [IR.Instr], Bool) ->  IR.Instr -> (SubExprMap, [IR.Instr], Bool)
        accSubExprElim  (m, accIns, changed) instr  =
            let (m', ins', changed') = eliminateSubExprInstr m instr
            in (m', ins': accIns, changed || changed')

eliminateSubExprInstr :: SubExprMap -> IR.Instr -> (SubExprMap, IR.Instr, Bool)
eliminateSubExprInstr subExprMap (IR.BinOp var arg1 op arg2) =
    let lookupResult = Map.lookup (BinOpKey arg1 arg2 op) subExprMap
    in case lookupResult of 
        Just var' ->
        --   trace ("Found common subexpression: " ++ show (IR.BinOp var arg1 op arg2)) $
            (subExprMap, IR.Assign var (IR.Ident var'), True)
        Nothing ->
        --   trace ("Did not find common subexpression: " ++ show (IR.BinOp var arg1 op arg2)) $
            let newMap =  Map.insert (BinOpKey arg1 arg2 op) var (Map.insert (BinOpKey arg1 arg2 op) var subExprMap)
            in (newMap, IR.BinOp var arg1 op arg2, False)
eliminateSubExprInstr subExprMap (IR.UnOp var op arg) =
    let lookupResult = Map.lookup (UnOpKey arg op) subExprMap
    in case lookupResult of
        Just var' ->
            (subExprMap, IR.Assign var (IR.Ident var'), True)
        Nothing ->
            let newMap = Map.insert (UnOpKey arg op) var subExprMap
            in (newMap, IR.UnOp var op arg, False)
eliminateSubExprInstr subExprMap instr = (subExprMap, instr, False)

