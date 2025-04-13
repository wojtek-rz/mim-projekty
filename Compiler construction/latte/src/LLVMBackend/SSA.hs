module LLVMBackend.SSA where

import CodeGenerator.IRCode
import qualified CodeGenerator.IRCode as IR
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import LLVMBackend.Utils (replaceValueBasicBlock)

generateLLVM :: IR.Program -> String
generateLLVM _ = undefined

generateLLVMFunction :: IR.Function -> String
generateLLVMFunction _ = undefined

type FirstID = Integer

type BlockSSASymbolTable = Map.Map Variable Integer

data SSAState = SSAState
  { blocksFirstID :: Map.Map Label FirstID,
    symbolTable :: BlockSSASymbolTable,
    blocksOutSymbolTable :: Map.Map Label BlockSSASymbolTable,
    blocksInSymbolTable :: Map.Map Label BlockSSASymbolTable
  }

initialSSAState :: SSAState
initialSSAState = SSAState Map.empty Map.empty Map.empty Map.empty

programToSSA :: IR.Program -> IR.Program
programToSSA program = program {IR.functions = Map.map _functionToSSA $ IR.functions program}
  where
    _functionToSSA :: IR.Function -> IR.Function
    _functionToSSA function = fixEntryUndefinedRegisters $ removeAssign $ evalState (functionToSSA function) initialSSAState

functionToSSA :: IR.Function -> State SSAState IR.Function
functionToSSA function =
  let labels = IR.basicBlockOrder function
      basicBlocks = map (\label -> blocks function Map.! label) labels
      functionVariables = map (uncurry Variable) $ Map.toList (symbols function)
      initialSymbolTable = Map.fromList $ zip functionVariables (repeat 0)
      args' = map (\arg -> IR.Variable (getSSASymbol (IR.varName arg) 1) (IR.varType arg)) $ args function
   in do
        modify $ \state -> state {symbolTable = initialSymbolTable}
        blocks' <- mapM basicBlockToSSA basicBlocks
        blocks'' <- mapM addPhiToBlock blocks'
        return $ function {blocks = Map.fromList $ zip labels blocks'', args = args'}

basicBlockToSSA :: BasicBlock -> State SSAState BasicBlock
basicBlockToSSA block = do
  let blockInstrs = instrs block
  let blockLabel = label block

  blockSymbolTable <- gets symbolTable

  let blockInSymbolTable = incrementSymbolTable blockSymbolTable

  (blockInstrSSA, blockOutSymbolTable) <- return $ runState (mapM instrToSSA blockInstrs) blockInSymbolTable
  blocksOutSymbolTable_ <- gets blocksOutSymbolTable
  blocksInSymbolTable_ <- gets blocksInSymbolTable

  let newBlocksOutSymbolTable = Map.insert blockLabel blockOutSymbolTable blocksOutSymbolTable_
  let newBlocksInSymbolTable = Map.insert blockLabel blockInSymbolTable blocksInSymbolTable_

  modify $ \state ->
    state
      { symbolTable = blockOutSymbolTable,
        blocksOutSymbolTable = newBlocksOutSymbolTable,
        blocksInSymbolTable = newBlocksInSymbolTable
      }
  return $ block {instrs = blockInstrSSA}

incrementSymbolTable :: BlockSSASymbolTable -> BlockSSASymbolTable
incrementSymbolTable = Map.map (+ 1)

getSSASymbol :: Ident -> Integer -> Ident
getSSASymbol ident id = ident ++ "_" ++ show id

getSSAVariableIncrement :: Variable -> State BlockSSASymbolTable Variable
getSSAVariableIncrement var = do
  symbolTable <- get
  case Map.lookup var symbolTable of
    Just id -> do
      put $ Map.insert var (id + 1) symbolTable
      return $ _getSSAVariable var (id + 1)
    Nothing -> return var

getSSAVariable :: Variable -> State BlockSSASymbolTable Variable
getSSAVariable var = do
  symbolTable <- get
  case Map.lookup var symbolTable of
    Just id -> return $ _getSSAVariable var id
    Nothing -> return $ var

getSSAArgument :: Arg -> State BlockSSASymbolTable Arg
getSSAArgument (Ident var) = do
  symbolTable <- get
  case Map.lookup var symbolTable of
    Just id -> return $ Ident $ _getSSAVariable var id
    Nothing -> return $ Ident $ var
getSSAArgument arg = return arg

_getSSAVariable :: Variable -> Integer -> Variable
_getSSAVariable (Variable ident ty) id = Variable (getSSASymbol ident id) ty

instrToSSA :: IR.Instr -> State BlockSSASymbolTable IR.Instr
instrToSSA (BinOp arg1 arg2 operator arg3) = do
  arg3' <- getSSAArgument arg3
  arg2' <- getSSAArgument arg2
  arg1' <- getSSAVariableIncrement arg1
  return $ BinOp arg1' arg2' operator arg3'
instrToSSA (UnOp arg1 operator arg2) = do
  arg2' <- getSSAArgument arg2
  arg1' <- getSSAVariableIncrement arg1
  return $ UnOp arg1' operator arg2'
instrToSSA (Assign arg1 arg2) = do
  arg2' <- getSSAArgument arg2
  arg1' <- getSSAVariableIncrement arg1
  return $ Assign arg1' arg2'
instrToSSA (Branch arg1 label1 label2) = do
  arg1' <- getSSAArgument arg1
  return $ Branch arg1' label1 label2
instrToSSA (Call arg1 ident args) = do
  args' <- mapM getSSAArgument args
  arg1' <- getSSAVariableIncrement arg1
  return $ Call arg1' ident args'
instrToSSA (Return arg1) = do
  arg1' <- getSSAArgument arg1
  return $ Return arg1'
-- instrToSSA (GetGlobalStringPointer arg1 arg2 str) = do
--   arg2' <- getSSAArgument arg2
--   arg1' <- getSSAVariableIncrement arg1
--   return $ GetGlobalStringPointer arg1' arg2' str
instrToSSA (GEP res arg offsets) = do
  arg' <- getSSAArgument arg
  res' <- getSSAVariableIncrement res
  offsets' <- mapM getSSAArgument offsets
  return $ GEP res' arg' offsets'
instrToSSA instr = return instr

addPhi :: IR.Function -> State SSAState IR.Function
addPhi function = do
  let basicBlocks = Map.elems $ blocks function
  let labels = Map.keys $ blocks function
  blocks' <- mapM addPhiToBlock basicBlocks
  return $ function {blocks = Map.fromList $ zip labels blocks'}

addPhiToBlock :: BasicBlock -> State SSAState BasicBlock
addPhiToBlock block =
  let blockLabel = label block
   in do
        blocksInSymbolTable_ <- gets blocksInSymbolTable
        blocksOutSymbolTable_ <- gets blocksOutSymbolTable
        let blockInSymbolTable = blocksInSymbolTable_ Map.! blockLabel
        let blockPredecessors = predecessors block
        case blockPredecessors of
          [] -> return block
          [predecessor] ->
            let newInstr = generateAssignInstr blockInSymbolTable (blocksOutSymbolTable_ Map.! predecessor)
             in return $ block {instrs = newInstr ++ instrs block}
          predecessors ->
            let newInstr = generatePhiInstr blockInSymbolTable (mapGetList predecessors blocksOutSymbolTable_)
             in return $ block {instrs = newInstr ++ instrs block}
  where
    generateAssignInstr :: BlockSSASymbolTable -> BlockSSASymbolTable -> [IR.Instr]
    generateAssignInstr inSymTable outSymTable =
      let inSymTableList = Map.toList inSymTable
       in map (\(var, id) -> Assign (_getSSAVariable var id) (Ident $ _getSSAVariable var (outSymTable Map.! var))) inSymTableList
    generatePhiInstr :: BlockSSASymbolTable -> [(Label, BlockSSASymbolTable)] -> [IR.Instr]
    generatePhiInstr inSymTable outSymTables =
      let inSymTableList = Map.toList inSymTable
       in map (\(var, id) -> Phi (_getSSAVariable var id) (phiArgList var outSymTables)) inSymTableList
    phiArgList :: Variable -> [(Label, BlockSSASymbolTable)] -> [(Label, Maybe Arg)]
    phiArgList var =
      map (\(label, outSymTable) -> (label, Just $ Ident $ _getSSAVariable var (outSymTable Map.! var)))
    mapGetList :: [Label] -> Map.Map Label a -> [(Label, a)]
    mapGetList labels labelsMap = map (\label -> (label, labelsMap Map.! label)) labels

removeAssign :: IR.Function -> IR.Function
removeAssign = execState _removeAssign
  where
    _removeAssign :: State IR.Function ()
    _removeAssign = do
      function <- get
      let labels = basicBlockOrder function
      mapM_ _removeAllAssign labels

    _removeAllAssign :: Label -> State IR.Function ()
    _removeAllAssign label = do
      removedAny <- _removeOneAssign label
      if removedAny
        then do
          _removeAllAssign label
          return ()
        else return ()

    _removeOneAssign :: Label -> State IR.Function Bool
    _removeOneAssign label = do
      function <- get
      let block = blocks function Map.! label
      let (maybeAssign, instrs') = findAndRemoveAssign $ instrs block
      case maybeAssign of
        Just (old, new) -> do
          put $ modifyBlock (\block' -> block' {instrs = instrs'}) label function
          allBlocks <- gets blocks
          let allBlocks' = Map.map (replaceValueBasicBlock old new) allBlocks
          put $ function {blocks = allBlocks'}
          return True
        Nothing -> return False

    findAndRemoveAssign :: [IR.Instr] -> (Maybe (IR.Ident, IR.Arg), [IR.Instr])
    findAndRemoveAssign [] = (Nothing, [])
    findAndRemoveAssign (x : xs) =
      case getAssign x of
        Just assign -> (Just assign, xs)
        Nothing -> let (assign, instrs') = findAndRemoveAssign xs in (assign, x : instrs')
      where
        getAssign :: IR.Instr -> Maybe (IR.Ident, IR.Arg)
        getAssign (Assign res arg) = Just (IR.varName res, arg)
        getAssign (Phi res args) =
          let args' = map snd args
              firstArg = head args'
           in case firstArg of
                Just arg -> if all (== firstArg) args' then Just (IR.varName res, arg) else Nothing
                Nothing -> case args' of 
                  [Just (Ident res), Just arg2] -> Just (IR.varName res, arg2)
                  [Just arg1, Just (Ident res)] -> Just (IR.varName res, arg1)
                  _ -> Nothing
        getAssign _ = Nothing

-- Should be called after removing assigns, so all uses of registers are removed
fixEntryUndefinedRegisters :: IR.Function -> IR.Function
fixEntryUndefinedRegisters function =
  let definedRegs = getDefinedRegisters (concatMap instrs $ Map.elems $ blocks function) -- @TODO full liveness analysis
      shouldBeDefined = getUsedRegisters (concatMap instrs $ Map.elems $ blocks function)
      shouldBeDefined' = Set.fromList shouldBeDefined
      definedRegs' = Set.fromList (definedRegs ++ (args function))
      undefinedRegs = Set.toList $ Set.difference shouldBeDefined' definedRegs'
      blocks' = Map.map (removeFromPhiBlockAll undefinedRegs) (blocks function)
   in function {blocks = blocks'}
  where
    isRegisterUsed :: IR.Variable -> IR.BasicBlock -> Bool
    isRegisterUsed var block = undefined
    getUsedRegisters :: [IR.Instr] -> [IR.Variable]
    getUsedRegisters instrs =
      foldr
        ( \maybeVar acc -> case maybeVar of
            Just var -> var : acc
            Nothing -> acc
        )
        []
        (concatMap getUsedRegistersInstr instrs)
    getUsedRegistersInstr :: IR.Instr -> [Maybe IR.Variable]
    getUsedRegistersInstr (BinOp res arg1 _ arg2) = [getVarArg arg1, getVarArg arg2]
    getUsedRegistersInstr (UnOp res _ arg) = [getVarArg arg]
    getUsedRegistersInstr (Assign res arg) = [getVarArg arg]
    getUsedRegistersInstr (Branch arg _ _) = [getVarArg arg]
    getUsedRegistersInstr (Call res _ args) = map getVarArg args
    getUsedRegistersInstr (Return arg) = [getVarArg arg]
    getUsedRegistersInstr (Phi res args) =
      let maybeArgs = map snd args
          justArgs = filter isJust maybeArgs
          args' =
            foldr
              ( \maybeVar acc -> case maybeVar of
                  Just var -> var : acc
                  Nothing -> acc
              )
              []
              justArgs
       in map getVarArg args'
    getUsedRegistersInstr (GEP res arg offsets) = getVarArg arg : map getVarArg offsets
    getUsedRegistersInstr _ = []

    getDefinedRegisters :: [IR.Instr] -> [IR.Variable]
    getDefinedRegisters instrs = concatMap getDefinedRegistersInstr instrs
    getDefinedRegistersInstr :: IR.Instr -> [IR.Variable]
    getDefinedRegistersInstr (BinOp res _ _ _) = [res]
    getDefinedRegistersInstr (UnOp res _ _) = [res]
    getDefinedRegistersInstr (Assign res _) = [res]
    getDefinedRegistersInstr (Call res _ _) = [res]
    getDefinedRegistersInstr (Phi res _) = [res]
    getDefinedRegistersInstr (GEP res _ _) = [res]
    getDefinedRegistersInstr _ = []

    getVarArg :: IR.Arg -> Maybe IR.Variable
    getVarArg (IR.Ident var) = Just var
    getVarArg _ = Nothing
    assignDefualt :: IR.Variable -> IR.Instr
    assignDefualt var@(IR.Variable ident ty) = IR.Assign var (defaultArg ty)

    removeFromPhiBlockAll :: [IR.Variable] -> IR.BasicBlock -> IR.BasicBlock
    removeFromPhiBlockAll vars block = foldr removeFromPhiBlock block vars
    removeFromPhiBlock :: IR.Variable -> IR.BasicBlock -> IR.BasicBlock
    removeFromPhiBlock var block = block {instrs = map (removeFromPhi var) (instrs block)}

    removeFromPhi :: IR.Variable -> IR.Instr -> IR.Instr
    removeFromPhi var (Phi res args) = Phi res (map checkArg args)
      where
        checkArg :: (Label, Maybe Arg) -> (Label, Maybe Arg)
        checkArg (label, Just arg) = if arg == Ident var then (label, Nothing) else (label, Just arg)
        checkArg arg = arg
    removeFromPhi _ instr = instr