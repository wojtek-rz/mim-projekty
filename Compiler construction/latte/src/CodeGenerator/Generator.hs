module CodeGenerator.Generator (compile) where

import qualified Bnfc.Abs as Abs
-- import CodeGenerator.SymbolTable (genSymbolTable)
import CodeGenerator.IRCode (Arg, Ident, Label)
import qualified CodeGenerator.IRCode as IR
import CodeGenerator.Symbols (functionAddSymbolIDs)
import Control.Monad.State
import qualified Data.Map as Map
import TypeCheck.LangTypes
import TypeCheck.Builtins (toDeclList)
import BnfcUtils (getIdent)
import qualified TypeCheck.LangTypes as LangType
import Debug.Trace (traceM, trace)

type GenM result = State GenState result

data GenState = GenState
  { functionsDefinitions :: Map.Map Ident IR.Function,
    functionsDeclarations :: Map.Map Ident FunctionType,
    classDefinitions :: Map.Map Ident LangType.ClassTypeDef,
    gconstants :: Map.Map Ident IR.Value,
    currentFunction :: IR.Function,
    currentBlock :: IR.BasicBlock,
    nextLabel :: Integer,
    nextGlobal :: Integer,
    nextTemp :: Integer
  }

initState :: Map.Map Ident FunctionType -> Map.Map Ident LangType.ClassTypeDef -> GenState
initState funDecls clsDefs =
  GenState
    { functionsDefinitions = Map.empty,
      functionsDeclarations = funDecls,
      classDefinitions = clsDefs,
      gconstants = Map.empty,
      currentFunction = IR.Function "" VoidType [] Map.empty Map.empty [],
      currentBlock = IR.BasicBlock "" [] [] [],
      nextLabel = 1,
      nextGlobal = 0,
      nextTemp = 0
    }

compile :: Abs.Program -> IR.Program
compile (Abs.Program _ topDefs) = IR.Program functions_ gconstants_ definedClasses
  where
    declaredFunTypes = getFunctionsDeclarations topDefs
    definedClasses = Map.fromList $ (builtinClasses ++ getClassDefinitions topDefs)
    builtinFunTypes = toDeclList
    funTypes = Map.fromList $ declaredFunTypes ++ builtinFunTypes
    resultState = execState (mapM_ genTopDef topDefs) (initState funTypes definedClasses)
    functions_ = functionsDefinitions resultState
    gconstants_ = gconstants resultState

getFunctionsDeclarations:: [Abs.TopDef] -> [(Ident, FunctionType)]
getFunctionsDeclarations [] = []
getFunctionsDeclarations ((Abs.FnDef _ ty (Abs.Ident ident) args _):topDefs) = (ident, FunctionType (fromAstType ty) (map (\(Abs.Arg _ ty _) -> fromAstType ty) args)) : getFunctionsDeclarations topDefs
getFunctionsDeclarations (_:topDefs) = getFunctionsDeclarations topDefs

getClassDefinitions :: [Abs.TopDef] -> [(Ident, LangType.ClassTypeDef)]
getClassDefinitions [] = []
getClassDefinitions (def@(Abs.ClsDef _ ident _):topDefs) =
  (getIdent ident, classFromAst def) : getClassDefinitions topDefs
getClassDefinitions (_:topDefs) = getClassDefinitions topDefs

builtinClasses :: [(Ident, LangType.ClassTypeDef)]
builtinClasses = [("__arr__", [("data", StringType), ("length", IntType)])]

genTopDef :: Abs.TopDef -> GenM ()
genTopDef topDef@(Abs.FnDef _ _ _ _ _) =
  let (symTable, topDef') = functionAddSymbolIDs topDef
   in case topDef' of
        Abs.FnDef _ ty (Abs.Ident ident) _args block ->
          let arguments = map (\(Abs.Arg _ ty (Abs.Ident arg)) -> IR.Variable arg (fromAstType ty)) _args
              retType = fromAstType ty
           in do
                genFunction ident retType arguments symTable block
        _ -> return ()
genTopDef (Abs.ClsDef _ _ _) = return ()

genFunction :: Ident -> LangType -> [IR.Variable] -> Map.Map Ident LangType -> Abs.Block -> GenM ()
genFunction ident retType args symTable funBlock@(Abs.Block _ stmts) = do
  let function = IR.Function ident retType args Map.empty symTable []
      startBlock = IR.BasicBlock "entry" [] [] []
      startLabel = 1
      startTemp = 0
  modify $ \state -> state {currentFunction = function, currentBlock = startBlock, nextLabel = startLabel, nextTemp = startTemp}
  genBlock funBlock
  fixNoReturn
  endBasicBlock
  _function <- gets currentFunction
  let fun = functionPostProcess _function
  modify $ \state -> state {functionsDefinitions = Map.insert (IR.name fun) fun $ functionsDefinitions state}
  where
  -- If there is no return statement in the function, add a return statement
    fixNoReturn :: GenM ()
    fixNoReturn = do
      instrs <- gets $ IR.instrs . currentBlock
      case instrs of
        (IR.VReturn:_) -> return ()
        (IR.Return _:_) -> return ()
        _ -> case retType of
          VoidType -> emit IR.VReturn
          _ -> return ()


genBlock :: Abs.Block -> GenM ()
genBlock (Abs.Block _ stmts) = do
  mapM_ genStmt stmts

genStmt :: Abs.Stmt -> GenM ()
genStmt (Abs.Empty _) = return ()
genStmt (Abs.BStmt _ block) = genBlock block
genStmt (Abs.Decl _ _ items) = mapM_ genDeclItem items
genStmt (Abs.Ass _ lvalue expr) = do
  rhs <- genExpr expr
  storeLValue lvalue rhs
genStmt (Abs.Incr _ lvalue) = do
  oldVal <- getLValue lvalue
  newVal <- getTempVar (IR.typeFromArg oldVal)
  emit $ IR.BinOp newVal oldVal IR.Add (IR.intArg 1)
  storeLValue lvalue (IR.Ident newVal)
genStmt (Abs.Decr _ lvalue) = do
  oldVal <- getLValue lvalue
  newVal <- getTempVar (IR.typeFromArg oldVal)
  emit $ IR.BinOp newVal oldVal IR.Sub (IR.intArg 1)
  storeLValue lvalue (IR.Ident newVal)
genStmt (Abs.Ret _ expr) = do
  res <- genExpr expr
  emit $ IR.Return res
genStmt (Abs.VRet _) = do
  emit IR.VReturn
genStmt (Abs.Cond _ expr stmt) = do
  jTrue <- getNewLabel "true"
  jFalse <- getNewLabel "false"
  genExprWithJumps expr jTrue jFalse
  startNewBasicBlock jTrue
  genStmt stmt
  emit $ IR.GoTo jFalse
  startNewBasicBlock jFalse
genStmt (Abs.CondElse _ expr stmtTrue stmtFalse) = do
  jTrue <- getNewLabel "true"
  jFalse <- getNewLabel "false"
  jContinue <- getNewLabel "continue"
  genExprWithJumps expr jTrue jFalse
  startNewBasicBlock jTrue
  genStmt stmtTrue
  emit $ IR.GoTo jContinue
  startNewBasicBlock jFalse
  genStmt stmtFalse
  emit $ IR.GoTo jContinue
  startNewBasicBlock jContinue
genStmt (Abs.While _ expr stmt) = do
  jBody <- getNewLabel "body_while"
  jCond <- getNewLabel "cond_while"
  jCont <- getNewLabel "after_while"
  emit $ IR.GoTo jCond
  startNewBasicBlock jCond
  genExprWithJumps expr jBody jCont
  startNewBasicBlock jBody
  genStmt stmt
  emit $ IR.GoTo jCond
  startNewBasicBlock jCont
genStmt (Abs.SExp _ expr) = do
  genExpr expr
  return ()
genStmt (Abs.For pos ty var array stmt) = error "For loop should be replaced in symbols"

genExpr :: Abs.Expr -> GenM Arg
genExpr (Abs.ELValue _ lvalue) = do
  getLValue lvalue
genExpr (Abs.ELitInt _ value) = return $ IR.intArg value
genExpr (Abs.ELitTrue _) = return $ IR.boolArg True
genExpr (Abs.ELitFalse _) = return $ IR.boolArg False
genExpr (Abs.EApp _ (Abs.Ident ident) exprs) = do
  args <- mapM genExpr exprs
  funType <- gets $ \state -> functionsDeclarations state Map.! ident
  temp <- getTempVar (returns funType)
  emit $ IR.Call temp ident args
  return $ IR.Ident temp
genExpr (Abs.EString _ string) = do
  arg <- getNewGlobal $ IR.LiteralString string
  converted <- getTempVar StringType
  emit $ IR.GEP converted arg [IR.intArg 0, IR.intArg 0]
  return $ IR.Ident converted
genExpr (Abs.Neg _ expr) = do
  res <- genExpr expr
  temp <- getTempVar IntType
  emit $ IR.UnOp temp IR.Neg res
  return $ IR.Ident temp
genExpr (Abs.EMul _ leftExpr mulOp rightExpr) = do
  leftCode <- genExpr leftExpr
  rightCode <- genExpr rightExpr
  temp <- getTempVar IntType
  case mulOp of
    Abs.Times _ -> do
      -- Handle Times operation
      -- For example, you might generate code like this:
      emit $ IR.BinOp temp leftCode IR.Mul rightCode
    Abs.Div _ -> do
      emit $ IR.BinOp temp leftCode IR.Div rightCode
    Abs.Mod _ -> do
      emit $ IR.BinOp temp leftCode IR.Mod rightCode
  return $ IR.Ident temp
genExpr (Abs.EAdd _ leftExpr addOp rightExpr) = do
  leftCode <- genExpr leftExpr
  rightCode <- genExpr rightExpr
  case (IR.typeFromArg leftCode, IR.typeFromArg rightCode) of
    (StringType, StringType) -> genConcatStrings leftCode rightCode
    (IntType, IntType) -> genAddIntegers leftCode rightCode addOp
    (_, _) -> error "Invalid types for addition"
  where
    genConcatStrings leftCode rightCode = do
      temp <- getTempVar StringType
      emit $ IR.Call temp "_concat_strings" [leftCode, rightCode]
      return $ IR.Ident temp
    genAddIntegers leftCode rightCode addOp = do
      temp <- getTempVar IntType
      case addOp of
        Abs.Plus _ -> do
          emit $ IR.BinOp temp leftCode IR.Add rightCode
        Abs.Minus _ -> do
          emit $ IR.BinOp temp leftCode IR.Sub rightCode
      return $ IR.Ident temp
genExpr (Abs.ERel _ expr1 relOp expr2) = do
  leftCode <- genExpr expr1
  rightCode <- genExpr expr2
  temp <- getTempVar BoolType
  emit $ IR.BinOp temp leftCode (IR.CmpOp (toRelOp relOp)) rightCode
  return $ IR.Ident temp -- result is of type Boolean
genExpr expr@(Abs.EOr _ _ _) = do
  genLogicExpr expr
genExpr expr@(Abs.EAnd _ _ _) = do
  genLogicExpr expr
genExpr expr@(Abs.Not _ _) = do
  genLogicExpr expr
genExpr (Abs.ENewArray _ ty expr) = do
  arrPointer <- getTempVar StringType
  typedArrPointer <- getTempVar (ArrayType (fromAstType ty))
  arrStructBytes <- calculateBytes (ClassType "__arr__")
  emit $ IR.Call arrPointer "malloc" [IR.intArg arrStructBytes]
  emit $ IR.UnOp typedArrPointer IR.BitCast (IR.Ident arrPointer)

  arraySizeBytes <- getTempVar IntType
  len <- genExpr expr
  arrayElemSize <- calculateBytes (fromAstType ty)
  emit $ IR.BinOp arraySizeBytes len IR.Mul (IR.intArg arrayElemSize)

  rawDataPtr <- getTempVar StringType
  emit $ IR.Call rawDataPtr "malloc" [IR.Ident arraySizeBytes]

  storeClassAttribute (IR.Ident typedArrPointer) "__arr__" "data" (IR.Ident rawDataPtr)
  storeClassAttribute (IR.Ident typedArrPointer) "__arr__" "length" len

  return $ IR.Ident typedArrPointer
genExpr (Abs.ENewObject _ (Abs.Ident ident)) = do
  rawPointer <- getTempVar StringType
  typedPointer <- getTempVar (ClassType ident)
  mallocBytes <- calculateBytes (ClassType ident)
  emit $ IR.Call rawPointer "malloc" [IR.intArg mallocBytes]
  emit $ IR.UnOp typedPointer IR.BitCast (IR.Ident rawPointer)
  return $ IR.Ident typedPointer
genExpr (Abs.ENull _ (Abs.Ident ident)) = do
  return $ IR.Literal $ IR.Null (ClassType ident)

-- @TODO from subexpression elimination exclude arrays and load and store
getLValue :: Abs.LValue -> GenM Arg
getLValue (Abs.LVar _ (Abs.Ident ident)) = do
  var <- localVariableLookup ident
  return $ IR.Ident var
getLValue (Abs.LArr _ lvalue expr) = do
  arrayStruct <- getLValue lvalue
  let arrType = IR.typeFromArg arrayStruct
      (ArrayType underlyingType) = arrType

  datai8Ptr <- loadClassAttribute arrayStruct "__arr__" "data"
  dataPtr <- getTempVar $ Pointer underlyingType
  emit $ IR.UnOp dataPtr IR.BitCast datai8Ptr

  index <- genExpr expr

  valuePtr <- getTempVar $ Pointer underlyingType
  emit $ IR.GEP valuePtr (IR.Ident dataPtr) [index]

  value <- getTempVar underlyingType
  emit $ IR.Call value "__load__" [(IR.Ident valuePtr)]
  return $ IR.Ident value
getLValue (Abs.LAttr _ lvalue (Abs.Ident ident)) = do
  obj <- getLValue lvalue
  let objType = IR.typeFromArg obj
  case objType of
    ClassType cls -> do
      loadClassAttribute obj cls ident
    ArrayType _ -> do
      case ident of
        "length" -> do
            loadClassAttribute obj "__arr__" "length"
        _ -> error "Invalid attribute"
    _ -> error "Taking attribute of a non class type"

storeLValue :: Abs.LValue -> Arg -> GenM ()
storeLValue (Abs.LVar _ (Abs.Ident ident)) newVal = do
  var <- localVariableLookup ident
  emit $ IR.Assign var newVal
storeLValue (Abs.LArr _ lvalue expr) newVal = do
  array <- getLValue lvalue
  let arrType = IR.typeFromArg array
      (ArrayType underlyingType) = arrType
  datai8Ptr <- loadClassAttribute array "__arr__" "data"
  dataPtr <- getTempVar $ Pointer underlyingType
  emit $ IR.UnOp dataPtr IR.BitCast datai8Ptr

  index <- genExpr expr

  valuePtr <- getTempVar $ Pointer underlyingType
  emit $ IR.GEP valuePtr (IR.Ident dataPtr) [index]

  tmpVoidReturn <- getTempVar VoidType
  emit $ IR.Call tmpVoidReturn "__store__" [newVal, IR.Ident valuePtr]
storeLValue (Abs.LAttr _ lvalue (Abs.Ident ident)) newVal = do 
  obj <- getLValue lvalue
  let objType = IR.typeFromArg obj
  case objType of
    ClassType cls -> do
      storeClassAttribute obj cls ident newVal
    _ -> error "Taking attribute of a non class type"

loadClassAttribute :: Arg -> String -> String -> GenM Arg
loadClassAttribute obj cls ident = do
  classDef <- gets $ \state -> classDefinitions state Map.! cls
  let attr = getClassAttr classDef ident
  case attr of 
    Just (ty, offset) -> do
      valuePtr <- getTempVar (Pointer ty)
      value <- getTempVar ty
      emit $ IR.GEP valuePtr obj [IR.intArg 0, IR.intArg offset]
      emit $ IR.Call value "__load__" [IR.Ident valuePtr]
      return $ IR.Ident value
    Nothing -> error $ "Invalid attribute: " ++ ident ++ " in class " ++ cls ++ show obj

storeClassAttribute :: Arg -> String -> String -> Arg -> GenM ()
storeClassAttribute obj cls ident newVal = do
  classDef <- gets $ \state -> classDefinitions state Map.! cls
  let attr = getClassAttr classDef ident
  case attr of 
    Just (ty, offset) -> do
      valuePtr <- getTempVar (Pointer ty)
      emit $ IR.GEP valuePtr obj [IR.intArg 0, IR.intArg offset]
      tmpVoidReturn <- getTempVar VoidType
      emit $ IR.Call tmpVoidReturn "__store__" [newVal, IR.Ident valuePtr]
    Nothing -> error "Invalid attribute"

calculateBytesPrimitive :: LangType -> Integer
calculateBytesPrimitive IntType = 4
calculateBytesPrimitive BoolType = 1
calculateBytesPrimitive StringType = 8
calculateBytesPrimitive (ArrayType _) = 8
calculateBytesPrimitive (ClassType _) = 8
calculateBytesPrimitive _ = error "Unsupported type"

calculateBytes :: LangType -> GenM Integer
calculateBytes (ClassType cls) = do
  classDef <- gets $ \state -> classDefinitions state Map.! cls
  return $ sum $ map (calculateBytesPrimitive . snd) classDef
calculateBytes ty = return $ calculateBytesPrimitive ty

genDeclItem :: Abs.Item -> GenM ()
genDeclItem (Abs.NoInit _ (Abs.Ident ident)) = do
  var <- localVariableLookup ident
  case IR.varType var of
    IntType -> emit $ IR.Assign var (IR.intArg 0)
    BoolType -> emit $ IR.Assign var (IR.boolArg False)
    StringType -> do
      arg <- genExpr (Abs.EString undefined "")
      emit $ IR.Assign var arg
    ArrayType ty -> do
      let arg = IR.Literal $ IR.Null $ ArrayType ty
      emit $ IR.Assign var arg
    ClassType cls -> do
      let arg = IR.Literal $ IR.Null $ ClassType cls
      emit $ IR.Assign var arg
    _ -> error "Unsupported type"
genDeclItem (Abs.Init _ (Abs.Ident ident) expr) = do
  var <- localVariableLookup ident
  rhs <- genExpr expr
  emit $ IR.Assign var rhs

genLogicExpr :: Abs.Expr -> GenM Arg
genLogicExpr expr = do
  jTrue <- getNewLabel "true"
  jFalse <- getNewLabel "false"
  jContinue <- getNewLabel "continue"
  newVar <- getHiddenVar BoolType
  genExprWithJumps expr jTrue jFalse
  startNewBasicBlock jTrue
  emit $ IR.Assign newVar (IR.boolArg True)
  emit $ IR.GoTo jContinue
  startNewBasicBlock jFalse
  emit $ IR.Assign newVar (IR.boolArg False)
  emit $ IR.GoTo jContinue
  startNewBasicBlock jContinue
  return $ IR.Ident newVar

genExprWithJumps :: Abs.Expr -> Label -> Label -> GenM ()
genExprWithJumps (Abs.ELitTrue _) jTrue jFalse = emit $ IR.GoTo jTrue
genExprWithJumps (Abs.ELitFalse _) jTrue jFalse = emit $ IR.GoTo jFalse
genExprWithJumps (Abs.Not _ expr) jTrue jFalse = genExprWithJumps expr jFalse jTrue
genExprWithJumps (Abs.ERel _ expr1 relOp expr2) jTrue jFalse = do
  leftCode <- genExpr expr1
  rightCode <- genExpr expr2
  temp <- getTempVar BoolType
  emit $ IR.BinOp temp leftCode (IR.CmpOp (toRelOp relOp)) rightCode
  emit $ IR.Branch (IR.Ident temp) jTrue jFalse
genExprWithJumps (Abs.EAnd _ expr1 expr2) jTrue jFalse = do
  jContinue <- getNewLabel "and_middle"
  genExprWithJumps expr1 jContinue jFalse
  startNewBasicBlock jContinue
  genExprWithJumps expr2 jTrue jFalse
genExprWithJumps (Abs.EOr _ expr1 expr2) jTrue jFalse = do
  jContinue <- getNewLabel "or_middle"
  genExprWithJumps expr1 jTrue jContinue
  startNewBasicBlock jContinue
  genExprWithJumps expr2 jTrue jFalse
genExprWithJumps expr jTrue jFalse = do
  res <- genExpr expr
  emit $ IR.Branch res jTrue jFalse

toRelOp :: Abs.RelOp -> IR.CompareOperator
toRelOp (Abs.LTH _) = IR.Lt
toRelOp (Abs.LE _) = IR.Le
toRelOp (Abs.GTH _) = IR.Gt
toRelOp (Abs.GE _) = IR.Ge
toRelOp (Abs.EQU _) = IR.Eq
toRelOp (Abs.NE _) = IR.Ne


--------------------------------------- Utilities ---------------------------------------

getTempVar :: LangType -> GenM IR.Variable
getTempVar langType = do
  temp <- gets nextTemp
  modify $ \state -> state {nextTemp = temp + 1}
  return
    IR.Variable
      { IR.varName = "temp_" ++ show temp,
        IR.varType = langType
      }

getHiddenVar :: LangType -> GenM IR.Variable
getHiddenVar langType = do
  var <- getTempVar langType
  -- add to the symbol table
  function <- gets currentFunction
  let symTable = IR.symbols function
  let newSymTable = Map.insert (IR.varName var) (IR.varType var) symTable
  modify $ \state -> state {currentFunction = function {IR.symbols = newSymTable}}
  return var

getNewLabel :: String -> GenM Label
getNewLabel prefix = do
  label <- gets nextLabel
  modify $ \state -> state {nextLabel = label + 1}
  return $ prefix ++ "_" ++ show label

emit :: IR.Instr -> GenM ()
emit instr = do
  block <- gets currentBlock
  let newBlock = block {IR.instrs = instr : IR.instrs block}
  modify $ \state -> state {currentBlock = newBlock}

getNewGlobal :: IR.Value -> GenM Arg -- @TODO currently globals are read only
getNewGlobal value = do
  global <- gets nextGlobal
  modify $ \state -> state {nextGlobal = global + 1}
  let newGlobal = "_global_" ++ show global
  modify $ \state -> state {gconstants = Map.insert newGlobal value $ gconstants state}
  return $ IR.Global (IR.Variable newGlobal (IR.typeFromValue value))

localVariableLookup :: Ident -> GenM IR.Variable
localVariableLookup ident = do
  function <- gets currentFunction
  let symTable = IR.symbols function
  case Map.lookup ident symTable of
    Just var -> return $ IR.Variable ident var
    Nothing -> error $ "Variable " ++ ident ++ " not found in the symbol table"

startNewBasicBlock :: Label -> GenM ()
startNewBasicBlock newLabel = do
  endBasicBlock
  let newCurrentBlock = IR.BasicBlock newLabel [] [] []
  modify $ \state -> state {currentBlock = newCurrentBlock}

endBasicBlock :: GenM ()
endBasicBlock = do
  block <- gets currentBlock
  function <- gets currentFunction
  let newCurrentBlock = block {IR.instrs = reverse $ IR.instrs block}
  let oldBlocks = IR.blocks function
  let newBlocks = Map.insert (IR.label block) newCurrentBlock oldBlocks
  let oldLabelOrder = IR.basicBlockOrder function
  let newLabelOrder = IR.label block : oldLabelOrder
  let newFunction = function {IR.blocks = newBlocks, IR.basicBlockOrder = newLabelOrder}
  let emptyBlock = IR.BasicBlock "" [] [] []
  modify $ \state -> state {currentFunction = newFunction, currentBlock = emptyBlock}


--------------------------------------- Post processing ---------------------------------------

functionPostProcess :: IR.Function -> IR.Function
functionPostProcess function = removeUnusedBlocks $ removeEmptyBlocks $ processBasicBlocks $ fixLabelOrder function
  where
    fixLabelOrder :: IR.Function -> IR.Function
    fixLabelOrder function = function {IR.basicBlockOrder = newLabelOrder}
      where
        newLabelOrder = reverse $ IR.basicBlockOrder function

processBasicBlocks :: IR.Function -> IR.Function
processBasicBlocks = execState _processBasicBlocks
  where
    _processBasicBlocks :: State IR.Function ()
    _processBasicBlocks = do
      function <- get
      mapM_ processBlock $ Map.elems $ IR.blocks function
    processBlock :: IR.BasicBlock -> State IR.Function ()
    processBlock block = do
      let successors = getSuccessorsFromInstr block
      let predecessor = IR.label block
      mapM_ (addSuccessorPredecessor predecessor) successors
    getSuccessorsFromInstr :: IR.BasicBlock -> [Label]
    getSuccessorsFromInstr block =
      case reverse $ IR.instrs block of
        (IR.Branch _ label1 label2 : _) -> [label1, label2]
        (IR.GoTo label : _) -> [label]
        _ -> []
    addSuccessorPredecessor :: Label -> Label -> State IR.Function ()
    addSuccessorPredecessor predLabel succLabel = do
      function <- get
      let predBlock = IR.blocks function Map.! predLabel
      let succBlock = IR.blocks function Map.! succLabel
      let newPredBlock = predBlock {IR.successors = succLabel : IR.successors predBlock}
      let newSuccBlock = succBlock {IR.predecessors = predLabel : IR.predecessors succBlock}
      modify $ \function -> function {IR.blocks = Map.insert predLabel newPredBlock $ Map.insert succLabel newSuccBlock $ IR.blocks function}

-- Remove empty blocks
removeEmptyBlocks :: IR.Function -> IR.Function
-- while there are empty blocks, keep removing them
removeEmptyBlocks function =
  let (function', removed) = _removeEmptyBlocks function
   in (if removed then removeEmptyBlocks function' else function')

_removeEmptyBlocks :: IR.Function -> (IR.Function, Bool)
_removeEmptyBlocks function =
  let emptyBlocks = Map.keys $ Map.filter (null . IR.instrs) $ IR.blocks function
   in (execState (mapM_ removeEmptyBlock emptyBlocks) function, not $ null emptyBlocks)
  where
    removeEmptyBlock :: Label -> State IR.Function ()
    removeEmptyBlock label = do
      function <- get
      let basicBlocks = IR.blocks function
          block = basicBlocks Map.! label
          predecessorBlocks = map (basicBlocks Map.!) $ IR.predecessors block
          updatedBlockList = map (removeJumpBlock label) predecessorBlocks
          updatedBlocksMap = Map.delete label $ foldl (\blocks block -> Map.insert (IR.label block) block blocks) basicBlocks updatedBlockList
          newLabelOrder = filter (/= label) $ IR.basicBlockOrder function
      modify $ \function -> function {IR.blocks = updatedBlocksMap, IR.basicBlockOrder = newLabelOrder}

    removeJumpBlock :: Label -> IR.BasicBlock -> IR.BasicBlock
    removeJumpBlock succLabel predBlock = predBlock {IR.instrs = newInstrs, IR.successors = newSuccesors}
      where
        newSuccesors = filter (/= succLabel) $ IR.successors predBlock
        newInstrs = case reverse $ IR.instrs predBlock of
          [] -> []
          (instr : rest) ->
            case removeJumpInstr succLabel instr of
              Just instr -> reverse rest ++ [instr]
              Nothing -> reverse rest
    removeJumpInstr :: IR.Label -> IR.Instr -> Maybe IR.Instr
    removeJumpInstr label instr =
      case instr of
        IR.Branch _ l1 l2 ->
          if l1 == label
            then Just $ IR.GoTo l2
            else
              if l2 == label
                then Just $ IR.GoTo l1
                else Just instr
        IR.GoTo l -> if l == label then Nothing else Just instr
        instr -> Just instr

removeUnusedBlocks :: IR.Function -> IR.Function
removeUnusedBlocks function =
  let blocksList = Map.toList (IR.blocks function)
      firstBlock = head (IR.basicBlockOrder function)
      isUnused = \block -> null (IR.predecessors block) && IR.label block /= firstBlock
      unusedBlocks = filter isUnused (map snd blocksList)
      unusedBlocksLabels = map IR.label unusedBlocks
      newBlocks = foldl (\blocks block -> Map.delete (IR.label block) blocks) (IR.blocks function) unusedBlocks
      newBlocks' = Map.map (\block -> block {IR.predecessors = filter (\label -> not $ elem label unusedBlocksLabels) (IR.predecessors block)}) newBlocks
      newBlocks'' = Map.map (\block -> block {IR.successors = filter (\label -> not $ elem label unusedBlocksLabels) (IR.successors block)}) newBlocks'
      newLabelOrder = filter (\label -> not $ elem label unusedBlocksLabels) (IR.basicBlockOrder function)
    in function {IR.blocks = newBlocks'', IR.basicBlockOrder = newLabelOrder}


-- LLVM doesn't accept a basic block that doesn't end with a branch instruction
-- like "goto", "branch", "return", etc. But in the source code we can have a function that returns void 
-- and doesn't have a return statement. In this case, we need to add a return instruction at the end of the function.
-- addVoidReturns :: IR.Function -> IR.Function
