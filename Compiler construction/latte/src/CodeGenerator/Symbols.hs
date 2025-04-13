module CodeGenerator.Symbols (functionAddSymbolIDs) where

import qualified Bnfc.Abs as Abs
import Control.Monad.State
import qualified Data.Map as Map
import TypeCheck.LangTypes (LangType, fromAstType)

type Ident = String

-- @TODO Fresh temporary class for all the states
data SymbolTableState = TypeSymbolState
  { symbolTable :: Map.Map Ident Ident,
    allSymbols :: Map.Map Ident LangType,
    nextID :: Integer
  }

initialSymbolTableState :: SymbolTableState
initialSymbolTableState = TypeSymbolState Map.empty Map.empty 0

newSymbol :: String -> LangType -> State SymbolTableState Ident
newSymbol name langType = do
  state <- get
  let id = nextID state
  let symbolTable_ = symbolTable state
  let allSymbols_ = allSymbols state
  let newSymbolName = name ++ "_" ++ show id
  let newSymbolTable = Map.insert name newSymbolName symbolTable_
  let newAllSymbols = Map.insert newSymbolName langType allSymbols_
  put $ state {nextID = id + 1, symbolTable = newSymbolTable, allSymbols = newAllSymbols}
  return $ newSymbolName

lookupSymbol :: String -> State SymbolTableState Ident
lookupSymbol name = do
  state <- get
  let symbolTable_ = symbolTable state
  return (symbolTable_ Map.! name)

addArguments :: [(String, LangType)] -> State SymbolTableState [String]
addArguments args = do
  mapM (uncurry newSymbol) args

functionAddSymbolIDs :: Abs.TopDef -> (Map.Map Ident LangType, Abs.TopDef)
functionAddSymbolIDs topDef = (allSymbols state, topDef')
  where
    (topDef', state) = runState (topDefAddSymbolIDs topDef) initialSymbolTableState

topDefAddSymbolIDs :: Abs.TopDef -> State SymbolTableState Abs.TopDef
topDefAddSymbolIDs (Abs.FnDef pos ty ident args block) = do
  argIdents <- addArguments (map (\(Abs.Arg _ ty (Abs.Ident arg)) -> (arg, fromAstType ty)) args)
  let args' = map (\((Abs.Arg pos ty _), newIdent) -> Abs.Arg pos ty (Abs.Ident newIdent)) (zip args argIdents)
  block' <- blockAddSymbolIDs block
  return $ Abs.FnDef pos ty ident args' block'
topDefAddSymbolIDs topDef = do
  return topDef

blockAddSymbolIDs :: Abs.Block -> State SymbolTableState Abs.Block
blockAddSymbolIDs (Abs.Block pos stmts) = do
  stmts' <- withRestoredSymbolTable $ mapM stmtAddSymbolIDs stmts
  return $ Abs.Block pos stmts'

stmtAddSymbolIDs :: Abs.Stmt -> State SymbolTableState Abs.Stmt
stmtAddSymbolIDs (Abs.BStmt pos1 block) = do
  block' <- blockAddSymbolIDs block
  return $ Abs.BStmt pos1 block'
stmtAddSymbolIDs (Abs.Decl pos ty items) = do
  items' <- mapM (itemAddSymbolIDs (fromAstType ty)) items
  return $ Abs.Decl pos ty items'
stmtAddSymbolIDs (Abs.Ass pos lvalue expr) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  expr' <- exprAddSymbolIDs expr
  return $ Abs.Ass pos lvalue' expr'
stmtAddSymbolIDs (Abs.Incr pos lvalue) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  return $ Abs.Incr pos lvalue'
stmtAddSymbolIDs (Abs.Decr pos lvalue) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  return $ Abs.Decr pos lvalue'
stmtAddSymbolIDs (Abs.Ret pos expr) = do
  expr' <- exprAddSymbolIDs expr
  return $ Abs.Ret pos expr'
stmtAddSymbolIDs (Abs.Cond pos expr stmt) = do
  expr' <- exprAddSymbolIDs expr
  stmt' <- stmtAddSymbolIDs stmt
  return $ Abs.Cond pos expr' stmt'
stmtAddSymbolIDs (Abs.CondElse pos expr stmt1 stmt2) = do
  expr' <- exprAddSymbolIDs expr
  stmt1' <- stmtAddSymbolIDs stmt1
  stmt2' <- stmtAddSymbolIDs stmt2
  return $ Abs.CondElse pos expr' stmt1' stmt2'
stmtAddSymbolIDs (Abs.While pos expr stmt) = do
  expr' <- exprAddSymbolIDs expr
  stmt' <- stmtAddSymbolIDs stmt
  return $ Abs.While pos expr' stmt'
stmtAddSymbolIDs (Abs.SExp pos expr) = do
  expr' <- exprAddSymbolIDs expr
  return $ Abs.SExp pos expr'
stmtAddSymbolIDs (Abs.For pos ty var arrayExpr stmt) = stmtAddSymbolIDs otherStmt
  where
    otherStmt =
      Abs.BStmt
        pos
        ( Abs.Block
            pos
            [ -- Initialization: i = 0
              Abs.Decl pos (Abs.Int pos) [Abs.Init pos (Abs.Ident "__i__") (Abs.ELitInt pos 0)],
              -- Declaration var
              Abs.Decl pos ty [Abs.NoInit pos var],
              -- Declaration array
              Abs.Decl pos (Abs.Arr pos ty) [Abs.Init pos (Abs.Ident "__array__") arrayExpr],
              -- While loop
              Abs.While
                pos
                -- Condition: i < length(array)
                ( Abs.ERel
                    pos
                    (Abs.ELValue pos (Abs.LVar pos (Abs.Ident "__i__")))
                    (Abs.LTH pos)
                     (Abs.ELValue pos (Abs.LAttr pos (Abs.LVar pos (Abs.Ident "__array__")) (Abs.Ident "length")))
                )
                ( Abs.BStmt
                    pos
                    ( Abs.Block
                        pos
                        [ -- Assign var = array[i]
                          Abs.Ass
                            pos
                            (Abs.LVar pos var)
                            (Abs.ELValue pos (Abs.LArr pos (Abs.LVar pos (Abs.Ident "__array__")) (Abs.ELValue pos (Abs.LVar pos (Abs.Ident "__i__"))))),
                          -- Increment: i++
                          Abs.Incr pos (Abs.LVar pos (Abs.Ident "__i__")),
                          -- Original loop body
                          stmt
                        ]
                    )
                )
            ]
        )
stmtAddSymbolIDs stmt = return stmt

lvalueAddSymbolIDs :: Abs.LValue -> State SymbolTableState Abs.LValue
lvalueAddSymbolIDs (Abs.LVar pos (Abs.Ident ident)) = do
  ident' <- lookupSymbol ident
  return $ Abs.LVar pos (Abs.Ident ident')
lvalueAddSymbolIDs (Abs.LArr pos lvalue expr) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  expr' <- exprAddSymbolIDs expr
  return $ Abs.LArr pos lvalue' expr'
lvalueAddSymbolIDs (Abs.LAttr pos lvalue attr) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  return $ Abs.LAttr pos lvalue' attr

itemAddSymbolIDs :: LangType -> Abs.Item -> State SymbolTableState Abs.Item
itemAddSymbolIDs ty (Abs.NoInit pos (Abs.Ident ident)) = do
  ident' <- newSymbol ident ty
  return $ Abs.NoInit pos (Abs.Ident ident')
itemAddSymbolIDs ty (Abs.Init pos (Abs.Ident ident) expr) = do
  expr' <- exprAddSymbolIDs expr
  ident' <- newSymbol ident ty
  return $ Abs.Init pos (Abs.Ident ident') expr'

exprAddSymbolIDs :: Abs.Expr -> State SymbolTableState Abs.Expr
exprAddSymbolIDs (Abs.ELValue pos lvalue) = do
  lvalue' <- lvalueAddSymbolIDs lvalue
  return $ Abs.ELValue pos lvalue'
exprAddSymbolIDs (Abs.EApp pos (Abs.Ident ident) exprs) = do
  exprs' <- mapM exprAddSymbolIDs exprs
  return $ Abs.EApp pos (Abs.Ident ident) exprs'
exprAddSymbolIDs (Abs.Neg pos expr) = do
  expr' <- exprAddSymbolIDs expr
  return $ Abs.Neg pos expr'
exprAddSymbolIDs (Abs.Not pos expr) = do
  expr' <- exprAddSymbolIDs expr
  return $ Abs.Not pos expr'
exprAddSymbolIDs (Abs.EMul pos expr1 mulOp expr2) = do
  expr1' <- exprAddSymbolIDs expr1
  expr2' <- exprAddSymbolIDs expr2
  return $ Abs.EMul pos expr1' mulOp expr2'
exprAddSymbolIDs (Abs.EAdd pos expr1 addOp expr2) = do
  expr1' <- exprAddSymbolIDs expr1
  expr2' <- exprAddSymbolIDs expr2
  return $ Abs.EAdd pos expr1' addOp expr2'
exprAddSymbolIDs (Abs.ERel pos expr1 relOp expr2) = do
  expr1' <- exprAddSymbolIDs expr1
  expr2' <- exprAddSymbolIDs expr2
  return $ Abs.ERel pos expr1' relOp expr2'
exprAddSymbolIDs (Abs.EAnd pos expr1 expr2) = do
  expr1' <- exprAddSymbolIDs expr1
  expr2' <- exprAddSymbolIDs expr2
  return $ Abs.EAnd pos expr1' expr2'
exprAddSymbolIDs (Abs.EOr pos expr1 expr2) = do
  expr1' <- exprAddSymbolIDs expr1
  expr2' <- exprAddSymbolIDs expr2
  return $ Abs.EOr pos expr1' expr2'
exprAddSymbolIDs (Abs.ENewArray pos ty expr) = do
  expr' <- exprAddSymbolIDs expr
  return $ Abs.ENewArray pos ty expr'
exprAddSymbolIDs expr = return expr

withRestoredSymbolTable :: State SymbolTableState a -> State SymbolTableState a
withRestoredSymbolTable action = do
  oldSymTable <- gets symbolTable
  result <- action
  modify $ \state -> state {symbolTable = oldSymTable}
  return result