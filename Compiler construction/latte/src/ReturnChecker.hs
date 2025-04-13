module ReturnChecker where
import Bnfc.Abs
import Errors.Definitions
import Control.Monad.Except
import BnfcUtils (getIdent)

data ReturnCheckErrors =
    NoReturnStatementError TopDef String

instance Error ReturnCheckErrors where
    title (NoReturnStatementError _ _) = "No return statement"
    description (NoReturnStatementError _ ident) = "Function " ++ ident ++ " does not have a return statement in every branch"
    codeFragment (NoReturnStatementError topdef _) = TopDefFragment topdef

runReturnCheck :: Program -> ExceptT String IO ()
runReturnCheck program = withExceptT showError $ checkProgramReturn program

checkProgramReturn :: Program -> ExceptT ReturnCheckErrors IO ()
checkProgramReturn (Program _ topDefs) = do
  mapM_ checkTopDefReturn topDefs

checkTopDefReturn :: TopDef -> ExceptT ReturnCheckErrors IO ()
checkTopDefReturn topdef@(FnDef _ retType ident _ block) = do
  case retType of
    Void _ -> return ()
    _ -> do
      let res = isReturnGuaranteedBlock block
      if res then return ()
      else throwError $ NoReturnStatementError topdef (getIdent ident)
checkTopDefReturn _ = return ()


isReturnGuaranteedBlock :: Block -> Bool
isReturnGuaranteedBlock (Block _ stmts) = isReturnGuarantedStmt stmts

-- Checks if every branch of the statements has a return statement
isReturnGuarantedStmt :: [Stmt] -> Bool
isReturnGuarantedStmt [] = False
isReturnGuarantedStmt (stmt:rest) = case stmt of
  Ret _ _ -> True
  VRet _ -> True
  BStmt _ block ->
    let res = isReturnGuaranteedBlock block
      in (res || isReturnGuarantedStmt rest)
  Cond _ expr stmt ->
    let resultIf = isReturnGuarantedStmt [stmt]
        resultAfterIf = isReturnGuarantedStmt rest
      in case expr of
        ELitTrue _ -> resultIf
        ELitFalse _ -> resultAfterIf
        _ -> resultAfterIf
  CondElse _ expr stmt1 stmt2 ->
      let
          resultIf = isReturnGuarantedStmt [stmt1]
          resultElse = isReturnGuarantedStmt [stmt2]
      in case expr of
        ELitTrue _ -> resultIf
        ELitFalse _ -> resultElse
        _ -> (resultIf && resultElse) || isReturnGuarantedStmt rest
  While _ epxr stmt ->
    let resultAfter = isReturnGuarantedStmt rest
      in case epxr of
        ELitFalse _ -> resultAfter
        ELitTrue _ -> searchLoopExitStmt [stmt]
        _ -> resultAfter
  _ -> isReturnGuarantedStmt rest

searchLoopExitBlock :: Block -> Bool
searchLoopExitBlock (Block _ stmts) = searchLoopExitStmt stmts

searchLoopExitStmt :: [Stmt] -> Bool
searchLoopExitStmt [] = False
searchLoopExitStmt (stmt:rest) = case stmt of
  (BStmt _ block) -> searchLoopExitBlock block
  (Ret _ _) -> True
  (VRet _) -> True
  (Cond _ expr stmt) ->
    let restultIf = searchLoopExitStmt [stmt]
        resultAfterIf = searchLoopExitStmt rest
      in case expr of
        ELitFalse _ -> resultAfterIf
        _ -> restultIf || resultAfterIf
  (CondElse _ expr stmt1 stmt2) ->
    let resultIf = searchLoopExitStmt [stmt1]
        resultElse = searchLoopExitStmt [stmt2]
      in case expr of
        ELitTrue _ -> resultIf
        ELitFalse _ -> resultElse
        _ -> resultIf || resultElse
  (While _ expr stmt) ->
    let resultAfter = searchLoopExitStmt [stmt]
        resultInside = searchLoopExitStmt [stmt]
      in case expr of
        ELitFalse _ -> resultAfter
        ELitTrue _ -> resultInside
        _ -> resultInside || resultAfter
  _ -> searchLoopExitStmt rest

