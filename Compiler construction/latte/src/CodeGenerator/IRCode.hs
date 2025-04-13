module CodeGenerator.IRCode where

import qualified Data.Map as Map
import qualified TypeCheck.LangTypes as LangTypes
import qualified TypeCheck.LangTypes as LangType

-- IR quadruples code
data BinaryOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | CmpOp CompareOperator
  deriving (Eq, Ord)

data UnaryOperator
  = Neg | BitCast
  deriving (Eq, Ord)

data CompareOperator
  = Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Eq, Ord)

data Value
  = LiteralInt Integer
  | LiteralBool Bool
  | LiteralString String
  | Null LangTypes.LangType
  deriving (Eq, Ord)

data Arg
  = Ident Variable
  | Literal Value
  | Global Variable
  | Void
  deriving (Eq, Ord)

type Label = String

type Ident = String

data Variable = Variable
  { varName :: Ident,
    varType :: LangTypes.LangType
  }
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable ident _) = ident

data Instr
  = BinOp Variable Arg BinaryOperator Arg -- arg1 <- arg2 operator arg3
  | UnOp Variable UnaryOperator Arg -- arg1 <- operator arg2
  | Assign Variable Arg -- arg1 <- arg2
  | Branch Arg Label Label -- branch arg1 label1 label2
  | GoTo Label -- goto label
  | Call Variable Ident [Arg] -- arg1 <- call ident(arg2, arg3, ...)
  | Return Arg -- return arg1
  | VReturn
  | Phi Variable [(Label, Maybe Arg)] -- arg1 <- phi [(arg2, label1), (arg3, label2), ...]
  | GEP Variable Arg [Arg]

data BasicBlock = BasicBlock
  { label :: Label,
    instrs :: [Instr],
    predecessors :: [Label],
    successors :: [Label]
  }

data Function = Function
  { name :: Ident,
    retType :: LangTypes.LangType,
    args :: [Variable],
    blocks :: Map.Map Label BasicBlock,
    symbols :: Map.Map Ident LangTypes.LangType, -- handles scopes
    basicBlockOrder :: [Label]
  }

data Program = Program
  { functions :: Map.Map Ident Function,
    gconstants :: Map.Map Ident Value,
    classes :: Map.Map Ident LangType.ClassTypeDef
  }

------------------------ Helper functions ------------------------

isBinOpSymmetric :: BinaryOperator -> Bool
isBinOpSymmetric Sub = False
isBinOpSymmetric Div = False
isBinOpSymmetric Mod = False
isBinOpSymmetric (CmpOp Lt) = False
isBinOpSymmetric (CmpOp Le) = False
isBinOpSymmetric (CmpOp Gt) = False
isBinOpSymmetric (CmpOp Ge) = False
isBinOpSymmetric _ = True

modifyBlock :: (BasicBlock -> BasicBlock) -> Label -> Function -> Function
modifyBlock f label' function =
  let block = blocks function Map.! label'
   in function {blocks = Map.insert label' (f block) (blocks function)}

typeFromArg :: Arg -> LangTypes.LangType
typeFromArg (Ident (Variable _ ty)) = ty
typeFromArg (Global (Variable _ ty)) = ty
typeFromArg (Literal value) = typeFromValue value
typeFromArg Void = LangTypes.VoidType

typeFromValue :: Value -> LangTypes.LangType
typeFromValue (LiteralInt _) = LangTypes.IntType
typeFromValue (LiteralBool _) = LangTypes.BoolType
typeFromValue (LiteralString s) = LangTypes.SizedString (fromIntegral $ length s)
typeFromValue (Null ty) = LangTypes.Pointer ty

intArg :: Integer -> Arg
intArg i = Literal (LiteralInt i)

boolArg :: Bool -> Arg
boolArg b = Literal (LiteralBool b)

stringArg :: String -> Arg
stringArg s = Literal (LiteralString s)

defaultArg :: LangTypes.LangType -> Arg
defaultArg LangTypes.IntType = intArg 0
defaultArg LangTypes.BoolType = boolArg False
defaultArg LangTypes.StringType = stringArg ""
defaultArg _ = error "Unsupported type"

instance Show Instr where
  show (BinOp arg1 arg2 operator arg3) = show arg1 ++ " = " ++ show arg2 ++ " " ++ show operator ++ " " ++ show arg3
  show (UnOp arg1 operator arg2) = show arg1 ++ " = " ++ show operator ++ " " ++ show arg2
  show (Assign arg1 arg2) = show arg1 ++ " = " ++ show arg2
  show (Branch arg1 label1 label2) = "if " ++ show arg1 ++ " goto " ++ label1 ++ " else goto " ++ label2
  show (GoTo label) = "goto " ++ label
  show (Call arg1 ident args) = show arg1 ++ " = call " ++ ident ++ "(" ++ unwords (map show args) ++ ")"
  show (Return arg1) = "return " ++ show arg1
  show VReturn = "return"
  show (Phi arg1 args) = show arg1 ++ " = phi " ++ "(" ++ unwords (map (\(label, arg) -> "(" ++ showArg arg ++ ", " ++ label ++ ")") args) ++ ")"
    where
      showArg (Just arg) = show arg
      showArg Nothing = "undef"
  show (GEP result source offset) = show result ++ " = getelementptr " ++ show source ++ ", " ++ show offset

instance Show BinaryOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show (CmpOp operator) = show operator

instance Show UnaryOperator where
  show Neg = "-"
  show BitCast = "bitcast"

instance Show CompareOperator where
  show Eq = "=="
  show Ne = "!="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

instance Show Arg where
  show (Ident var) = show var
  show (Literal value) = show value
  show (Global var) = "@" ++ show var
  show Void = "void"

instance Show Value where
  show (LiteralInt i) = show i
  show (LiteralBool b) = show b
  show (LiteralString s) = s
  show (Null t) = "(" ++ show t ++ ")null"
