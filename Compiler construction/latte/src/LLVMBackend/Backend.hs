module LLVMBackend.Backend where

import CodeGenerator.IRCode (typeFromArg)
import qualified CodeGenerator.IRCode as IR
import Data.List (intercalate)
import qualified Data.Map as Map
import LLVMBackend.CommonSubexprElimination (eliminateSubExpr)
import LLVMBackend.SSA (programToSSA)
import qualified TypeCheck.LangTypes as IR
import qualified TypeCheck.LangTypes as LangType
import qualified TypeCheck.LangTypes as LangTypes
import Utilities (escapesToHexEscapes)

type LLVMInstruction = String

data LLVMFunction = LLVMFunction
  { name :: String,
    returnType :: String,
    args :: [(String, String)],
    body :: [LLVMInstruction]
  }

standardLibararyDeclaration :: String
standardLibararyDeclaration =
       "@_dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
    ++ "@_d   = internal constant [3 x i8] c\"%d\\00\"\n"
    ++ "@_s  = internal constant [3 x i8] c\"%s\\00\"\n"
    ++ "@runtime_error = private constant [15 x i8] c\"Runtime error\\0A\\00\"\n"
    ++ "\n"
    ++ "declare i32 @printf(i8*, ...)\n"
    ++ "declare i32 @scanf(i8*, ...)\n"
    ++ "declare i32 @puts(i8*)\n"
    ++ "declare void @exit(i32)\n"
    ++ "\n"
    ++ "define void @printInt(i32 %x) {\n"
    ++ "   %t0 = getelementptr [4 x i8], [4 x i8]* @_dnl, i32 0, i32 0\n"
    ++ "   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n"
    ++ "   ret void\n"
    ++ "}\n\n"
    ++ "define void @printString(i8* %s) {\n"
    ++ "    call i32 @puts(i8* %s)\n"
    ++ "    ret void\n"
    ++ "}\n\n"
    ++ "define i32 @readInt() {\n"
    ++ "    %res = alloca i32\n"
    ++ "    %t1 = getelementptr [3 x i8], [3 x i8]* @_d, i32 0, i32 0\n"
    ++ "    call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)\n"
    ++ "    %t2 = load i32, i32* %res\n"
    ++ "    ret i32 %t2\n"
    ++ "}\n\n"
    ++ "define i8* @readString() {\n"
    ++ "    %res = call i8* @malloc(i32 1024)\n"
    ++ "    %t1 = getelementptr [3 x i8], [3 x i8]* @_s, i32 0, i32 0\n"
    ++ "    call i32 (i8*, ...) @scanf(i8* %t1, i8* %res)\n"
    ++ "    ret i8* %res\n"
    ++ "}\n\n"
    ++ "define void @error() {\n"
    ++ "    %msg = getelementptr [15 x i8], [15 x i8]* @runtime_error, i32 0, i32 0\n"
    ++ "    call i32 @puts(i8* %msg)\n"
    ++ "    call void @exit(i32 1)\n"
    ++ "    unreachable\n"
    ++ "}\n"
    ++ "\n"
    ++ "declare i8* @malloc(i32)\n"
    ++ "declare i32 @strlen(i8*)\n"
    ++ "declare void @memcpy(i8*, i8*, i32)\n"
    ++ "declare i32 @strcmp(i8*, i8*)\n"
    ++ "\n"
    ++ "define i8* @_concat_strings(i8* %str1, i8* %str2) {\n"
    ++ "entry:\n"
    ++ "    %len1 = call i32 @strlen(i8* %str1)\n"
    ++ "    %len2 = call i32 @strlen(i8* %str2)\n"
    ++ "    %sum_len = add i32 %len1, %len2\n"
    ++ "    %total_len = add i32 %sum_len, 1\n"
    ++ "    %new_str = call i8* @malloc(i32 %total_len)\n"
    ++ "    call void @memcpy(i8* %new_str, i8* %str1, i32 %len1)\n"
    ++ "    %offset = getelementptr i8, i8* %new_str, i32 %len1\n"
    ++ "    call void @memcpy(i8* %offset, i8* %str2, i32 %len2)\n"
    ++ "    %end = getelementptr i8, i8* %new_str, i32 %sum_len\n"
    ++ "    store i8 0, i8* %end\n"
    ++ "    ret i8* %new_str\n"
    ++ "}\n\n"

generate :: IR.Program -> String
generate program = classes ++ globals ++ programString ++ standardLibararyDeclaration
  where
    programSSA = programToSSA program
    programSSA' = eliminateSubExpr programSSA
    globals = generateGlobalsConsts programSSA'
    classes = generateClasses programSSA'
    functions = map toLLVMFunction (Map.elems (IR.functions programSSA'))
    functionStrings = map generateFunction functions
    programString = intercalate "\n\n" functionStrings ++ "\n\n"

generateClasses :: IR.Program -> String
generateClasses program = intercalate "\n" classStrings ++ "\n\n"
  where
    classStrings = map generateClass (Map.toList (IR.classes program))

generateClass :: (IR.Ident, IR.ClassTypeDef) -> String
generateClass (_name, typeDef) =
  "%" ++ _name ++ " = type {\n" ++ intercalate ",\n" (map generateClassAttr typeDef) ++ "\n}"
  where
    generateClassAttr (_, ty) = "    " ++ toLLVMType ty

generateGlobalsConsts :: IR.Program -> String
generateGlobalsConsts program = intercalate "\n" globalStrings ++ "\n\n"
  where
    globalStrings = map (uncurry generateGlobalConst) (Map.toList (IR.gconstants program))

generateGlobalConst :: IR.Ident -> IR.Value -> String
generateGlobalConst ident value =
  "@" ++ ident ++ " = internal constant " ++ toLLVMTypeValue value ++ " " ++ toLLVMLiteral value

generateFunction :: LLVMFunction -> String
generateFunction function =
  "define "
    ++ returnType function
    ++ " @"
    ++ name function
    ++ "("
    ++ (intercalate ", " (map (\(argType, argName) -> argType ++ " " ++ _toLLVMRegister argName) (args function)))
    ++ ") {\n"
    ++ (intercalate "\n" (body function))
    ++ "\n"
    ++ "}"

toLLVMFunction :: IR.Function -> LLVMFunction
toLLVMFunction (IR.Function name retType args blocks symbols labelOrder) = LLVMFunction name (toLLVMType retType) argsLLVM body
  where
    argsLLVM = map (\var -> ((toLLVMType (IR.varType var)), (IR.varName var))) args
    body = concatMap toLLVMBasicBlock (map (\label -> blocks Map.! label) $ labelOrder)

toLLVMBasicBlock :: IR.BasicBlock -> [LLVMInstruction]
toLLVMBasicBlock (IR.BasicBlock label instrs _ _) =
  [label ++ ":"] ++ (map (("    " ++) . toLLVMInstruction) instrs)

toLLVMInstruction :: IR.Instr -> LLVMInstruction
toLLVMInstruction (IR.BinOp arg1 arg2 (IR.CmpOp cmpOp) arg3) =
  toLLVMRegister arg1 ++ " = icmp " ++ toLLVMCmpOp cmpOp ++ " " ++ toLLVMTypeArg arg2 ++ " " ++ toLLVMArg arg2 ++ ", " ++ toLLVMArg arg3
  where
    toLLVMCmpOp :: IR.CompareOperator -> String
    toLLVMCmpOp IR.Eq = "eq"
    toLLVMCmpOp IR.Ne = "ne"
    toLLVMCmpOp IR.Lt = "slt"
    toLLVMCmpOp IR.Le = "sle"
    toLLVMCmpOp IR.Gt = "sgt"
    toLLVMCmpOp IR.Ge = "sge"
toLLVMInstruction (IR.BinOp arg1 arg2 operator arg3) =
  toLLVMRegister arg1 ++ " = " ++ toLLVMBinOp operator ++ " i32 " ++ toLLVMArg arg2 ++ ", " ++ toLLVMArg arg3
  where
    toLLVMBinOp :: IR.BinaryOperator -> String
    toLLVMBinOp IR.Add = "add"
    toLLVMBinOp IR.Sub = "sub"
    toLLVMBinOp IR.Mul = "mul"
    toLLVMBinOp IR.Div = "sdiv"
    toLLVMBinOp IR.Mod = "srem"
toLLVMInstruction (IR.UnOp arg1 IR.Neg arg2) =
  toLLVMInstruction (IR.BinOp arg1 (IR.intArg 0) IR.Sub arg2)
toLLVMInstruction (IR.UnOp arg1 IR.BitCast arg2) =
  toLLVMRegister arg1 ++ " = bitcast " ++ toLLVMTypeArg arg2 ++ " " ++ toLLVMArg arg2 ++ " to " ++ toLLVMTypeVariable arg1
-- toLLVMInstruction (IR.UnOp arg1 IR.Load arg2) =
--   toLLVMRegister arg1 ++ " = load " ++ toLLVMTypeVariable arg1 ++  " ," ++ toLLVMTypeArg arg2 ++ " " ++ toLLVMArg arg2
toLLVMInstruction (IR.Assign arg1 arg2) = error "Assign instruction not allowed in LLVM IR"
toLLVMInstruction (IR.Branch arg1 label1 label2) = "br i1 " ++ toLLVMArg arg1 ++ ", label %" ++ label1 ++ ", label %" ++ label2
toLLVMInstruction (IR.GoTo label) = "br label %" ++ label
-- Special functctions for "load" and "store" instructions
toLLVMInstruction (IR.Call retVal "__load__" [arg]) =
  toLLVMRegister retVal ++ " = load " ++ toLLVMTypeVariable retVal ++ ", " ++ toLLVMTypeArg arg ++ " " ++ toLLVMArg arg
toLLVMInstruction (IR.Call _ "__load__" _) = error "Invalid number of arguments for __load__"
toLLVMInstruction (IR.Call _ "__store__" [arg1, arg2]) =
  "store " ++ toLLVMTypeArg arg1 ++ " " ++ toLLVMArg arg1 ++ ", " ++ toLLVMTypeArg arg2 ++ " " ++ toLLVMArg arg2
toLLVMInstruction (IR.Call _ "__store__" _) = error "Invalid number of arguments for __store__"
toLLVMInstruction (IR.Call arg1 ident args) =
  case IR.varType arg1 of
    LangTypes.VoidType -> "call void @" ++ ident ++ "(" ++ (intercalate ", " (map (\arg -> toLLVMTypeArg arg ++ " " ++ toLLVMArg arg) args)) ++ ")"
    _ -> toLLVMRegister arg1 ++ " = call " ++ toLLVMTypeVariable arg1 ++ " @" ++ ident ++ "(" ++ (intercalate ", " (map (\arg -> toLLVMTypeArg arg ++ " " ++ toLLVMArg arg) args)) ++ ")"
toLLVMInstruction (IR.Return arg1) = "ret " ++ toLLVMTypeArg arg1 ++ " " ++ toLLVMArg arg1
toLLVMInstruction IR.VReturn = "ret void"
toLLVMInstruction (IR.Phi arg1 args) =
  toLLVMRegister arg1 ++ " = phi " ++ toLLVMTypeVariable arg1 ++ " " ++ (intercalate ", " (map _toPhiArg args))
  where
    _toPhiArg :: (IR.Label, Maybe IR.Arg) -> String
    _toPhiArg (label, Just arg) = "[" ++ toLLVMArg arg ++ ", %" ++ label ++ "]"
    _toPhiArg (label, Nothing) = "[ undef, %" ++ label ++ "]"
-- toLLVMInstruction (IR.GetGlobalStringPointer arg1 arg2 str) =
--   let strLen = length str
--       arrType = "[" ++ show (strLen + 1) ++ " x i8]"
--   in
--   toLLVMRegister arg1 ++ " = getelementptr " ++ arrType ++ ", " ++ arrType ++ "* " ++ toLLVMArg arg2 ++ ", i32 0, i32 0"
toLLVMInstruction (IR.GEP result source offsets) =
  let sourceType = IR.typeFromArg source
      llvmOffsets = intercalate ", " $ map (\arg -> toLLVMTypeArg arg ++ " " ++ toLLVMArg arg) offsets
   in toLLVMRegister result ++ " = getelementptr " ++ toLLVMBaseType sourceType ++ ", " ++ toLLVMType sourceType ++ " " ++ toLLVMArg source ++ ", " ++ llvmOffsets

toLLVMArg :: IR.Arg -> String
toLLVMArg (IR.Ident var) = toLLVMRegister var
toLLVMArg (IR.Literal int) = toLLVMLiteral int
toLLVMArg (IR.Global (IR.Variable ident _)) = "@" ++ ident
toLLVMArg IR.Void = error "Void argument not allowed in LLVM IR"

toLLVMLiteral :: IR.Value -> String
toLLVMLiteral (IR.LiteralInt int) = show int
toLLVMLiteral (IR.LiteralBool bool) = if bool then "1" else "0"
toLLVMLiteral (IR.LiteralString str) = "c\"" ++ escapesToHexEscapes str ++ "\\00\""
toLLVMLiteral (IR.Null _) = "null"

toLLVMLabel :: IR.Label -> String
toLLVMLabel label = "%" ++ label

toLLVMRegister :: IR.Variable -> String
toLLVMRegister (IR.Variable var _) = _toLLVMRegister var

_toLLVMRegister :: IR.Label -> String
_toLLVMRegister var = "%" ++ var

toLLVMType :: LangTypes.LangType -> String
toLLVMType LangTypes.IntType = "i32"
toLLVMType LangTypes.BoolType = "i1"
toLLVMType LangTypes.StringType = "i8*"
toLLVMType LangTypes.VoidType = "void"
toLLVMType (LangTypes.ClassType ident) = "%" ++ ident ++ "*"
toLLVMType (LangTypes.ArrayType ty) = "%__arr__" ++ "*"
toLLVMType (LangTypes.SizedString s) = "[" ++ show (s + 1) ++ " x i8]*"
toLLVMType (LangTypes.Pointer ty) = toLLVMType ty ++ "*"
toLLVMType _ = error "Unsupported type in LLVM IR"

toLLVMBaseType :: LangTypes.LangType -> String
toLLVMBaseType LangTypes.IntType = "i32"
toLLVMBaseType LangTypes.BoolType = "i1"
toLLVMBaseType LangTypes.StringType = "i8"
toLLVMBaseType LangTypes.VoidType = "void"
toLLVMBaseType (LangTypes.ClassType ident) = "%" ++ ident
toLLVMBaseType (LangTypes.ArrayType ty) = "%__arr__"
toLLVMBaseType (LangTypes.SizedString s) = "[" ++ show (s + 1) ++ " x i8]"
toLLVMBaseType (LangTypes.Pointer ty) = toLLVMType ty
toLLVMBaseType _ = error "Unsupported type in LLVM IR"

toLLVMTypeArg :: IR.Arg -> String
toLLVMTypeArg (IR.Ident var) = toLLVMTypeVariable var
toLLVMTypeArg (IR.Literal lit) = toLLVMTypeValue lit
toLLVMTypeArg (IR.Global var) = toLLVMTypeVariable var
toLLVMTypeArg IR.Void = "void"

toLLVMTypeVariable :: IR.Variable -> String
toLLVMTypeVariable (IR.Variable _ ty) = toLLVMType ty

toLLVMTypeValue :: IR.Value -> String
toLLVMTypeValue (IR.LiteralString s) = "[" ++ show (length s + 1) ++ " x i8]"
toLLVMTypeValue value = toLLVMType (IR.typeFromValue value)