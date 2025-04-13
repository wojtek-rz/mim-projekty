module LLVMBackend.Utils where
import qualified CodeGenerator.IRCode as IR
import CodeGenerator.IRCode

replaceValueBasicBlock :: IR.Ident -> Arg -> IR.BasicBlock -> IR.BasicBlock
replaceValueBasicBlock oldSymbol newValue block =
  block {instrs = map (\instr -> replaceValueInstr instr oldSymbol newValue) (instrs block)}
replaceValueInstr :: IR.Instr -> IR.Ident -> Arg -> IR.Instr
replaceValueInstr (BinOp res arg1 op arg2) oldSymbol newValue =
  BinOp res (replaceValueArg oldSymbol newValue arg1) op (replaceValueArg oldSymbol newValue arg2)
replaceValueInstr (UnOp res op arg1) oldSymbol newValue =
  UnOp res op (replaceValueArg oldSymbol newValue arg1)
replaceValueInstr (Assign res arg1) oldSymbol newValue = Assign res (replaceValueArg oldSymbol newValue arg1)
replaceValueInstr (Branch arg1 label1 label2) oldSymbol newValue =
  Branch (replaceValueArg oldSymbol newValue arg1) label1 label2
replaceValueInstr (Call res func args) oldSymbol newValue =
  Call res func (map (\newValue' -> replaceValueArg oldSymbol newValue newValue') args)
replaceValueInstr (Return arg1) oldSymbol newValue = Return (replaceValueArg oldSymbol newValue arg1)
replaceValueInstr (Phi res args) oldSymbol newValue =
  Phi res (map (\(label, arg') -> (label, fmap (replaceValueArg oldSymbol newValue) arg')) args)
replaceValueInstr (GEP res arg1 offsets) oldSymbol newValue =
  GEP res (replaceValueArg oldSymbol newValue arg1) (map (\newValue' -> replaceValueArg oldSymbol newValue newValue') offsets)
replaceValueInstr instr _ _ = instr

replaceValueArg :: IR.Ident -> Arg -> Arg -> Arg
replaceValueArg toReplace' newArg oldValue@(Ident (Variable ident _)) = if ident == toReplace' then newArg else oldValue
replaceValueArg _ _ arg = arg