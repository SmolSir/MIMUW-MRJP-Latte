module C_Utils where

import Data.List as List
import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except

import Latte.Abs


-----------------------------------
-- compiler runtime types & data --
-----------------------------------
type Variable = String
type VariableMap = Map.Map Variable (Memory, Type)
type ReturnTypeMap = Map.Map Variable Type
type StringLabelMap = Map.Map String Label
type ClassDefinitionMap = Map.Map Variable ClassDefinitionData

data ClassDefinitionData = ClassDefinitionData {
    extends :: Maybe Variable,
    attributeMap :: ReturnTypeMap,
    methodMap :: ReturnTypeMap
}

data VMTable = VMTable {
    virtualAttributeMap :: VariableMap,
    virtualMethodList :: [ (Variable, (Type, Integer, Variable)) ]
}

data CEnvironment = CEnvironment {
    scope :: Integer,
    variableMap :: VariableMap,
    insideClass :: Maybe Variable
}

data CState = CState {
    localsCount :: Integer,
    nextLabel :: Integer,
    returnLabel :: String,
    stringMap :: StringLabelMap,
    classMap :: ClassDefinitionMap,
    vmtableMap :: Map.Map Variable VMTable,
    functionReturnTypeMap :: ReturnTypeMap

}

type CMonad a = ReaderT CEnvironment (StateT CState (ExceptT String IO)) a

----------------------------------
-- code generation types & data --
----------------------------------
data Register = EAX | ECX | EDX | EBP | ESP

data Memory = Local Integer | Parameter Integer | Attribute Integer

data Operand =
      RegisterOp Register
    | MemoryOp Memory
    | LiteralOp Integer
    | StringLiteralOp Integer
    | VMTableLiteralOp String
    | AddressOp Integer Register
    | ArrayElementAddressOp Register Register Integer
    | AttributeAddressOp Integer Register
    | MethodAddressOp Integer Register

data BinaryOperator = ADD | SUB | MUL | DIV | CMP | XOR

data UnaryOperator = INC | DEC | NEG

data ZeroOperator = RET | CDQ -- CDQ -> Convert Doubleword to Quadword

data JumpOperator = JL | JLE | JE | JGE | JG | JNE | JMP

data Label = FunctionLabel String | JumpLabel Integer | StringLabel Integer String

data Instruction =
      ENTRY
    | DATA
    | TEXT
    | HEADER
    | FOOTER
    | STACK_ALLOCATION Integer
    | PUSH Operand
    | POP Operand
    | MOV Operand Operand
    | LEA Operand Operand
    | CALL String
    | CALL_METHOD Operand
    | BINARY_INSTRUCTION BinaryOperator Operand Operand
    | UNARY_INSTRUCTION UnaryOperator Operand
    | ZERO_INSTRUCTION ZeroOperator
    | JUMP JumpOperator Label
    | LABEL Label
    | VM_TABLE [String]

-- constants --
dword = 4
trueLiteral = LiteralOp 1
falseLiteral = LiteralOp 0
nullPointer = LiteralOp 0

------------------------------------
-- code generation show functions --
------------------------------------
instance Show Register where
    show EAX = "%eax"
    show ECX = "%ecx"
    show EDX = "%edx"
    show EBP = "%ebp"
    show ESP = "%esp"

instance Show Memory where
    show (Local value)     = show (-dword * value) ++ "(" ++ show EBP ++ ")"
    show (Parameter value) = show (dword * (value + 1)) ++ "(" ++ show EBP ++ ")"
    show (Attribute value) = show (dword * value) ++ "(" ++ show EAX ++ ")"

instance Show Operand where
    show (RegisterOp register) = show register

    show (MemoryOp memory) = show memory

    show (LiteralOp literal) = '$' : show literal

    show (StringLiteralOp stringLiteral) = '$' : show (StringLabel stringLiteral "")

    show (VMTableLiteralOp vmtableLiteral) = '$' : vmtableLiteral

    show (AddressOp 0     register) = "(" ++ show register ++ ")"

    show (AddressOp value register) = show (-dword * value) ++ "(" ++ show register ++ ")"

    show (ArrayElementAddressOp registerL registerR value) =
        "(" ++ show registerL ++ "," ++ show registerR ++ "," ++ show value ++ ")"

    show (AttributeAddressOp 0     register) = error "Cannot show Operand for attribute address 0"

    show (AttributeAddressOp value register) = show (dword * value) ++ "(" ++ show register ++ ")"

    show (MethodAddressOp 0     register) = "*" ++ "(" ++ show register ++ ")"

    show (MethodAddressOp value register) = "*" ++ show (dword * value) ++ "(" ++ show register ++ ")"

instance Show BinaryOperator where
    show ADD = "addl" ++ " "
    show SUB = "subl" ++ " "
    show MUL = "imul" ++ " "
    show DIV = "idiv" ++ " "
    show CMP = "cmp" ++ " "
    show XOR = "xor" ++ " "

instance Show UnaryOperator where
    show INC = "incl" ++ " "
    show DEC = "decl" ++ " "
    show NEG = "neg" ++ " "

instance Show ZeroOperator where
    show RET = "ret" ++ "\n"
    show CDQ = "cdq"

instance Show JumpOperator where
    show JL  = "jl"
    show JLE = "jle"
    show JE  = "je"
    show JGE = "jge"
    show JG  = "jg"
    show JNE = "jne"
    show JMP = "jmp"

instance Show Label where
    show (FunctionLabel function  ) = function
    show (JumpLabel     jump      ) = ".L"  ++ show jump
    show (StringLabel   string   _) = ".LC" ++ show string

instance Show Instruction where
    show ENTRY = ".globl main"

    show DATA = ".data"

    show TEXT = "\n\
        \.text"

    show HEADER =
        show (PUSH $ RegisterOp EBP) ++ "\n" ++
        "\t" ++ show (MOV (RegisterOp ESP) $ RegisterOp EBP)

    show FOOTER =
        show (MOV (RegisterOp EBP) $ RegisterOp ESP) ++ "\n" ++
        "\t" ++ show (POP $ RegisterOp EBP)

    show (STACK_ALLOCATION 0) = ""

    show (STACK_ALLOCATION value) =
        show $ BINARY_INSTRUCTION SUB (LiteralOp (dword * value)) $ RegisterOp ESP

    show (PUSH operand) = "pushl" ++ " " ++ show operand

    show (POP  operand) = "popl"  ++ " " ++ show operand

    show (MOV operandL operandR) = "movl" ++ " " ++ show operandL ++ ", " ++ show operandR

    show (LEA operandL operandR) = "leal" ++ " " ++ show operandL ++ ", " ++ show operandR

    show (CALL literal) = "call" ++ " " ++ literal

    show (CALL_METHOD operand) = "call" ++ " " ++ show operand

    show (BINARY_INSTRUCTION binaryOperator operandL operandR) =
        show binaryOperator ++ " " ++ show operandL ++ ", " ++ show operandR

    show (UNARY_INSTRUCTION unaryOperator operand) = show unaryOperator ++ " " ++ show operand

    show (ZERO_INSTRUCTION zeroOperator) = show zeroOperator

    show (JUMP jumpOperator label) = show jumpOperator ++ " " ++ show label

    show (LABEL label@(StringLabel _ stringLabel)) =
        show label ++ ":" ++ "\t" ++ ".asciz" ++ " " ++ show stringLabel

    show (LABEL label) = show label ++ ":"

    show (VM_TABLE vmList) = ".int" ++ " " ++ intercalate ", " vmList

-------------------------------------
-- instruction collector functions --
-------------------------------------
type InstructionPrepend = [Instruction] -> [Instruction]

-- adds indent before instruction based on its kind
instructionIndent :: Instruction -> String
instructionIndent ENTRY = ""
instructionIndent DATA  = ""
instructionIndent TEXT  = ""
instructionIndent (LABEL    _) = ""
instructionIndent (VM_TABLE _) = ""
instructionIndent _ = "\t"

-- prepends an instruction to a list of instructions
instructionListAdd :: Instruction -> InstructionPrepend
instructionListAdd = (:)

-- merges two lists of instructions
instructionListMerge :: [Instruction] -> InstructionPrepend
instructionListMerge = (++)

instructionListUnpack :: [Instruction] -> CMonad String
instructionListUnpack instructionList =
    return $ (
        unlines .
            List.map (\instruction -> instructionIndent instruction ++ show instruction)
        )
        instructionList

----------------------
-- helper functions --
----------------------
throwCMonad :: String -> CMonad a
throwCMonad = lift . lift . throwE

getFreeLabel :: CMonad Integer
getFreeLabel = do
    label <- gets nextLabel
    modify (\state -> state { nextLabel = label + 1 })
    return label

getStringLabel :: String -> CMonad Integer
getStringLabel string = do
    stringMap <- gets stringMap
    case Map.lookup string stringMap of
        Nothing -> do
            let label = fromIntegral $ Map.size stringMap
            modify (\state -> state {
                stringMap = Map.insert string (StringLabel label string) stringMap
            })
            return label
        (Just (StringLabel label string)) -> return label

getMethodLabel :: Variable -> Variable -> String
getMethodLabel className methodName = className ++ "." ++ methodName

getVirtualMethodTableLabel :: Variable -> String
getVirtualMethodTableLabel className = getMethodLabel className "VMT"

translateOperator :: RelOp -> JumpOperator
translateOperator operator = case operator of
    LTH -> JL
    LE  -> JLE
    EQU -> JE
    GE  -> JGE
    GTH -> JG
    NE  -> JNE
