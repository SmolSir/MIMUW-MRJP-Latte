module C_Statements where

import Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State

import Latte.Abs

import C_Expressions
import C_Utils


----------------------
-- helper functions --
----------------------
translateDeclaration :: Type -> Item -> CMonad (CEnvironment, InstructionPrepend)
translateDeclaration declarationType declaration = do
    state <- get
    scope <- asks scope
    let location = Local $ localsCount state + 1
    modify (\state -> state {
        localsCount = localsCount state + 1
    })
    (identifier, generatedCode) <- case declaration of
        (NoInit (Ident identifier)) -> do
            value <- defaultValueForType declarationType
            return (
                identifier,
                instructionListMerge [
                    MOV value (MemoryOp location)
                ])
        (Init (Ident identifier) expression) -> do
            initializationCode <- translateExpression expression
            return (
                identifier,
                initializationCode . instructionListMerge [
                    MOV (RegisterOp EAX) (MemoryOp location)
                ])
    env <- ask
    let updatedVariableMap = Map.insert identifier (location, declarationType) $ variableMap env
    return (
        env { variableMap = updatedVariableMap }, generatedCode
        )
    where
        defaultValueForType :: Type -> CMonad Operand
        defaultValueForType Int  = return $ LiteralOp 0
        defaultValueForType Bool = return falseLiteral
        defaultValueForType _    = return nullPointer

translateStatementList :: [Stmt] -> CMonad (CEnvironment, InstructionPrepend)
translateStatementList statementList = do
    environment        <- ask
    (_, generatedCode) <- foldM foldFunction (environment, id) statementList
    return (environment, generatedCode)
    where
        foldFunction :: (CEnvironment, InstructionPrepend) -> Stmt -> CMonad (CEnvironment, InstructionPrepend)
        foldFunction (currentEnvironment, accumulatedCode) statement = do
            (resultEnvironment, resultCode) <- local (const currentEnvironment) $ translateStatement statement
            return (resultEnvironment, accumulatedCode . resultCode)

translateStatementBlockWrap :: Stmt -> CMonad InstructionPrepend
translateStatementBlockWrap statement@(BStmt _) = do
    (_, generatedCode) <- translateStatement statement
    return generatedCode

translateStatementBlockWrap statement = do
    (_, generatedCode) <- translateStatement (BStmt (Block [statement]))
    return generatedCode

----------------
-- statements --
----------------
translateStatement :: Stmt -> CMonad (CEnvironment, InstructionPrepend)
translateStatement Empty = do
    environment <- ask
    return (
        environment,
        id
        )

translateStatement (BStmt (Block statementList)) = do
    environment <- ask
    local (\environment -> environment { scope = scope environment + 1 }) $ translateStatementList statementList

translateStatement (Decl declarationType declarationList) = do
    environment <- ask
    foldM (foldFunction declarationType) (environment, id) declarationList
    where
        foldFunction :: Type -> (CEnvironment, InstructionPrepend) -> Item -> CMonad (CEnvironment, InstructionPrepend)
        foldFunction declType (currentEnvironment, accumulatedCode) declaration = do
            (resultEnvironment, resultCode) <- local (const currentEnvironment) $ translateDeclaration declType declaration
            return (
                resultEnvironment,
                accumulatedCode . resultCode
                )

translateStatement (Ass expressionL expressionR) = do
    expressionCodeL <- translateLValue expressionL
    expressionCodeR <- translateExpression expressionR
    environment <- ask
    return (
        environment,
        expressionCodeL . instructionListAdd (PUSH $ RegisterOp EAX) . expressionCodeR . instructionListMerge [
            POP $ RegisterOp EDX,
            MOV (RegisterOp EAX) $ AddressOp 0 EDX
        ])

translateStatement (Incr expression) = do
    expressionCode <- translateLValue expression
    environment <- ask
    return (
        environment,
        expressionCode . instructionListMerge [
            UNARY_INSTRUCTION INC $ AddressOp 0 EAX
        ])

translateStatement (Decr expression) = do
    expressionCode <- translateLValue expression
    environment <- ask
    return (
        environment,
        expressionCode . instructionListMerge [
            UNARY_INSTRUCTION DEC $ AddressOp 0 EAX
        ])

translateStatement (Ret expression) = do
    generatedCode <- translateExpression expression
    environment <- ask
    label <- gets returnLabel
    return (
        environment,
        generatedCode . instructionListMerge [
            JUMP JMP $ FunctionLabel label
        ])

translateStatement VRet = do
    environment <- ask
    label <- gets returnLabel
    return (
        environment,
        instructionListMerge [
            JUMP JMP $ FunctionLabel label
        ])

translateStatement (Cond expression statement) = do
    labelTrue <- getFreeLabel
    labelNext <- getFreeLabel
    expressionConditionCode <- translateCondition expression labelTrue labelNext
    statementTrueCode       <- translateStatementBlockWrap statement
    let resultCollector =
            instructionListAdd (LABEL $ JumpLabel labelTrue) .
            statementTrueCode .
            instructionListAdd (LABEL $ JumpLabel labelNext)
    environment <- ask
    return (
        environment,
        expressionConditionCode . resultCollector
        )

translateStatement (CondElse expression statementL statementR) = do
    labelTrue  <- getFreeLabel
    labelFalse <- getFreeLabel
    labelNext  <- getFreeLabel
    expressionConditionCode <- translateCondition expression labelTrue labelFalse
    statementTrueCode       <- translateStatementBlockWrap statementL
    statementFalseCode      <- translateStatementBlockWrap statementR
    let resultCollector =
            instructionListAdd (LABEL $ JumpLabel labelTrue) .
            statementTrueCode .
            instructionListAdd (JUMP JMP $ JumpLabel labelNext) .
            instructionListAdd (LABEL $ JumpLabel labelFalse) .
            statementFalseCode .
            instructionListAdd (LABEL $ JumpLabel labelNext)
    environment <- ask
    return (
        environment,
        expressionConditionCode . resultCollector
        )

translateStatement (While expression statement) = do
    labelNext      <- getFreeLabel
    labelCondition <- getFreeLabel
    labelLoop      <- getFreeLabel
    expressionConditionCode <- translateCondition expression labelLoop labelNext
    statementLoopCode       <- translateStatementBlockWrap statement
    environment <- ask
    let resultCollector =
            instructionListAdd (JUMP JMP $ JumpLabel labelCondition) .
            instructionListAdd (LABEL $ JumpLabel labelLoop) .
            statementLoopCode .
            instructionListAdd (LABEL $ JumpLabel labelCondition) .
            expressionConditionCode .
            instructionListAdd (LABEL $ JumpLabel labelNext)
    return (
        environment,
        resultCollector
        )

translateStatement (For iteratorType identifier expression statement) =
    let iterator = Ident "__iterator"
        arrayPointer = Ident "__arrayPointer"
        whileTransformate expressionType = BStmt $ Block [
            Decl Int [NoInit iterator],
            Decl expressionType [Init arrayPointer expression],
            While
                (ERel (EVar iterator) LTH (EAttr (EVar arrayPointer) (Ident "length")))
                (BStmt $ Block [
                    Decl iteratorType [Init identifier (EArr (EVar arrayPointer) (EVar iterator))],
                    statement,
                    Incr (EVar iterator)
                ])
            ]
    in do
        expressionType <- getExpressionType expression
        translateStatement $ whileTransformate expressionType

translateStatement (SExp expression) = do
    generatedCode <- translateExpression expression
    environment   <- ask
    return (
        environment,
        generatedCode
        )
