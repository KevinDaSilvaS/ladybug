module Lang.Src.Compiler.FunctionSemantics where

import Lang.Src.Compiler.Tokens
import Lang.Src.Compiler.ExtractToken ( extractTokenInfo )
import Lang.Src.Compiler.ParseTree
import Data.Maybe ( isNothing, fromJust )

-- fnTable = ([Char], [[([Char], Token)], [([Char], Token)], Token])
-- fnTable = [([Char], ([], [], Token)

_fns EndNode sTable fnTable = (sTable, fnTable)
_fns (Assign identifier value ptr) sTable fnTable
    | isNothing alreadyDeclared = _fns ptr nSTable fnTable
    | otherwise = error $ show identifier ++ " already declared"
    where
        nSTable = (snd tokenIdentifier, tokenValue):sTable
        tokenIdentifier = token identifier
        tokenValue = parseExpr value fnTable False
        alreadyDeclared = lookup (snd tokenIdentifier) sTable
_fns (Export expr ptr) sTable fnTable = _fns ptr sTable nFnTable
    where
        token' = parseExpr expr [] False
        (fnName, (parameters, scope, export)) = head fnTable
        nFnTable = (fnName, (parameters, scope, token')):tail fnTable
_fns (Function fnName parameters ptr) sTable fnTable
    | isNothing alreadyDeclared = _fns tree sTable nFnTable''
    | otherwise = error (show fnName ++ " already declared")
    where
        alreadyDeclared = lookup (snd token') fnTable
        token' = token fnName
        nFnTable = fn:fnTable
        nFnTable' = _parameters parameters nFnTable
        fn = (snd token', ([], [], (EmptyToken, "")))
        (tree, nFnTable'') = _scope ptr nFnTable'

_parameters [] fnTable = fnTable
_parameters ((Assign identifier value ptr):xs) fnTable
    | isNothing alreadyDeclared = _parameters xs nFnTable
    | otherwise = error $ show identifier ++ " already declared."
    where
        tokenIdentifier = token identifier
        tokenValue = parseExpr value scope True
        alreadyDeclared = lookup (snd tokenIdentifier) scope
        (fnName, (parameters, scope, export)) = head fnTable
        nParameters = tokenValue:parameters
        nScope = (snd tokenIdentifier, tokenValue):scope
        nFnTable = (fnName, (nParameters, nScope, export)):tail fnTable

_scope (Assign identifier value ptr) fnTable
    | isNothing alreadyDeclared = _scope ptr nFnTable
    | otherwise = error $ show identifier ++ " already declared"
    where
        tokenIdentifier = token identifier
        tokenValue = parseExpr value scope False
        alreadyDeclared = lookup (snd tokenIdentifier) scope
        (fnName, (parameters, scope, export)) = head fnTable
        nFnTable = (fnName, (parameters, 
            (snd tokenIdentifier, tokenValue):scope, export)): tail fnTable
_scope ptr fnTable = (ptr, fnTable)

parseExpr (Value value) symbolTable True
    | fst token' == IntegerToken = token'
    | otherwise = error "Expecting integer value"
    where
        token' = token value
parseExpr (Value value) symbolTable False
    | fst token' == IntegerToken = token'
    | isNothing alreadyDeclared  = error "Variable or function not declared"
    | otherwise = token'
    where
        token' = token value
        alreadyDeclared = lookup (snd token') symbolTable
parseExpr (FunctionCall fn args) symbolTable False
    | isNothing alreadyDeclared = error "function not declared"
    | otherwise = tokenFn
    where
        tokenFn = token fn
        alreadyDeclared = lookup (snd tokenFn) symbolTable
parseExpr x symbolTable _ = error $ show x
