module Lang.Src.Compiler.SemanticAnalyser where

import Lang.Src.Compiler.Tokens
import Lang.Src.Compiler.ExtractToken ( extractTokenInfo )
import Lang.Src.Compiler.ParseTree
import Data.Maybe

--startSemanticAnalysis (x:xs) symbolTable fnTable
startSemanticAnalysis EndNode symbolTable _ = symbolTable
startSemanticAnalysis (Assign identifier expr ptr) symbolTable fnTable
    | isNothing alreadyDeclared = startSemanticAnalysis ptr nSymbolTable fnTable
    | otherwise = error "Identifier already declared"
    where
        token' = token identifier
        alreadyDeclared  = lookup (snd token') symbolTable
        parsedIdentifier = (snd token', parseExpr expr symbolTable fnTable)
        nSymbolTable     = parsedIdentifier:symbolTable
startSemanticAnalysis _ symbolTable _ = symbolTable

--parseExpr expr symbolTable fnTable
parseExpr (Value value) symbolTable fnTable
    | fst token' == IntegerToken = token'
    | isNothing alreadyDeclared  = error "Variable or function not declared"
    | otherwise = token'
    where
        token' = token value
        alreadyDeclared = lookup (snd token') symbolTable