module Lang.Src.Compiler.ParseTree where

import Lang.Src.Compiler.Tokens
import Lang.Src.Compiler.ExtractToken ( extractTokenInfo )

data Expr = Value Token |
            FunctionCall Token [Token] | 
            EndExpr deriving (Show)

data ParseTree = Assign Token Expr ParseTree |
                 Function Token [ParseTree] ParseTree |
                 Export Expr ParseTree                |
                 EndNode deriving (Show)

starParsingTree :: [Token] -> ParseTree
starParsingTree [] = EndNode
starParsingTree (x:xs)
    | token' == FnToken = 
        Function tokenIdentifier arguments (starParsingTree remain)
    | token' == IdentifierToken = 
        Assign x expr (starParsingTree remain')
    | token' == ExportToken = Export expr (starParsingTree remain')
    | otherwise = error $ show (x:xs)
    where
        token' = fst (token x)
        tokenIdentifier = head xs
        (arguments, remain) = params $ tail xs
        (expr, remain') = expressionValue xs


params :: [Token] -> ([ParseTree], [Token])
params [] = ([], [])
params [x] = ([], [])
params (x:xs) 
    | token' == OpenParamsToken = params xs
    | token' == IdentifierToken = 
        (Assign x (Value (head xs)) EndNode:ptr, remain)
    | otherwise = ([], xs)
    where
        (ptr, remain) = params $ tail xs
        token' = fst (token x)

expressionValue :: [Token] -> (Expr, [Token])
expressionValue [] = (EndExpr, [])
expressionValue [x] = (Value x, [])
expressionValue (x:xs)
    | token' == EndParamsToken = (FunctionCall (head xs) expr, remain)
    | token' == IdentifierToken || token' == IntegerToken = (Value x, xs)
    | otherwise = error $ show (x:xs)
    where
        (expr, remain) = argumentsFnCall $ tail $ tail xs
        token' = fst (token x)

argumentsFnCall :: [Token] -> ([Token], [Token])
argumentsFnCall [] = ([], [])
argumentsFnCall (x:xs)
    | token' == IdentifierToken || token' == IntegerToken = (x:tkList, remain)
    | otherwise = ([], xs)
    where
        token' = fst (token x)
        (tkList, remain) = argumentsFnCall xs
