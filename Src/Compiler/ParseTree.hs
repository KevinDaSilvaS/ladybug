module Lang.Src.Compiler.ParseTree where

import Lang.Src.Compiler.Tokens
import Lang.Src.Compiler.ExtractToken ( extractTokenInfo )

data Expr = Value Token |
            FunctionCall Token [Token] deriving (Show)

data ParseTree = Assign Token Expr ParseTree |
                 Function Token [ParseTree] ParseTree |
                 Export Expr                          |
                 EndNode deriving (Show)

starParsingTree (x:xs)
    | token' == FnToken = 
        Function fnName arguments (starParsingTree remain)
    | token' == IdentifierToken = 
        Function fnName arguments (starParsingTree remain)
    where
        token' = fst (token x)
        fnName = head xs
        (arguments, remain) = params $ tail xs

params (x:xs) 
    | token' == OpenParamsToken = params xs
    | token' == IdentifierToken = 
        (Assign x (Value (head xs)) EndNode:ptr, remain)
    | otherwise = ([], remain)
    where
        (ptr, remain) = params $ tail xs
        token' = fst (token x)

{- expressionValue (x:xs)
    | token' == EndParamsToken = FunctionCall (head xs) expr
    where
        (expr, remain) = arguments $ tail xs
        token' = fst (token x)

arguments (x:xs)
    | token' == IdentifierToken || token' == IntegerToken =
        x:arguments xs
    | token' == EndParamsToken = ([], remain)
    where
        (expr, remain) = arguments $ tail xs
        token' = fst (token x) -}