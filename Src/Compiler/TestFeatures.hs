module Lang.Src.Compiler.TestFeatures where

import Lang.Src.Compiler.LexicalAnalyser
import Lang.Src.Compiler.SintaticAnalyser
import Lang.Src.Compiler.ParseTree
import Lang.Src.Compiler.SemanticAnalyser

tokens = do
    let program = "fn fnc <: _v=0 v1=12 :> v = 90 export 90" ---"fnw3030fn ,=..."
    --startLexicalAnalysis program 1 0
    getAllTokensFromProgram program 1 0

getAllTokensFromProgram [] _ _= []
getAllTokensFromProgram xs l c = 
    parsedToken:getAllTokensFromProgram remain line col 
    where
        (parsedToken, line, col, remain) = startLexicalAnalysis xs l c

sintaticAnalysis = do
    let program = "r = 22 l = r fn fnc <: _v=0 v1=12 :> v = 90 export 90 i = . fnc . 20 40 20 ."
    let tokens = startSintaticAnalysis program 1 0 0 0
    let ptr = starParsingTree tokens
    startSemanticAnalysis ptr [] []
