module Lang.Src.Compiler.TestFeatures where

import Lang.Src.Compiler.LexicalAnalyser

tokens = do
    let program = "fn fnc <: _v=0 v1=12 :> v = 90 export 90" ---"fnw3030fn ,=..."
    --startLexicalAnalysis program 1 0
    getAllTokensFromProgram program 1 0

getAllTokensFromProgram [] _ _= []
getAllTokensFromProgram xs l c = 
    parsedToken:getAllTokensFromProgram remain line col 
    where
        (parsedToken, line, col, remain) = startLexicalAnalysis xs l c