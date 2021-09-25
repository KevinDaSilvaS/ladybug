module Lang.Src.Compiler.SintaticAnalyser where

import Lang.Src.Compiler.LexicalAnalyser
import Lang.Src.Compiler.Tokens

extractTokenInfo (parsedToken, line, col, remain) = 
    (rawToken, parsedToken, line, col, remain)
    where
        rawToken = token parsedToken

startSintaticAnalysis [] line col 0 0 = [] 
startSintaticAnalysis program line col exports params 
    | token' == FnToken = 
        parsedToken:
            _fnName remain nline ncol (exports+1) params
    | token' == IdentifierToken = 
        parsedToken:_assign remain nline ncol exports params
    | token' == ExportToken = 
        parsedToken:_value remain nline ncol (exports-1) params startSintaticAnalysis
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken
startSintaticAnalysis program _ _ _ _ = 
    error ("PROGRAM ERROR :: " ++ show program) 

_fnName program line col exports params
    | token' == IdentifierToken = 
        parsedToken:_openParams remain nline ncol exports params
    | otherwise = error ("_fnName " ++ show token)
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_openParams program line col exports params
    | token' == OpenParamsToken = 
        parsedToken:_identifier remain nline ncol exports (params+1)
    | otherwise = error ("_openParams " ++ show token)
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_identifier program line col exports params  
    | token' == CloseParamsToken = 
        parsedToken:startSintaticAnalysis remain nline ncol exports (params-1)
    | token' == IdentifierToken = 
        parsedToken:_assign remain nline ncol exports params
    | otherwise = 
        startSintaticAnalysis program line col exports params
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_assign program line col exports params 
    | token' == AssignToken = 
        _value remain nline ncol exports params _identifier
    | otherwise = error $ show rawToken
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_value program line col exports params fn
    | token' == IntegerToken || token' == IdentifierToken =
        parsedToken:fn remain nline ncol exports params
    | otherwise = error ("_value " ++ show token)
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken