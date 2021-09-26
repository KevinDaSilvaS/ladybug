module Lang.Src.Compiler.SintaticAnalyser where

import Lang.Src.Compiler.LexicalAnalyser
import Lang.Src.Compiler.Tokens
import Lang.Src.Compiler.ExtractToken ( extractTokenInfo )

startSintaticAnalysis [] line col 0 0 = [] 
startSintaticAnalysis program line col exports params 
    | token' == FnToken = 
        parsedToken:
            _fnName remain nline ncol (exports+1) params
    | token' == IdentifierToken = 
        parsedToken:_assign remain nline ncol exports params
    | token' == ExportToken = 
        parsedToken:
        _value remain nline ncol (exports-1) params startSintaticAnalysis
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

_fnCall program line col exports params
    | token' == IdentifierToken =
        parsedToken:_fnCallParamsStart remain nline ncol exports params
    | otherwise = error $ show rawToken
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_fnCallParamsStart program line col exports params
    | token' == EndParamsToken =
        parsedToken:_fnCallParams remain nline ncol exports params
    | otherwise = error $ show rawToken
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_fnCallParams program line col exports params 
    | token' == IntegerToken || token' == IdentifierToken =
        parsedToken:_fnCallParams remain nline ncol exports params
    | token' == EndParamsToken =
        parsedToken:startSintaticAnalysis remain nline ncol exports params
    | otherwise = error $ show rawToken
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken

_value program line col exports params fn
    | token' == IntegerToken || token' == IdentifierToken =
        parsedToken:fn remain nline ncol exports params
    | token' == EndParamsToken =
        parsedToken:_fnCall remain nline ncol exports params
    | otherwise = error ("_value " ++ show token ++ " <-- " ++ show program)
    where
        token = startLexicalAnalysis program line col 
        (rawToken, parsedToken, nline, ncol, remain) = extractTokenInfo token
        (token', value') = rawToken