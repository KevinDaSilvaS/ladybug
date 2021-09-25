module Lang.Src.Compiler.LexicalAnalyser where

import Lang.Src.Compiler.Tokens

generateToken value line col = Token {
            token     = value,
            tokenLine = line,
            tokenCol  = col
        } 

integersSet = ['0'..'9']
identifiersStartSet = '_':['A'..'z'] 
identifiersSet = integersSet ++ identifiersStartSet 

--startLexicalAnalysis program line col
startLexicalAnalysis ('.':xs) line col = (parsedToken, line, nCol, xs)
    where
        nCol = col + 1
        parsedToken = generateToken (EndParamsToken, ".") line nCol
startLexicalAnalysis (',':xs) line col = (parsedToken, line, nCol, xs)
    where
        nCol = col + 1
        parsedToken = generateToken (SeparatorToken, ",") line nCol
startLexicalAnalysis ('=':xs) line col = (parsedToken, line, nCol, xs)
    where
        nCol = col + 1
        parsedToken = generateToken (AssignToken , "=") line nCol
startLexicalAnalysis ('f':xs) line col = 
    fnAutomaton xs line col "f"
startLexicalAnalysis ('e':xs) line col = 
    exportAutomaton xs line col "e"
startLexicalAnalysis ('<':xs) line col = 
    openParamsAutomaton xs line nCol "<"
    where
        nCol = col + 1
startLexicalAnalysis (':':xs) line col = 
    closeParamsAutomaton xs line nCol ":"
    where
        nCol = col + 1
startLexicalAnalysis ('\n':xs) line col = 
    startLexicalAnalysis xs (line+1) 0 
startLexicalAnalysis (' ':xs) line col = 
    startLexicalAnalysis xs line (col+1) 
startLexicalAnalysis (x:xs) line col 
    | x `elem` integersSet = 
        integerAutomaton xs line col [x]
    | x `elem` identifiersStartSet = identifierAutomaton xs line col [x]
    | otherwise = error "startLexicalAnalysis"
startLexicalAnalysis [] line col = (parsedToken, line, col, [])
    where
        parsedToken = generateToken (EmptyToken, "") line col

fnAutomaton ('n':xs) line col "f"  = fnAutomaton xs line nCol "fn"
    where
        nCol = col + 1
fnAutomaton (' ':xs) line col "fn" = (parsedToken, line, col, ' ':xs)
    where
        parsedToken = generateToken (FnToken, "fn") line col
fnAutomaton xs line col reading = 
    identifierAutomaton xs line col reading

integerAutomaton (x:xs) line col reading 
    | x `elem` integersSet = 
        integerAutomaton xs line nCol (reading ++ [x])
    | otherwise = (parsedToken, line, nCol, x:xs)
    where
        nCol = col + 1
        parsedToken = generateToken (IntegerToken, reading) line nCol
integerAutomaton [] line col reading = (parsedToken, line, nCol, [])
    where
        nCol = col + 1
        parsedToken = generateToken (IntegerToken, reading) line nCol

identifierAutomaton (x:xs) line col reading 
    | x `elem` identifiersSet = 
        identifierAutomaton xs line nCol (reading ++ [x])
    | otherwise = (parsedToken, line, nCol, x:xs)
    where
        nCol = col + 1
        parsedToken = generateToken (IdentifierToken, reading) line nCol
identifierAutomaton [] line col reading = (parsedToken, line, nCol, [])
    where
        nCol = col + 1
        parsedToken = generateToken (IntegerToken, reading) line nCol

openParamsAutomaton (':':xs) line col "<" = (parsedToken, line, nCol, xs)
    where
        nCol = col + 1
        parsedToken = generateToken (OpenParamsToken, "<:") line nCol
openParamsAutomaton xs line col r = error "openParamsAutomaton"

closeParamsAutomaton ('>':xs) line col ":" = (parsedToken, line, nCol, xs)
    where
        nCol = col + 1
        parsedToken = generateToken (CloseParamsToken, ":>") line nCol
closeParamsAutomaton xs line col _ = error "closeParamsAutomaton"

exportAutomaton ('x':xs) line col "e"  = exportAutomaton xs line nCol "ex"
    where
        nCol = col + 1
exportAutomaton ('p':xs) line col "ex"  = exportAutomaton xs line nCol "exp"
    where
        nCol = col + 1
exportAutomaton ('o':xs) line col "exp"  = exportAutomaton xs line nCol "expo"
    where
        nCol = col + 1
exportAutomaton ('r':xs) line col "expo"  = exportAutomaton xs line nCol "expor"
    where
        nCol = col + 1
exportAutomaton ('t':xs) line col "expor"  = exportAutomaton xs line nCol "export"
    where
        nCol = col + 1
exportAutomaton (' ':xs) line col "export" = (parsedToken, line, col, ' ':xs)
    where
        parsedToken = generateToken (ExportToken, "fn") line col
exportAutomaton xs line col reading = 
    identifierAutomaton xs line col reading