module Lang.Src.Compiler.ExtractToken where

import Lang.Src.Compiler.Tokens ( Token(token) )

extractTokenInfo (parsedToken, line, col, remain) = 
    (rawToken, parsedToken, line, col, remain)
    where
        rawToken = token parsedToken