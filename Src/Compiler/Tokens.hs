module Lang.Src.Compiler.Tokens where

data Tokens = FnToken          |
              ExportToken      |
              IdentifierToken  |
              OpenParamsToken  |
              CloseParamsToken |
              IntegerToken     |
              SeparatorToken   |
              EndParamsToken   |
              AssignToken      |
              EmptyToken       |
              ShowToken        deriving(Show, Eq)

data Token = Token {
        token     :: (Tokens, [Char]),
        tokenLine :: Int,
        tokenCol  :: Int
    } deriving (Show, Eq)