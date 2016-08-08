module Ref where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

fixLangDef = LanguageDef { 
    commentStart = "{*",
    commentEnd = "*}",
    commentLine = "//",
    nestedComments = True,
    identStart = alphaNum,
    identLetter = alphaNum,
    opStart = opLetter fixLangDef,
    opLetter = oneOf ";:!&*+./<=>#?@^|-~_()[]",
    reservedOpNames = ["{","}"],
    reservedNames = [],
    caseSensitive = True
}

lexer = P.makeTokenParser fixLangDef

identifier= P.identifier lexer
symbol = P.symbol lexer
reservedOp = P.reservedOp lexer
operator = P.operator lexer

file :: Parser String
file = do
    name <- many1 alphaNum
    symbol "."
    ext <- many1 alphaNum
    return $ name++"."++ext

ref :: Parser (IO String)
ref = try $ do
    reservedOp "{"
    f <- file
    reservedOp "}"
    return $ readFile f

solve :: Parser (IO String)
solve = do
    s <- many $ do
        s1 <- many1 $ noneOf ['{','}']
        r <- option (return "") ref
        return $ do {r1 <- r; return $ s1++r1}
    return $ do {ss <- sequence s; return $ concat ss}
    ---return $ sequence s
        
    


