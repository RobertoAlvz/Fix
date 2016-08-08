module Parser (Type(..), Tree(..), Op(..), parse, program)where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

data Type = T0 | T1 | TNum | TList Type | TProd Type Type | TSum Type Type | TExp Type Type

data Tree = Fix Tree | Abs [Char] Type Tree | App Tree Tree | Exp Op Tree Tree | Prd Tree Tree | Lst [Tree] | Num (Either Integer Double) | Id String | Init deriving Show

data Op = Comp | Prod | Sum | Proy | LIn | RIn | Eqs | Lts | Les | Gts | Ges | Neq | Head | Tail | Concat | Mod | Add | Dif | Mul | Div | Pow | Or | And | Not | Test deriving (Eq, Show)

instance Show Type where
    show T0 = "0"
    show T1 = "1"
    show TNum = "Num"
    show (TList t) = "["++(show t)++"]"
    show (TProd a b) = (show a)++"*"++(show b)
    show (TSum a b) = (show a)++"+"++(show b)
    show (TExp a b) = (show a)++"=>"++(show b)

fixLangDef = LanguageDef { 
    commentStart = "{*",
    commentEnd = "*}",
    commentLine = "//",
    nestedComments = True,
    identStart = letter,
    identLetter = alphaNum,
    opStart = opLetter fixLangDef,
    opLetter = oneOf ";:!&*+./<=>?@^|-~_",
    reservedOpNames = ["->","=>",":"],
    reservedNames = ["fix"],
    caseSensitive = True
}

lexer = P.makeTokenParser fixLangDef

number = P.naturalOrFloat lexer
identifier= P.identifier lexer
comma = P.comma lexer
semi = P.semi lexer
symbol = P.symbol lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

program = do
    P.whiteSpace lexer
    p <- apps
    eof
    return p

apps = do
    es <- many1 (try (reserved "fix" >> fun >>= \e->return (Fix e) <?> "fix") <|> fun)
    return (foldl1 App es)

fun = try lambda <|> expr 

term :: Parser Tree
term =  do {n <- number; return (Num n)}
    <|> do {id <- identifier; return (Id id)}
    <|> try (do {symbol "("; t <- apps; symbol ")"; return t})
    <|> try (do {symbol "{"; t <- apps; symbol "}"; return t})
    <|> do {symbol "("; t <- sepBy apps comma; symbol ")"; return $ foldr Prd Init t}
    <|> do {symbol "["; t <- sepBy apps comma; symbol "]"; return $ Lst t}
    <?> "term"

expr :: Parser Tree
expr = buildExpressionParser table term <?> "expression"
    where table = [ [Prefix (action1 "!" Exp Not)],
                    [Infix (action2 "&&" Exp And) AssocLeft],
                    [Infix (action2 "||" Exp Or) AssocLeft],
                    [Infix (action2 "^" Exp Pow) AssocLeft],
                    [Infix (action2 "*" Exp Mul) AssocLeft, Infix (action2 "/" Exp Div) AssocLeft],
                    [Infix (action2 "+" Exp Add) AssocLeft, Infix (action2 "-" Exp Dif) AssocLeft],
                    [Infix (action2 "%" Exp Mod) AssocLeft],
                    [Prefix (action1 "@" Exp Head), Prefix (action1 "~" Exp Tail)],
                    [Infix (action2 "#" Exp Concat) AssocLeft],
                    [Infix (action2 "=" Exp Eqs) AssocNone, Infix (action2 "<" Exp Lts) AssocNone, 
                     Infix (action2 ">" Exp Gts) AssocNone, Infix (action2 "!=" Exp Neq) AssocNone,
                     Infix (action2 ">=" Exp Ges) AssocNone, Infix (action2 "<=" Exp Les) AssocNone],
                    [Infix (action2 "?" Exp Test) AssocNone],
                    [Infix (action2 "_" Exp Proy) AssocLeft],
                    [Prefix (action1 "|>" Exp RIn), Prefix (action1 "<|" Exp LIn)],
                    [Infix (action2 "&" Exp Prod) AssocLeft],
                    [Infix (action2 "|" Exp Sum) AssocLeft],
                    [Infix (action2 ";" Exp Comp) AssocRight]  ]
          action2 s tOp op = try $ symbol s >> return (\x y -> tOp op x y)
          action1 s tOp op = try $ symbol s >> return (\x -> tOp op x Init)

ground :: Parser Type
ground = (symbol "0" >> return T0)
    <|> (symbol "1" >> return T1)
    <|> (symbol "Num" >> return TNum)
    <|> do {symbol "["; t <- tipo; symbol "]"; return $ TList t}
    <|> do {symbol "("; t <- tipo; symbol ")"; return t}
    <?> "type expression"

tipo  :: Parser Type
tipo = buildExpressionParser table ground <?> "type expression"
    where table = [ [Infix (symbol "*">> return TProd) AssocRight],
                    [Infix (symbol "+">> return TSum) AssocRight],
                    [Infix (symbol "=>">> return TExp) AssocRight]
                  ]

lambda :: Parser Tree
lambda = do
    arg <- identifier
    reservedOp ":"
    t <- tipo
    reservedOp "->"
    body <- fun
    return (Abs arg t body)
    <?> "lambda"
