module Eval where

import Parser
import Data.List
import Data.IORef

data Value = V1 | VNum (Either Integer Double) | VList [Value] | VProd Value Value | VSum (Either Value Value) | VFunction (RefVal -> IO Value)

instance Show Value where
    show V1 = "*"
    show (VNum (Left n)) = show n
    show (VNum (Right n)) = show n
    show (VList vs) = "["++(concat $ intersperse ", " $ map show vs)++"]"
    show (VProd a b) = "("++(show a)++", "++(show b)++")"
    show (VSum (Left n)) = "<"++(show n)++"<"
    show (VSum (Right n)) = ">"++(show n)++">"
    show (VFunction _) = "<<Function>>"

instance Eq Value where
    a == b = compare a b == EQ 

instance Ord Value where
    compare (VFunction _) (VFunction _) = EQ
    compare V1 V1 = EQ
    compare (VNum a) (VNum b) = compare (fromNum a) (fromNum b)
    compare (VList a) (VList b) = if length a /= length b
        then case compare (VList $ zipWith (\x y -> x) a b) (VList $ zipWith (\x y -> y) a b) of
            EQ -> compare (length a) (length b)
            r  -> r
        else let aux = dropWhile (==EQ) $ zipWith compare a b in if null aux then EQ else head aux
    compare (VProd a1 a2) (VProd b1 b2) = compare (compare a1 b1) (compare a2 b2)
    compare (VSum (Left a)) (VSum (Left b)) = compare a b
    compare (VSum (Right a)) (VSum (Left b)) = compare a b
    compare (VSum a) (VSum b) = compare b a
    compare _ _ = LT

type Env = [(String, IORef RefVal)]

type RefVal = () -> IO Value

lookupEnv :: Env -> String -> IO (IORef RefVal)
lookupEnv [] y = error $ "Unbound Variable " ++ y
lookupEnv ((x, v) : xs) n = if x == n then return v else lookupEnv xs n

force :: IORef RefVal -> IO Value
force ref = do
    th <- readIORef ref
    v <- th ()
    update ref v
    return v

mkRefVal :: Env -> String -> Tree -> (RefVal -> IO Value)
mkRefVal env x body = \a -> do
    a' <- newIORef a
    eval ((x, a') : env) body

update :: IORef RefVal -> Value -> IO ()
update ref v = do
    writeIORef ref (\() -> return v)
    return ()

eval :: Env -> Tree -> IO Value
eval env ex = case ex of
    Init            -> return V1
    Id s            -> do rv <- lookupEnv env s
                          v <- force rv
                          return v
    Num n           -> return $ VNum n
    Lst ls          -> do {l <- sequence $ map (eval env) ls; return $ VList l }
    Prd t s         -> do t1 <- eval env t
                          s1 <- eval env s
                          return $ VProd t1 s1
    Exp Prod f g    -> do f1 <- eval env f
                          case f1 of
                               VFunction fc -> do g1 <- eval env g
                                                  case g1 of
                                                       VFunction gc -> return $ VFunction $ \t-> do { f1 <- fc t; g1 <- gc t; return $ VProd f1 g1 }
                                                       _            -> error "Nonfunction Product"
                               _            -> error "Nonfunction Product"
                               
    Exp Sum f g     -> do f1 <- eval env f
                          case f1 of
                               VFunction fc -> do g1 <- eval env g
                                                  case g1 of
                                                       VFunction gc -> return $ VFunction $ \t-> do{VSum a <- t (); either (fc. \x ()->return x) (gc. \x ()->return x) a}
                                                       _            -> error "Nonfunction Sum"
                               _            -> error "Nonfunction Sum"
                               
    Exp Proy p i    -> do f1 <- eval env i
                          case f1 of
                               VNum n -> if (fromNum n)<=0 then error "Negative Proyection" else getProy env p (truncate $ fromNum n)
                               _      -> error "Non Numeric Proyection"
                               
    Exp Comp f g    -> do f1 <- eval env f
                          case f1 of
                               VFunction fc -> do g1 <- eval env g
                                                  case g1 of
                                                    VFunction gc -> return $ VFunction $ (\t-> fc (\()->gc t))
                                                    _            -> error "Nonfunction Composition"
                               _            -> error "Nonfunction Composition"
                               
    Exp LIn a b     -> do {a1 <- eval env a; return $ VSum $ Left a1}
    Exp RIn a b     -> do {a1 <- eval env a; return $ VSum $ Right a1}
    Exp Eqs a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if compare a1 b1 == EQ then VSum (Left V1) else VSum (Right V1)}
    Exp Neq a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if compare a1 b1 == EQ then VSum (Right V1) else VSum (Left V1)}
    Exp Lts a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if a1 < b1 then VSum (Left V1) else VSum (Right V1)}
    Exp Gts a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if a1 > b1 then VSum (Left V1) else VSum (Right V1)}
    Exp Les a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if a1 <= b1 then VSum (Left V1) else VSum (Right V1)}
    Exp Ges a b     -> do {a1 <- eval env a; b1 <- eval env b; return $ if a1 >= b1 then VSum (Left V1) else VSum (Right V1)}
    Exp Head a b    -> do a1 <- eval env a
                          case a1 of {VList l -> return (head l); _ -> error "Non List Head"}                               
    Exp Tail a b    -> do a1 <- eval env a
                          case a1 of {VList l -> return $ VList $ tail l; _ -> error "Non List Tail"}
    Exp Concat a b  -> do a1 <- eval env a
                          case a1 of 
                               VList l -> do b1 <- eval env b
                                             case b1 of
                                               VList k -> return $ VList $ l++k
                                               _ ->  error "Non List Concat"
                               _ ->  error "Non List Concat"
                               
    Exp Mod a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                 VNum y -> return . VNum . Right $ mod1 (fromNum x) (fromNum y)
                                                 _ ->  error "Non Numeric modulo"
                               _ ->  error "Non Numeric modulo"
                               
    Exp Add a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                 VNum y -> return . VNum . Right $ (fromNum x)+(fromNum y)
                                                 _ ->  error "Non Numeric addition"
                               _ ->  error "Non Numeric addition"
                               
    Exp Dif a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                VNum y -> return . VNum . Right $ (fromNum x)-(fromNum y)
                                                _ ->  error "Non Numeric diffence"
                               _ ->  error "Non Numeric difference"
                               
    Exp Mul a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                VNum y -> return . VNum . Right $ (fromNum x)*(fromNum y)
                                                _ ->  error "Non Numeric multiplication"
                               _ ->  error "Non Numeric multiplication"
                               
    Exp Div a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                VNum y -> return . VNum . Right $ (fromNum x)/(fromNum y)
                                                _ ->  error "Non Numeric divition"
    Exp Pow a b     -> do a1 <- eval env a
                          case a1 of
                               VNum x -> do b1 <- eval env b
                                            case b1 of
                                                VNum y -> return . VNum . Right $ (fromNum x)**(fromNum y)
                                                _ ->  error "Non Numeric exponentiation"
                               _ ->  error "Non Numeric exponentiation"
    Exp Or a b      -> do a1 <- eval env a
                          case a1 of 
                               VSum x -> do b1 <- eval env b
                                            case b1 of
                                                VSum y -> return $ VSum $ if (isLeft x)||(isLeft y) then Left V1 else Right V1
                                                _ ->  error "Non Logic disjunction"
                               _ ->  error "Non Logic disjunction"
    Exp And a b     -> do a1 <- eval env a
                          case a1 of 
                               VSum x -> do b1 <- eval env b
                                            case b1 of
                                                VSum y -> return $ VSum $ if (isLeft x)&&(isLeft y) then Left V1 else Right V1
                                                _ ->  error "Non Logic conjunction"
                               _ ->  error "Non Logic conjunction"
    Exp Not a b     -> do a1 <- eval env a
                          case a1 of 
                               VSum (Left x) -> return $ VSum $ Right x
                               VSum (Right x) -> return $ VSum $ Left x
                               _ ->  error "Non Logic negation"
    Exp Test a b    -> do a1 <- eval env a
                          case a1 of 
                               VSum (Left x) -> do {b1 <- getProy env b 1; return $ VSum $ Left b1}
                               VSum (Right x)-> do {b1 <- getProy env b 2; return $ VSum $ Right b1}
                               _ ->  error "Non Logic test"
    Abs x _ e       -> return $ VFunction $ mkRefVal env x e
    App f g         -> do f1 <- eval env f
                          case f1 of
                               VFunction aux -> aux (\()-> eval env g)
                               _            -> error "Nonfunction application"
    Fix f           -> eval env (App f (Fix f))

isLeft (Left _) = True
isLeft _ = False

fromNum (Left i) = fromIntegral i
fromNum (Right n) = n

mod1 :: Double->Double->Double
mod1 a b = a - (fromIntegral.floor $ a/b)*b

getProy :: Env -> Tree -> Integer -> IO Value
getProy env (Prd x _) 1 = eval env x
getProy env (Prd _ x) i = getProy env x (i-1)
getProy env (Id s) i = do
    iorv <- lookupEnv env s
    rv <- readIORef iorv
    v <- rv ()
    return $ getPV v i
getProy _ _ _ = return V1 

getPV (VProd x _) 1 = x
getPV (VProd _ x) i = getPV x (i-1)
getPV x _ = x
