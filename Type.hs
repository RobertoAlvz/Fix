module Type where
import Parser
import Data.List
import Control.Monad.Except
import Control.Monad.Reader

data TypeError = Mismatch Type Type | NonFunction Type | UnknownVar String | MultipleTypeList [Type] | ProyectionError deriving Show

type Check = ExceptT TypeError (Reader Env)

type Env = [(String,Type)]

instance Eq Type where
    T0 == T0 = True
    T1 == T1 = True
    TNum == TNum = True
    (TList t) == (TList s) = t==s || elem T1 [t,s]
    (TProd a b) == (TProd c d) = a==c && b==d
    (TProd a T1) == c = a==c
    (TProd T1 a) == c = a==c
    c == (TProd a T1) = a==c
    c == (TProd T1 a) = a==c
    (TSum a b) == (TSum c d) = a==c && b==d
    (TSum a T0) == c = a==c
    (TSum T0 a) == c = a==c
    c == (TSum a T0) = a==c
    c == (TSum T0 a) = a==c
    (TExp a b) == (TExp c d) = a==c && b==d
    (TExp T1 a) == c = a==c
    c == (TExp T1 a) = a==c
    _ == _ = False

extend :: (String, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (String, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: String -> Check Type
lookupVar x = do
    env <- ask
    case lookup x env of
        Just e  -> return e
        Nothing -> throwError $ UnknownVar x

fromEitherType x = case x of
    Right t -> t
    Left err -> error $ show err

check :: Tree -> Check Type
check expr = case expr of
    Init        -> return T1
    Id s        -> lookupVar s
    Num _       -> return TNum
    Lst ls      -> do env <- ask
                      let ts = nub $ map (fromEitherType.checkTop env) ls in if length ts == 1 then return $ TList $ head ts else if length ts == 0 then return $ TList T1 else throwError $ MultipleTypeList ts
    Prd t s     -> do env <- ask
                      return $ TProd (fromEitherType $ checkTop env t) (fromEitherType $ checkTop env s)
    Exp Prod f g-> do env <- ask
                      case fromEitherType $ checkTop env f of
                        TExp a b -> case fromEitherType $ checkTop env g of
                                        TExp c d -> if a==c then return $ TExp a (TProd b d) else throwError $ Mismatch a c
                                        t        -> throwError $ NonFunction t
                        s        -> throwError $ NonFunction s
    Exp Sum f g -> do env <- ask
                      case fromEitherType $ checkTop env f of
                        TExp a b -> case fromEitherType $ checkTop env g of
                                        TExp c d -> if b==d then return $ TExp (TSum a c) d else throwError $ Mismatch b d
                                        t        -> throwError $ NonFunction t
                        s        -> throwError $ NonFunction s
    Exp Proy p i-> do env <- ask
                      case fromEitherType $ checkTop env p of
                        TProd t s -> case fromEitherType $ checkTop env i of
                                        TNum -> let Num n = i
                                                    aux1 = fix (\f t i -> if i<=0 then T1 else case t of
                                                        {TProd a b -> if i==1 then a else f b (i-1); c -> if i==1 then c else T1})
                                                in case n of
                                                    Left m -> return $ aux1 (TProd t s) $ fromInteger m
                                                    _      -> throwError $ ProyectionError
                                        _    -> throwError $ ProyectionError
                        _         -> throwError $ ProyectionError
    Exp Comp f g-> do env <- ask
                      case fromEitherType $ checkTop env f of
                        TExp a b -> case fromEitherType $ checkTop env g of
                                         TExp c d -> if d==a then return $ TExp c b else throwError $ Mismatch a d
                                         t        -> throwError $ NonFunction t
                        s        -> throwError $ NonFunction s
    Exp op a b  -> do env <- ask
                      aux op (fromEitherType $ checkTop env a) (fromEitherType $ checkTop env b)
    Abs x t e   -> do {s <- inEnv (x,t) $ check e; return $ TExp t s}
    App f g     -> do t1 <- check f
                      t2 <- check g
                      case t1 of
                           TExp a b | a==t2 -> return b
                                    | otherwise -> throwError $ Mismatch t2 a
                           t -> throwError $ NonFunction t
    Fix f       -> do env <- ask
                      case fromEitherType $ checkTop env f of
                        TExp a b -> if a==b then return b else throwError $ Mismatch a b
                        t        -> throwError $ NonFunction t

aux op at bt
    | elem op [LIn, RIn] = return $ TSum at bt
    | elem op [Eqs, Lts, Les, Gts, Ges, Neq] = if at==bt then return $ TSum T1 T1 else throwError $ Mismatch at bt
    | elem op [Mod, Add, Dif, Mul, Div, Pow] = if at==TNum && bt==TNum then return TNum else throwError $ Mismatch at bt
    | elem op [Or, And] = case (at, bt) of 
                            (TSum a1 a2, TSum b1 b2) -> if a1==b1 && a2==b2 then return $ TSum a1 a2 else throwError $ Mismatch at bt
                            _                        -> throwError $ Mismatch at bt
    | op == Not = case at of
                       TSum a1 a2 -> return $ TSum a2 a1
                       _          -> throwError $ Mismatch at bt
    | op == Head = case at of
                        TList l -> return l
                        _       -> throwError $ Mismatch at (TList T1)
    | op==Tail = case at of
                        TList l -> return $ TList l
                        _       -> throwError $ Mismatch at (TList T1)
    | op==Concat = case at of
                        TList l -> case bt of
                                       TList k -> if l==k then return $ TList l else throwError $ Mismatch l k
                                       t      ->  throwError $ Mismatch (TList l) t
                        s      -> throwError $ Mismatch s (TList T1)
    | op==Test = case at of
                      TSum _ _ -> case bt of
                                       TProd b1 b2 -> return $ TSum b1 b2
                                       T1          -> return $ TSum T1 T1
                                       _           -> throwError $ Mismatch bt (TProd T1 T1)
                      _          -> throwError $ Mismatch at (TSum T1 T1)
    | otherwise = throwError $ UnknownVar (show op)

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> Tree -> Either TypeError Type
checkTop env x = runCheck env $ (check x)
