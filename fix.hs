import Parser
import Type
import Eval
import Ref
import Control.Monad
import Data.List
import System.Environment
import System.IO

main = do
    args <- getArgs
    if length args >= 1
        then let file = openFile (head args) ReadMode in
            do f1 <- file
               raw <- hGetContents f1
               case parse program "source code" raw of
                   Right t -> case parse solve "" raw of
                       Right iotr -> do tr <- iotr
                                        case parse program "referenced code" tr of
                                            Right tf -> case checkTop [] t of
                                               Right tp -> do ps <- getParms tp
                                                              case parse program "augmented code" $ raw++(concat $ intersperse " " ps) of
                                                                   Right ts -> do v <- eval [] ts
                                                                                  putStr $ (head args)++" = "
                                                                                  print v
                                                                   Left serr -> print serr
                                               Left terr -> print terr
                       Left err -> print err
                   Left err -> print err
        else print "Uso: fix <file>"

getParms :: Type -> IO [String]
getParms (TExp t s) =  do
    l <- prompt $ (show t) ++ ": "
    do case parse program "argument" l of
            Right t1 -> case checkTop [] t1 of
                        Right tp -> if t==tp
                                       then do ps <- getParms s
                                               return (l:ps)
                                       else do putStrLn "type argument error"
                                               getParms (TExp t s)
                        Left terr-> do print terr
                                       getParms (TExp t s)
            Left perr-> do print perr
                           getParms (TExp t s)

getParms _ = return []

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
