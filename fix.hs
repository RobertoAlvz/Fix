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
--               case parse program "" raw of
               case parse solve "source code" raw of
                   Right iotr -> do tr <- iotr
--                                    putStr tr
                                    case parse program "referenced code" tr of
                                        Right tf -> case checkTop [] tf of
                                            Right tp -> do ps <- getParms tp
                                                           case parse program "augmented code" $ tr++" "++(concat $ intersperse " " ps) of
                                                               Right ts -> do v <- eval [] ts
                                                                              putStr $ (head args)++" = "
                                                                              print v
                                                               Left serr -> print serr
                                            Left terr -> print terr
                                        Left rerr -> print rerr
                   Left err -> print err
        else print "Uso: fix <file>"

getParms :: Type -> IO [String]
getParms (TExp t s) =  do
    putStr $ (show t) ++ ": "
    hFlush stdout
    l <- getLine
    do case parse solve "argument reference" l of
            Right r -> do rs <- r
                          case parse program "argument" rs of
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
            Left rerr -> do print rerr
                            getParms (TExp t s)

getParms _ = return []
