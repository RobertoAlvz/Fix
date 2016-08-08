import Parser
import Type
import Control.Monad

main = forever $ do
    putStr "fix>> "
    prog <- getLine
    case parse program "" prog of
         Right t -> do let chk = checkTop [] t
                       case chk of
                            Right tp -> putStrLn $ "Type: "++(show tp)
                            Left terr -> print terr
         Left err -> print err
