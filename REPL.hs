--
-- repl.hs
--
module Main
where
import qualified System.Console.Haskeline as H
import Control.Monad.Error
import Type
import Read
import Interp

main = repl

repl :: IO () 
repl = H.runInputT H.defaultSettings loop
  where 
    loop :: H.InputT IO ()
    loop = do
      minput <- H.getInputLine "REPL>>> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just "" -> loop
        Just input -> do
          -- printExp (interp (readExp input))
          case readExp input of
            Right v -> printExp (interp v)
          loop

-- printExp :: ScmExp -> H.InputT IO ()
--printExp x = H.outputStrLn (show x)
          
printExp x = case x of
  Right v -> H.outputStrLn (show v)
  Left err -> H.outputStrLn (show err)
