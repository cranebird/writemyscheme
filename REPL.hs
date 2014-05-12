--
-- REPL.hs
--
module Main
where
import qualified System.Console.Haskeline as H
import Control.Monad.Error
import Type
import Read
import Interp
import System.IO

main = repl2

-- repl :: IO () 
-- repl = H.runInputT H.defaultSettings loop
--   where
--     -- loop :: H.InputT IO ()
--     loop = do
--       minput <- H.getInputLine "REPL>>> "
--       case minput of
--         Nothing -> return ()
--         Just "quit" -> return ()
--         Just "" -> loop
--         Just input -> do
--           case readExp input of
--             Right v -> printExp (interp v)
--           loop

-- repl :: IO () 
repl = H.runInputT H.defaultSettings loop

-- loop :: H.InputT IO ()
-- loop = do
--   minput <- H.getInputLine "REPL>>> "
--   case minput of
--     Nothing -> return ()
--     Just "quit" -> return ()
--     Just "" -> loop
--     Just input -> do
--       readAndEval input
--       loop

loop :: H.InputT IO ()
loop = undefined

-- loop' env = do
--   minput <- H.getInputLine "REPL>>> "
--   case minput of
--     Nothing -> return ()
--     Just "quit" -> return ()
--     Just "" -> loop
--     Just input -> do
--       readAndEval input
--       loop'


readAndEval :: MonadIO m => String -> H.InputT m ()
readAndEval str = case (readExp str) of
  Left err -> H.outputStrLn (show err)
  Right v -> H.outputStrLn (show v)

-- original
flushStr str = putStr str >> hFlush stdout
readPrompt prompt = flushStr prompt >> getLine

until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
    else action result >> until_ pred prompt action

repl2 = nullEnv >>= 
        until_ (== "quit") (readPrompt "REPL>>> ") . interpAndPrint


