{-# LANGUAGE FlexibleContexts #-}
--
-- REPL.hs
--
module Main
where
import qualified System.Console.Haskeline as H
import Control.Monad.Error
import Control.Monad.IO.Class
import Type
import Interp
import System.IO

main :: IO ()
main = repl

-- original
flushStr str = putStr str >> hFlush stdout
readPrompt prompt = flushStr prompt >> getLine

until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
    else action result >> until_ pred prompt action
-- Haskeline version.
repl :: IO ()
repl = do
  env <- primitiveBindings
  H.runInputT H.defaultSettings (loop env)
  where
    loop :: Env -> H.InputT IO ()
    loop env = do
      minput <- H.getInputLine "REPL>>> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          evalAndHaskelineOutput env input
          loop env

evalAndHaskelineOutput :: Env -> String -> H.InputT IO ()
evalAndHaskelineOutput env str = liftIO (evalString env str) >>= H.outputStrLn

-- ghc 7.8.2 error;
-- <interactive>:1274:23:
--     No instance for (MonadError ScmError (H.InputT IO))
--       arising from a use of ‘repl4’
--     In the second argument of ‘(>>=)’, namely ‘repl4’
--     In the expression: primitiveBindings >>= repl4
--     In an equation for ‘it’: it = primitiveBindings >>= repl4
-- *Main> :t repl4
-- repl4
--   :: (H.MonadException m, MonadError ScmError (H.InputT m),
--       MonadIO (H.InputT m)) =>
--      Env -> m ()
