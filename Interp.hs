{-# LANGUAGE FlexibleContexts #-}
module Interp (
  interp,
  interpAndPrint
  ) where
import Type
import Control.Monad.Error
import Data.IORef
import Read

isBound :: Env -> String -> IO Bool
isBound envRef var = 
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: (MonadIO m, MonadError ScmError m) =>
          Env -> String -> m ScmExp
getVar envRef var = do 
  env <- liftIO $ readIORef envRef
  maybe (throwError $ ScmUnboundVar "Getting an unbound variable: " var)
    (liftIO . readIORef)
    (lookup var env)
    
setVar :: (MonadIO m, MonadError ScmError m) =>
          Env -> String -> ScmExp -> m ScmExp
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ ScmUnboundVar "Getting an unbound variable: " var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

-- defineVar :: Env -> String -> ScmExp -> ScmIOThrowsError ScmExp
defineVar :: (MonadIO m, MonadError ScmError m) =>
             Env -> String -> ScmExp -> m ScmExp
defineVar envRef var value = do
  alreadyDefind <- liftIO $ isBound envRef var
  if alreadyDefind then
    setVar envRef var value >> return value
    else liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

interpAndPrint :: Env -> String -> IO ()
interpAndPrint env exp = interpString env exp >>= putStrLn

interpString env exp = 
  runIOThrows $ liftM show $ (liftThrows $ readExp exp) >>= interp env

interp :: Env -> ScmExp -> ErrorT ScmError IO ScmExp
interp env x | selfEvaluating x = return x
interp env (ScmSymbol "quote" `ScmCons` x `ScmCons` ScmEmptyList) = return x
interp env (ScmSymbol id) = getVar env id
interp env (ScmSymbol "+" `ScmCons` operand) = numericBinOp env (+) operand
interp env (ScmSymbol "-" `ScmCons` operand) = numericBinOp env (-) operand
interp env (ScmSymbol "*" `ScmCons` operand) = numericBinOp env (*) operand
interp env (ScmSymbol ">" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- interp env x
  y' <- interp env y
  liftM2 (\a b -> ScmBool (a > b)) (unpackNum x') (unpackNum y')
interp env (ScmSymbol "=" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- interp env x
  y' <- interp env y
  return $ ScmBool (x' == y')
interp env (ScmSymbol "define" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  interp env form >>= defineVar env var
interp env (ScmSymbol "set!" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  interp env form >>= setVar env var

interpOperand :: Env -> ScmExp -> ErrorT ScmError IO [ScmExp]  
interpOperand env x = mapM (interp env) (toList x)

numericBinOp :: Env -> (Int -> Int -> Int) -> ScmExp -> ErrorT ScmError IO ScmExp
numericBinOp env op operand =
  if length (toList operand) /= 2 then
    throwError $ ScmNumArgsError 2 operand
    else
    do
      args <- interpOperand env operand
      args'' <- mapM unpackNum args
      return $ ScmInt (foldl1 op args'')

unpackNum :: MonadError ScmError m => ScmExp -> m Int
unpackNum (ScmInt n) = return n
unpackNum notNum = throwError $ ScmTypeMismatch "int" notNum

-- Check proper list or not
isProperList :: ScmExp -> Bool
isProperList x = case x of
  ScmEmptyList -> True
  ScmCons _ b -> isProperList b
  _ -> False

-- convert Scheme List into Haskell List
-- TODO; use ScmError;
toList :: ScmExp -> [ScmExp]
toList x = if isProperList x then
             case x of
               ScmCons a ScmEmptyList -> [a]
               ScmCons a as -> a:toList as
           else 
             error ("Expect proper list but got: " ++ show x)