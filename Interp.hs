{-# LANGUAGE FlexibleContexts #-}
module Interp (
  interp,
  interpAndPrint
  ) where
import Type
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Data.Maybe

import Read

isBound :: Env -> String -> IO Bool
-- isBound envRef var = 
--   readIORef envRef >>= return . maybe False (const True) . lookup var
isBound envRef var = 
  liftM (isJust . lookup var) (readIORef envRef)

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
    (liftIO . (`writeIORef` value))
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
interpAndPrint env e = interpString env e >>= putStrLn

interpString env e = 
  runIOThrows $ liftM show $ liftThrows (readExp e) >>= interp env

primitives = [("+",
               \env operand -> do
                 args <- interpOperand env operand
                 args' <- mapM unpackNum args
                 return $ ScmInt (foldl1 (+) args'))]


apply :: String -> Env -> [ScmExp] -> ThrowsError ScmExp
apply f env args = case lookup f primitives of
  Nothing -> throwError $ ScmNotFunction "Unrecognized primitive" f
--  Just op -> _ TODO

interp :: Env -> ScmExp -> ErrorT ScmError IO ScmExp
interp env x | selfEvaluating x = return x
interp env (ScmSymbol "quote" `ScmCons` x `ScmCons` ScmEmptyList) = return x
interp env (ScmSymbol id) = getVar env id

-- apply ver.
interp env (ScmSymbol f `ScmCons` operand `ScmCons` ScmEmptyList) = do
  args <- interpOperand env operand
  case (apply f env args) of
    Right x -> return x
    Left err -> throwError err

-- car, cdr, cons
interp env (ScmSymbol "cons" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = 
  do
    x' <- interp env x
    y' <- interp env y
    return $ x' `ScmCons` y'
interp env (ScmSymbol "car" `ScmCons` x `ScmCons` ScmEmptyList) = do
  x' <- interp env x
  case x' of
    a `ScmCons` _ -> return a
    _ -> throwError $ ScmTypeMismatch "cons" x
    
interp _ (ScmSymbol "car" `ScmCons` operand) =
  throwError $ ScmNumArgsError 1 operand
    
interp env (ScmSymbol "cdr" `ScmCons` x `ScmCons` ScmEmptyList) = do
  x' <- interp env x
  case x' of
    _ `ScmCons` b -> return b
    _ -> throwError $ ScmTypeMismatch "cons" x
interp _ (ScmSymbol "cdr" `ScmCons` operand) = 
  throwError $ ScmNumArgsError 1 operand

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
--  
interp env (ScmSymbol "define" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  interp env form >>= defineVar env var
interp env (ScmSymbol "set!" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  interp env form >>= setVar env var
-- Equal?
interp env (ScmSymbol "eq?" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- interp env x
  y' <- interp env y
  case eqv x' y' of
    Right z -> return z
    Left err -> throwError err
interp env (ScmSymbol "eq?" `ScmCons` operand) =
  throwError $ ScmNumArgsError 2 operand

eqv :: ScmExp -> ScmExp -> ThrowsError ScmExp
eqv (ScmInt a1) (ScmInt a2) = return $ ScmBool $ a1 == a2
eqv (ScmBool a1) (ScmBool a2) = return $ ScmBool $ a1 == a2
eqv (ScmCons a1 b1) (ScmCons a2 b2) = 
  return $ ScmBool $ a1 `eqv'` a2 && b1 `eqv'` b2
  where
    eqv' x y = case eqv x y of
      Left err -> False
      Right (ScmBool val) -> val
      Right z -> False
eqv (ScmCons _ _) ScmEmptyList = return $ ScmBool False
eqv ScmEmptyList (ScmCons _ _) = return $ ScmBool False
eqv (ScmSymbol a1) (ScmSymbol a2) = return $ ScmBool $ a1 == a2
eqv (ScmChar a1) (ScmChar a2) = return $ ScmBool $ a1 == a2
eqv (ScmString a1) (ScmString a2) = return $ ScmBool $ a1 == a2
eqv ScmEmptyList ScmEmptyList = return $ ScmBool True

--
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