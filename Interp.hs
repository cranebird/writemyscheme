{-# LANGUAGE FlexibleContexts #-}
module Interp (
  eval,
  evalString,
  primitiveBindings
  ) where
import Type
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Data.Maybe

import Read

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

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

bindVars :: Env -> [(String, ScmExp)] -> IO Env
bindVars envRef bindings = 
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bs env = liftM (++ env) (mapM addBinding bs)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

evalString :: Env -> String -> IO String
evalString env e = 
  runIOThrows $ liftM show $ liftThrows (readExp e) >>= eval env
  
primitives :: [(String, ScmExp -> ThrowsError ScmExp)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              (">", numBoolBinOp (>)),
              ("<", numBoolBinOp (<)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("=", numBoolBinOp (==)),
              ("cons", primCons),
              ("car", primCar),
              ("cdr", primCdr),
              ("eq?", eqv)
             ]

-- eval :: Env -> ScmExp -> ErrorT ScmError IO ScmExp
-- eval :: (MonadError ScmError m, MonadIO m) => Env -> ScmExp -> m ScmExp
eval :: Env -> ScmExp -> ScmIOThrowsError ScmExp
eval env x | selfEvaluating x = return x
eval env (ScmSymbol "quote" `ScmCons` x `ScmCons` ScmEmptyList) = return x
eval env (ScmSymbol id) = getVar env id
eval env (ScmSymbol "define" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  eval env form >>= defineVar env var
eval env (ScmSymbol "set!" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  eval env form >>= setVar env var

eval env (ScmSymbol "lambda" `ScmCons` operand) =
  makeNormalFunc env (toList (car operand)) (cdr operand)

eval env (f `ScmCons` operand) = do
  f' <- eval env f
  operand' <- evalScmList env operand
  apply f' operand'

-- return length of scheme list.
lengthScmList x = if isProperList x then  
                    f x 0
                  else
                    throwError $ ScmTypeMismatch "list" x
  where
    f ScmEmptyList n = n
    f (x `ScmCons` y) n = f y (n + 1)

-- eval scheme list elements.
evalScmList env x = if isProperList x 
                    then
                       f x ScmEmptyList
                    else 
                      throwError $ ScmTypeMismatch "list" x
  where
    f ScmEmptyList acc = return $ reverseScmList acc
    f (x `ScmCons` y) acc = do
      x' <- eval env x
      f y (x' `ScmCons` acc)
      
-- | reverse scheme list.
reverseScmList x = rev x ScmEmptyList
  where
    rev ScmEmptyList acc = acc
    rev (x `ScmCons` y) acc = rev y (x `ScmCons` acc)
  
-- (lambda (x y) (+ x y))    
-- ScmFunc ["x","y"] Nothing (ScmSymbol "+" `ScmCons` ScmSymbol "x" `ScmCons` ScmSymbol "y" `ScmCons` ScmEmptyList
--     
apply (ScmPrimitiveFunc f) args = liftThrows $ f args
apply (ScmFunc params vararg body closure) args =
  liftIO (bindVars closure $ zip params (toList args)) >>= evalBody
  where
    evalBody env = liftM last $ mapM (eval env) (toList body)

makeNormalFunc env params body = return $ ScmFunc (map showExp params)
                                 Nothing body env

-- compare
eqv :: ScmExp -> ThrowsError ScmExp
eqv (ScmInt a1 `ScmCons` ScmInt a2 `ScmCons` ScmEmptyList) = 
  return $ ScmBool $ a1 == a2
eqv (ScmBool a1 `ScmCons` ScmBool a2 `ScmCons` ScmEmptyList) = 
  return $ ScmBool $ a1 == a2
eqv (ScmCons a1 b1 `ScmCons` ScmCons a2 b2 `ScmCons` ScmEmptyList) = do
  ScmBool v1 <- eqv (a1 `ScmCons` a2)
  ScmBool v2 <- eqv (b1 `ScmCons` b2)
  return $ ScmBool (v1 && v2)
eqv (ScmCons _ _ `ScmCons` ScmEmptyList `ScmCons` ScmEmptyList) = 
  return $ ScmBool False
eqv (ScmEmptyList `ScmCons` ScmCons _ _ `ScmCons` ScmEmptyList) = 
  return $ ScmBool False
eqv (ScmSymbol a1 `ScmCons` ScmSymbol a2 `ScmCons` ScmEmptyList) = 
  return $ ScmBool $ a1 == a2
eqv (ScmChar a1 `ScmCons` ScmChar a2 `ScmCons` ScmEmptyList) = 
  return $ ScmBool $ a1 == a2
eqv (ScmString a1 `ScmCons` ScmString a2 `ScmCons` ScmEmptyList) = 
  return $ ScmBool $ a1 == a2
eqv (ScmEmptyList `ScmCons` ScmEmptyList `ScmCons` ScmEmptyList) = 
  return $ ScmBool True

--
--
-- primitiveBindings :: IO Env
primitiveBindings = 
  nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, f) = (var, ScmPrimitiveFunc f)

primCons args = case args of
  (x `ScmCons` y `ScmCons` ScmEmptyList) -> return $ x `ScmCons` y
  _ -> throwError $ ScmNumArgsError 2 args

primCar args = case args of
  x `ScmCons` ScmEmptyList -> case x of
    a `ScmCons` _ -> return a
    _ -> throwError $ ScmTypeMismatch "cons" x
  _ -> throwError $ ScmNumArgsError 1 args

primCdr args = case args of
  x `ScmCons` ScmEmptyList -> case x of
    _ `ScmCons` b -> return b
    _ -> throwError $ ScmTypeMismatch "cons" x
  _ -> throwError $ ScmNumArgsError 1 args

numericBinOp op args = case args of
  x `ScmCons` y `ScmCons` ScmEmptyList -> do
    x' <- unpackNum x
    y' <- unpackNum y
    return $ ScmInt (x' `op` y')
  _ -> throwError $ ScmNumArgsError 2 args

numBoolBinOp op args = case args of
  x `ScmCons` y `ScmCons` ScmEmptyList -> do
    x' <- unpackNum x
    y' <- unpackNum y
    return $ ScmBool (x' `op` y')
  _ -> throwError $ ScmNumArgsError 2 args

unpackNum :: MonadError ScmError m => ScmExp -> m Int
unpackNum (ScmInt n) = return n
unpackNum notNum = throwError $ ScmTypeMismatch "int" notNum

-- Check proper list or not
isProperList :: ScmExp -> Bool
isProperList x = case x of
  ScmEmptyList -> True
  ScmCons _ b -> isProperList b
  _ -> False

-- | convert Scheme List into Haskell List. TODO; use ScmError;
toList :: ScmExp -> [ScmExp]
toList x = if isProperList x then
             case x of
               ScmCons a ScmEmptyList -> [a]
               ScmCons a as -> a:toList as
           else 
             error ("Expect proper list but got: " ++ show x)
