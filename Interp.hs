{-# LANGUAGE FlexibleContexts #-}
module Interp (
  eval,
  evalAndPrint,
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
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env e = evalString env e >>= putStrLn

evalString env e = 
  runIOThrows $ liftM show $ liftThrows (readExp e) >>= eval env

primitives :: [(String, ScmExp -> ThrowsError ScmExp)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              -- ("cons", primCons),
              -- ("car", primCar),
              -- ("cdr", primCdr),
              (">", numBoolBinOp (>)),
              ("<", numBoolBinOp (<)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("=", numBoolBinOp (==)) 
             ]

primCons env operand = case operand of
  (x `ScmCons` y `ScmCons` ScmEmptyList) -> do
    x' <- eval env x
    y' <- eval env y
    return $ x' `ScmCons` y'
  _ -> throwError $ ScmNumArgsError 2 operand
  
primCar env operand = case operand of
  x `ScmCons` ScmEmptyList -> do
    x' <- eval env x
    case x' of
      a `ScmCons` _ -> return a
      _ -> throwError $ ScmTypeMismatch "cons" x
  _ -> throwError $ ScmNumArgsError 1 operand

primCdr env operand = case operand of
  x `ScmCons` ScmEmptyList -> do
    x' <- eval env x
    case x' of
      _ `ScmCons` b -> return b
      _ -> throwError $ ScmTypeMismatch "cons" x
  _ -> throwError $ ScmNumArgsError 1 operand


eval :: Env -> ScmExp -> ErrorT ScmError IO ScmExp
eval env x | selfEvaluating x = return x
eval env (ScmSymbol "quote" `ScmCons` x `ScmCons` ScmEmptyList) = return x
eval env (ScmSymbol id) = getVar env id
eval env (ScmSymbol "define" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  eval env form >>= defineVar env var
eval env (ScmSymbol "set!" `ScmCons` ScmSymbol var `ScmCons`
            form `ScmCons` ScmEmptyList) =
  eval env form >>= setVar env var
eval env (ScmSymbol "eq?" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- eval env x
  y' <- eval env y
  case eqv x' y' of
    Right z -> return z
    Left err -> throwError err
eval env (ScmSymbol "eq?" `ScmCons` operand) =
  throwError $ ScmNumArgsError 2 operand
eval env (ScmSymbol f `ScmCons` operand) = do
  f' <- eval env (ScmSymbol f)
  operand' <- evalScmList env operand
  apply f' operand'

-- lisp の list の長さをかえす
lengthScmList x = f x 0
  where
    f ScmEmptyList n = n
    f (x `ScmCons` y) n = f y (n + 1)

-- lisp の list の要素を順に評価する
evalScmList env x = f x ScmEmptyList
  where
    f ScmEmptyList acc = return $ reverseScmList acc
    f (x `ScmCons` y) acc = do
      x' <- eval env x
      f y (x' `ScmCons` acc)
-- lisp の list を逆順にする
reverseScmList x = rev x ScmEmptyList
  where
    rev ScmEmptyList acc = acc
    rev (x `ScmCons` y) acc = rev y (x `ScmCons` acc)
  
apply (ScmPrimitiveFunc f) args = liftThrows $ f args

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
--
-- primitiveBindings :: IO Env
primitiveBindings = 
  nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, f) = (var, ScmPrimitiveFunc f)

numericBinOp op args = 
  if lengthScmList args /= 2 then
    throwError $ ScmNumArgsError 2 args
  else
    do
      x <- unpackNum (car args)
      y <- unpackNum (car (cdr args))
      return $ ScmInt (x `op` y)
      
numBoolBinOp op args =
  if lengthScmList args /= 2 then
    throwError $ ScmNumArgsError 2 args
  else
    do
      x <- unpackNum (car args)
      y <- unpackNum (car (cdr args))
      return $ ScmBool (x `op` y)

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