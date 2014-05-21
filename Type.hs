{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Type (
  ScmExp(..),
  consp,
  selfEvaluating,
  showExp,
  showExp',  
  showCons,
  ThrowsError,
  ScmError(..),
  Env,
  nullEnv,
  ScmIOThrowsError,
  liftThrows,
  runIOThrows  
  ) where
--ok but warn
-- import Control.Monad.Error
-- import Control.Monad.Error.Class 

import Control.Monad.Error
import Control.Monad.Trans.Except

import Text.ParserCombinators.Parsec
import Data.IORef

-- TODO; check R7RS 7.1.3 

data ScmExp = ScmInt Int 
            | ScmBool Bool
            | ScmCons { car :: ScmExp, cdr :: ScmExp }
            | ScmSymbol String | ScmChar Char | ScmString String
            | ScmEmptyList 
            | PrimitiveFunc (ScmExp -> ThrowsError ScmExp)
-- | ScmNumber Int
-- | ScmQuote ScmExp
-- ScmVector [ScmType] | ScmByteVector ? | ScmProc ? | 
-- ScmRecord ? | ScmPort

instance Eq ScmExp where
  ScmInt x == ScmInt y = x == y
  ScmBool x == ScmBool y = x == y
  ScmCons x x' == ScmCons y y' = x == y && x' == y'
  ScmSymbol x == ScmSymbol y = x == y
  ScmChar x == ScmChar y = x == y
  ScmString x == ScmString y = x == y
  ScmEmptyList == ScmEmptyList = True
  PrimitiveFunc _ == PrimitiveFunc _ = False

instance Show ScmExp where
  show = showExp

infixr 0 `ScmCons`

consp :: ScmExp -> Bool
consp x = case x of
  ScmCons _ _ -> True
  _ -> False

selfEvaluating :: ScmExp -> Bool
selfEvaluating x = case x of
  ScmInt _ -> True
  ScmBool _ -> True
  ScmChar _ -> True
  ScmString _ -> True
  _ -> False
  -- ScmNumber _ -> True
  -- ScmVector -> True
  -- ScmByteVector -> True

showExp :: ScmExp -> String
showExp (ScmInt a) = show a
showExp (ScmBool True) = "#t"
showExp (ScmBool False) = "#f"
showExp a@(ScmCons _ _) = showCons a
showExp (ScmSymbol a) = a
showExp (ScmChar a) = show a
showExp (ScmString s) = "\"" ++ s ++ "\""
showExp ScmEmptyList = "()"
showExp (PrimitiveFunc _) = "<primitive>"
-- showExp (ScmNumber a) = show a

-- show simple
showExp' :: ScmExp -> String
showExp' (ScmInt a) = "ScmInt " ++ show a
showExp' (ScmBool True) = "#t"
showExp' (ScmBool False) = "#f"
showExp' (ScmCons a b) = "ScmCons " ++ showExp' a ++ " " ++ showExp' b
showExp' (ScmSymbol a) = "ScmSymbol " ++ a
showExp' (ScmChar a) = "ScmChar " ++ show a
showExp' (ScmString s) = "ScmString " ++ s
showExp' ScmEmptyList = "ScmEmptyList"

showCons a = step1 a ""
  where
    step1 x res = step2 x (res ++ "(")
    step2 x res = step3 x (res ++ showExp (car x))
    step3 x res = if consp (cdr x)
                  then
                    step2 (cdr x) (res ++ " ")
                  else
                    step4 x res
    step4 x res = case (cdr x) of 
      ScmEmptyList -> res ++ ")"
      _ -> res ++ " . " ++ showExp (cdr x) ++ ")"

-- Error
data ScmError = ScmNumArgsError Int ScmExp
              | ScmTypeMismatch String ScmExp
              | ScmParserError ParseError
              | ScmBadSpecialForm String ScmExp
              | ScmNotFunction String String
              | ScmUnboundVar String String
              | ScmDefault String

showError :: ScmError -> String
showError e = "Scm: " ++ case e of
  ScmNumArgsError expected found -> "Expected " ++ show expected 
                            ++ " args, but found " ++ showExp found
  ScmTypeMismatch expected found -> "Invalid type: expected " ++ expected
                                 ++ ", found " ++ showExp found
  ScmParserError err -> "Parse error at: " ++ show err
  ScmBadSpecialForm message form -> message ++ ": " ++ showExp form
  ScmNotFunction message func -> message ++ ": " ++ func
  ScmUnboundVar message varname -> message ++ ": " ++ varname
  ScmDefault message -> message

instance Show ScmError where
  show = showError

instance Error ScmError where
  noMsg = ScmDefault "Scm: An error has occured"
  strMsg = ScmDefault

type ThrowsError = Either ScmError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type ScmIOThrowsError = ErrorT ScmError IO

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: Monad m => ErrorT ScmError m String -> m String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Environment
type Env = IORef [(String, IORef ScmExp)]

-- nullEnv :: IO Env
nullEnv = newIORef []
--
primitiveBindings :: IO Env
primitiveBindings = undefined
--primitiveBindings = nullEnv >>= 
-- testx :: IO (ScmIOThrowsError ScmExp)

