{- |
Module      : Type
-}
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
  runIOThrows,
  extractValue
  ) where
--ok but warn
-- import Control.Monad.Error
-- import Control.Monad.Error.Class 

import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Data.IORef

-- |The Scheme data type.TODO; check R7RS 7.1.3 
data ScmExp = ScmInt Int 
            | ScmBool Bool
            | ScmCons { car :: ScmExp, cdr :: ScmExp }
            | ScmSymbol String | ScmChar Char | ScmString String
            | ScmEmptyList 
            | ScmPrimitiveFunc (ScmExp -> ThrowsError ScmExp)
            | ScmFunc { params :: [String],
                        vararg :: Maybe String,
                        body :: ScmExp,
                        closure :: Env }

instance Eq ScmExp where
  ScmInt x == ScmInt y = x == y
  ScmBool x == ScmBool y = x == y
  ScmCons x x' == ScmCons y y' = x == y && x' == y'
  ScmSymbol x == ScmSymbol y = x == y
  ScmChar x == ScmChar y = x == y
  ScmString x == ScmString y = x == y
  ScmEmptyList == ScmEmptyList = True
  ScmPrimitiveFunc _ == ScmPrimitiveFunc _ = False
  ScmFunc {} == ScmFunc {} = False

instance Show ScmExp where show = showExp

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

showExp :: ScmExp -> String
showExp x = case x of
  ScmInt n -> show n
  ScmBool True -> "#t"
  ScmBool False -> "#f"
  ScmCons _ _ -> showCons x
  ScmSymbol a -> a
  ScmChar a -> show a
  ScmString s -> "\"" ++ s ++ "\""
  ScmEmptyList -> "()"
  ScmPrimitiveFunc _ -> "<primitive>"
  ScmFunc {params = args, vararg = varargs, body = body, closure = env} ->
    "(lambda (" ++ unwords (map show args) ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
-- |Show Cons.
showCons :: ScmExp -> String
showCons a = f a "("
  where
    f x s = g x (s ++ showExp (car x))
    g x s = if consp (cdr x) then
              f (cdr x) (s ++ " ")
            else
              case cdr x of 
                ScmEmptyList -> s ++ ")"
                _ -> s ++ " . " ++ showExp (cdr x) ++ ")"
-- |Show expression simple version.
showExp' :: ScmExp -> String
showExp' (ScmInt a) = "ScmInt " ++ show a
showExp' (ScmBool True) = "#t"
showExp' (ScmBool False) = "#f"
showExp' (ScmCons a b) = "ScmCons " ++ showExp' a ++ " " ++ showExp' b
showExp' (ScmSymbol a) = "ScmSymbol " ++ a
showExp' (ScmChar a) = "ScmChar " ++ show a
showExp' (ScmString s) = "ScmString " ++ s
showExp' ScmEmptyList = "ScmEmptyList"

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

-- liftThrows :: MonadError e m => Either e a -> m a
liftThrows :: ThrowsError a -> ScmIOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- runIOThrows :: Monad m => ErrorT ScmError m String -> m String
--type IOThrowsError = ErrorT ScmError IO
runIOThrows :: ScmIOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Environment
type Env = IORef [(String, IORef ScmExp)]

nullEnv :: IO Env
nullEnv = newIORef []


