module Interp (
  interp
  ) where
import Type
import Read
import Control.Monad.Error

interpOperand :: ScmExp -> Either ScmError [ScmExp]
interpOperand x = mapM interp (toList x)

interp :: ScmExp -> ThrowsError ScmExp
interp x | selfEvaluating x = return x
interp (ScmSymbol "quote" `ScmCons` x `ScmCons` ScmEmptyList) = return x
interp (ScmSymbol "+" `ScmCons` operand) = numericBinOp (+) operand
interp (ScmSymbol "-" `ScmCons` operand) = numericBinOp (-) operand
interp (ScmSymbol "*" `ScmCons` operand) = numericBinOp (*) operand

interp (ScmSymbol ">" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- interp x
  y' <- interp y
  liftM2 (\a b -> ScmBool (a > b)) (unpackNum x') (unpackNum y')

interp (ScmSymbol "=" `ScmCons` x `ScmCons` y `ScmCons` ScmEmptyList) = do
  x' <- interp x
  y' <- interp y
  return $ ScmBool (x' == y')

numericBinOp :: (Int -> Int -> Int) -> ScmExp -> ThrowsError ScmExp
numericBinOp op operand =
  if length (toList operand) /= 2 then
    throwError $ ScmNumArgsError 2 operand
    else
    do
      args <- interpOperand operand
      args'' <- mapM unpackNum args
      return $ ScmInt (foldl1 op args'')

unpackNum :: ScmExp -> ThrowsError Int
unpackNum (ScmInt n) = return n
unpackNum notNum = throwError $ ScmTypeMismatch "int" notNum

isScmInt :: ScmExp -> Bool
isScmInt x = case x of
  ScmInt _ -> True
  _ -> False

-- proper list
isProperList :: ScmExp -> Bool
isProperList x = case x of
  ScmEmptyList -> True
  ScmCons _ b -> isProperList b
  _ -> False

toList :: ScmExp -> [ScmExp]
toList x = if isProperList x then
             case x of
               ScmCons a ScmEmptyList -> [a]
               ScmCons a as -> a:toList as
           else 
             error ("Expect proper list but got: " ++ show x)