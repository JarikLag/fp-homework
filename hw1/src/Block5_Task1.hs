{-# LANGUAGE InstanceSigs #-}

module Block5_Task1 
  ( ArithmeticError (..)
  , Expr (..)

  , eval
  ) where

-- | Data type which represents arithmetical expression.
data Expr 
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving Show

-- | Data type which represents arithmetical error.
data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving Show

instance Eq ArithmeticError where
  (==) :: ArithmeticError -> ArithmeticError -> Bool
  (==) DivisionByZero DivisionByZero = True
  (==) NegativePow    NegativePow    = True
  (==) _              _              = False

data OpType
  = Safe
  | Division
  | Power
  deriving Show


-- | Evaluates given expression and returns result in Left.
-- If something went wrong returns reason in Right.
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = performOp Safe (wrapOp (+)) x y
eval (Sub x y) = performOp Safe (wrapOp (-)) x y
eval (Mul x y) = performOp Safe (wrapOp (*)) x y
eval (Div x y) = performOp Division (wrapOp (div)) x y
eval (Pow x y) = performOp Power (wrapOp (^)) x y

wrapOp :: (Int -> Int -> Int) -> (Int -> Int -> Either ArithmeticError Int)
wrapOp op = \x y -> return $ x `op` y

performOp :: OpType
          -> (Int -> Int -> Either ArithmeticError Int) 
          -> Expr -> Expr -> Either ArithmeticError Int
performOp opT f x y = do
  ex <- eval x
  ey <- eval y
  handleOp opT f ex ey

handleOp :: OpType
         -> (Int -> Int -> Either ArithmeticError Int) -> Int -> Int
         -> Either ArithmeticError Int
handleOp Safe f x y = f x y
handleOp Division f x y
  | y == 0    = Left DivisionByZero
  | otherwise = f x y
handleOp Power f x y
  | y < 0     = Left NegativePow
  | otherwise = f x y 



