{-# LANGUAGE FlexibleContexts #-}
module ArithmeticEvaluation where
import Text.Parsec
import qualified Text.Parsec.Expr as Expr(Assoc(AssocLeft), Operator (Infix), buildExpressionParser)
import Data.Function (on)

data Exp
    =Num Int
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp

expr :: Stream s m Char => ParsecT s u m Exp
expr = Expr.buildExpressionParser table factor
    where table = [[op "*" Mul Expr.AssocLeft],[op "/" Div Expr.AssocLeft],[op "+" Add Expr.AssocLeft],[op "-" Sub Expr.AssocLeft]]
          op s f = Expr.Infix (f <$ string s) --(<*)，它返回其左侧参数的结果
          factor = (between `on` char) '(' ')' expr <|> (Num . read <$> many1 digit)

eval :: Integral a => Exp -> a
eval (Num x) = fromIntegral  x
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

solution :: Integral  a => String -> a
solution = either (const (error "Did not parse"))  eval . parse expr "arithemtic"

runMain :: IO ()
runMain = print $ solution "(1+3)*7/2"