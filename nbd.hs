module Main where

import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

type PState = [(String, Integer)]

data Expr = NbdInt Integer
          | NbdSymbol String
          | NbdIsZero Expr [Expr]
          | NbdNothing

instance Show Expr where
  show NbdNothing = "<Nothing>"
  show (NbdInt x) = show x
  show (NbdSymbol x) = x
  show (NbdIsZero x _) = "<isZero " ++ (show x) ++ ">"

type NbdResult a = Control.Monad.State.State PState a

eval :: Expr -> NbdResult Expr
eval NbdNothing = return NbdNothing
eval (NbdInt x) = return $ NbdInt x
eval (NbdSymbol x) = do vars <- get
                        let val = lookup x vars
                        return $ NbdInt $ fromJust val
eval (NbdIsZero x e) = do y <- nbdIsZero x
                          if y
                            then do s0 <- get
                                    inIf <- run s0 e
                                    return inIf
                            else return NbdNothing

run :: PState -> [Expr] -> NbdResult Expr
run s0 []     = return NbdNothing
run s0 (x:xs) = do let (v, s1) = runState (eval x) s0
                   run s1 xs

nbdIsZero :: Expr -> NbdResult Bool
nbdIsZero e = do v <- eval e
                 return $ isZero v
  where
    isZero (NbdInt n) = n == 0

nbdPrint :: Expr -> NbdResult (IO ())
nbdPrint e = do v <- eval e
                return $ print v

parseInteger :: Parser Expr
parseInteger = do sign <- option "" (string "-")
                  number <- many1 digit
                  return $ NbdInt $ read $ sign ++ number

parseSymbol :: Parser Expr
parseSymbol = do f <- letter
                 r <- many (letter <|> digit)
                 return $ NbdSymbol $ f : r

parseValue :: Parser Expr
parseValue = (try parseInteger) <|> (try parseSymbol)

parseIsZero :: Parser Expr
parseIsZero = do char '?' ; skipMany space
                 x <- parseValue `sepEndBy` (many1 space)
                 char '{'
                 inner <- parseExpr
                 char '}'
                 return inner

parseExpr :: Parser Expr
parseExpr = do skipMany space
               x <- parseExpr
               skipMany space ; eof
               return x

main = do input <- fmap words getContents
          mapM_ print input

