module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

type PState = [(String, Integer)]

data Expr = NbdInt Integer
          | NbdSymbol String
          | NbdIsZero Expr [Expr]
          | NbdPrint Expr
          | NbdNothing

instance Show Expr where
  show NbdNothing = "<Nothing>"
  show (NbdInt x) = show x
  show (NbdSymbol x) = x
  show (NbdIsZero x _) = "<isZero " ++ (show x) ++ ">"
  show (NbdPrint x) = "<Print " ++ (show x) ++ ">"

type NbdResult = StateT PState IO

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
eval (NbdPrint x) = do v <- eval x
                       liftIO $ print v
                       return NbdNothing

run :: PState -> [Expr] -> NbdResult Expr
run s0 []     = return NbdNothing
run s0 (x:xs) = do let (v, s1) = runState (return $ eval x) s0
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

parsePrint :: Parser Expr
parsePrint = do char '!' ; skipMany space
                x <- parseValue
                return $ NbdPrint x

parseExpr :: Parser Expr
parseExpr = do skipMany space
               x <- parseExpr
               skipMany space ; eof
               return x

main = do input <- fmap words getContents
          mapM_ print input

