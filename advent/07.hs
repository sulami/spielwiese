module Main where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word16)

import Data.HashMap.Lazy (HashMap, delete, filterWithKey, fromList, lookup, map, union, elems)

data AExpr = AProvide String
           | ANot String
           | AAnd String String
           | AOr String String
           | ALShift String Int
           | ARShift String Int
           | AValue Word16
           deriving (Show)

isValue :: AExpr -> Bool
isValue (AValue _) = True
isValue _          = False

isVal :: HashMap String AExpr -> String -> Bool
isVal el e = any (`elem` ['0'..'9']) e || (isJust (lookup e el) && isValue (fromJust (lookup e el)))

solvable :: HashMap String AExpr -> AExpr -> Bool
solvable _  (AValue e)    = False
solvable el (AProvide e)  = isVal el e
solvable el (ANot e)      = isVal el e
solvable el (AAnd e0 e1)  = isVal el e0 && isVal el e1
solvable el (AOr e0 e1)   = isVal el e0 && isVal el e1
solvable el (ALShift e n) = isVal el e
solvable el (ARShift e n) = isVal el e

aeComplement :: AExpr -> AExpr
aeComplement (AValue e) = AValue $ complement e

aeAnd :: AExpr -> AExpr -> AExpr
aeAnd (AValue e0) (AValue e1) = AValue $ e0 .&. e1

aeOr :: AExpr -> AExpr -> AExpr
aeOr (AValue e0) (AValue e1) = AValue $ e0 .|. e1

aeShiftL :: AExpr -> Int -> AExpr
aeShiftL (AValue e) n = AValue $ shiftL e n

aeShiftR :: AExpr -> Int -> AExpr
aeShiftR (AValue e) n = AValue $ shiftR e n

evalAExpr :: HashMap String AExpr -> AExpr -> AExpr
evalAExpr el (AValue e)    = AValue e
evalAExpr el (AProvide e)  = evalAE el e
evalAExpr el (ANot e)      = aeComplement $ evalAE el e
evalAExpr el (AAnd e0 e1)  = evalAE el e0 `aeAnd` evalAE el e1
evalAExpr el (AOr e0 e1)   = evalAE el e0 `aeOr` evalAE el e1
evalAExpr el (ALShift e n) = aeShiftL (evalAE el e) n
evalAExpr el (ARShift e n) = aeShiftR (evalAE el e) n

evalAE :: HashMap String AExpr -> String -> AExpr
evalAE el e = if any (`elem` ['0'..'9']) e
                then AValue $ read e
                else evalAExpr el . fromJust $ lookup e el

solveAll :: HashMap String AExpr -> HashMap String AExpr
solveAll el = if all isValue (elems el)
                then el
                else
                  let solving = filterWithKey (\k v -> solvable el v) el
                      solved = Data.HashMap.Lazy.map (evalAExpr el) solving
                  in solveAll $ union solved el

parseAExpr :: [String] -> (String, AExpr)
parseAExpr w = case length w of
  3 -> (last w, AProvide $ head w)
  4 -> (last w, ANot $ w !! 1)
  5 -> case w !! 1 of
    "AND"    -> (last w, AAnd (head w) (w !! 2))
    "OR"     -> (last w, AOr  (head w) (w !! 2))
    "LSHIFT" -> (last w, ALShift (head w) (read $ w !! 2))
    "RSHIFT" -> (last w, ARShift (head w) (read $ w !! 2))

input :: IO (HashMap String AExpr)
input = fromList . Prelude.map (parseAExpr . words) . lines
          <$> readFile "07.input"

main = do
  i <- input
  let values = solveAll i
  print $ lookup "a" values
  putStrLn "Second solution: just change wire b to '956 -> b'"

