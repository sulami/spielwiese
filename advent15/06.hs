module Main where

import           Data.List (foldl')

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

data CType = TurnOn | TurnOff | Toggle
  deriving (Eq, Show)

data Cmd = Cmd
  { ctype  :: CType
  , cstart :: (Int, Int)
  , cstop  :: (Int, Int)
  } deriving (Eq, Show)

typeParser :: Parser CType
typeParser = do
  c <- try (string "turn on ") <|> try (string "turn off ") <|> string "toggle "
  case c of
    "turn on "  -> return TurnOn
    "turn off " -> return TurnOff
    "toggle "   -> return Toggle

coordParser :: Parser (Int, Int)
coordParser = do
  x <- read <$> many1 digit
  char ','
  y <- read <$> many1 digit
  return (x,y)

cmdParser :: Parser Cmd
cmdParser = Cmd
  <$> typeParser
  <*> coordParser
  <*  string " through "
  <*> coordParser

execCmd :: [Bool] -> Cmd -> [Bool]
execCmd is (Cmd ct (x0,y0) (x1,y1)) = case ct of
  TurnOn -> [ previously x y || inside x y | x <- [0..999], y <- [0..999] ]
  TurnOff -> [ outside x y && previously x y | x <- [0..999], y <- [0..999] ]
  Toggle ->
    [ (inside x y && not (previously x y)) || (outside x y && previously x y)
    | x <- [0..999], y <- [0..999] ]
  where
    inside x y = x >= x0 && x <= x1 && y >= y0 && y <= y1
    outside x y = not $ inside x y
    previously x y = is !! (y * 1000 + x)

main = do
  indata <- map (parse cmdParser "") . lines <$> readFile "06"
  let wdata = map (\(Right x) -> x) indata
      initalState = replicate (1000*1000) False
      results = foldl' execCmd initalState wdata
  print . length $ filter id results

