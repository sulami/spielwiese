#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.Maybe (fromMaybe)
import           Prelude    hiding (EQ, GT, LT)

data Comparison = LT | LTE | EQ | NE | GTE | GT
  deriving Show

readComparison :: String -> Comparison
readComparison s = case s of
  "<"  -> LT
  "<=" -> LTE
  "==" -> EQ
  "!=" -> NE
  ">=" -> GTE
  ">"  -> GT
  _    -> error "no comparison parse"

data Action = IncrAction
              { aRegister :: String
              , aAmount   :: Int
              } | DecrAction
              { aRegister :: String
              , aAmount   :: Int
              } deriving Show

readAction :: String -> Action
readAction s = let [reg, act, amo] = words s
               in case act of
                    "inc" -> IncrAction reg $ read amo
                    "dec" -> DecrAction reg $ read amo
                    _     -> error "no action parse"

data Condition = Condition
                 { cRegister   :: String
                 , cComparison :: Comparison
                 , cAmount     :: Int
                 } deriving Show

readCondition :: String -> Condition
readCondition s = let [reg, comp, amo] = words s
                  in Condition reg (readComparison comp) $ read amo

data Instruction = Instruction
                   { iAction    :: Action
                   , iCondition :: Condition
                   } deriving Show

readInstruction :: String -> Instruction
readInstruction s = let (act, cond) = splitAt 3 $ words s
                    in Instruction
                       (readAction $ unwords act)
                       (readCondition . unwords $ tail cond)

type State = [(String, Int)]

main :: IO ()
main = do
  input <- map readInstruction . lines <$> getContents
  let steps = scanr executeInstruction [] $ reverse input
      final = head steps
  print . maximum $ map snd final
  print . maximum . map snd $ concat steps

executeInstruction :: Instruction -> State -> State
executeInstruction instr s0
  | checkCondition (iCondition instr) s0 = applyAction (iAction instr) s0
  |                            otherwise = s0

checkCondition :: Condition -> State -> Bool
checkCondition cond s0 = let val = lookupOrZero (cRegister cond) s0
                             amo = cAmount cond
                         in case cComparison cond of
                              LT  -> val < amo
                              LTE -> val <= amo
                              EQ  -> val == amo
                              NE  -> val /= amo
                              GTE -> val >= amo
                              GT  -> val > amo

applyAction :: Action -> State -> State
applyAction (IncrAction reg amo) s0 = let val = lookupOrZero reg s0
                                      in modifyState s0 reg $ val + amo
applyAction (DecrAction reg amo) s0 = let val = lookupOrZero reg s0
                                      in modifyState s0 reg $ val - amo

lookupOrZero :: String -> State -> Int
lookupOrZero k = fromMaybe 0 . lookup k

modifyState :: State -> String -> Int -> State
modifyState s0 reg val = (reg, val) : filter ((/= reg) . fst) s0
