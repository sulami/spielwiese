{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)

import qualified Data.ByteString as BS

import Matasano

main = do
  let enc = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      options = [ replicate (BS.length enc) x | x <- [65..90] ]
      test = (\w -> (score w,w)) . unsafeFromHex . xorByteString (Hex enc) .
              toHex . BS.pack
  print . minimumBy (comparing fst) $ map test options

