{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Matasano

main = do
  indata <- map C8.pack . lines <$> readFile "4.txt"
  let options = [ replicate (BS.length $ head indata) x | x <- [3..126] ]
      test d = map ((\w -> (score w,w)) . unsafeFromHex . xorByteString (Hex d) .
               toHex . BS.pack) options
  mapM_ print . take 5 . sortBy (comparing fst) $ concatMap test indata

