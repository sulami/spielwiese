module Main where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.List             (minimumBy, sortBy)
import           Data.Ord              (comparing)

import           Matasano

keySizeScore :: [ByteString] -> Float
keySizeScore [a,b] = fromIntegral (hammingDistance a b) /
                      fromIntegral (BS.length a)

main = do
  indata <- C8.pack <$> readFile "6.txt"
  let keySizes = map (keySizeScore . take 2 . (`chunks` indata)) [2..40]
      ksScores = take 5 . map fst . sortBy (comparing snd) $ zip [2..] keySizes
      brokenUp = map (C8.transpose . (`chunks` indata)) ksScores
      options enc = [ replicate (BS.length enc) x | x <- [65..90] ]
      test enc = (\w -> (score w,w)) . unsafeFromHex . xorByteString (Hex enc) .
              toHex . BS.pack
  print . minimumBy (comparing fst) $ map test options
  print brokenUp

