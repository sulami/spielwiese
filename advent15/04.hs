{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS

import           Data.Digest.Pure.MD5

build :: Int -> BS.ByteString
build n = BS.append "iwrupvqb" . BS.pack $ show n

test :: Int -> (a, BS.ByteString) -> Bool
test n (_,bs) = all (== '0') . take n . show $ md5 bs

run :: Int -> Int
run n = fst . head . filter (test n) . zip [0..] $ map build [0..]

main = do
  print $ run 4
  print $ run 5

