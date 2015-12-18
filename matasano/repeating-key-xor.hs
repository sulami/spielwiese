{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Matasano

main = do
  let src = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
      key = toHex . C8.pack . take (length src) $ cycle "ICE"
  print . xorByteString key . toHex $ C8.pack src

