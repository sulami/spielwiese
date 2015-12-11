{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

incChar :: Bool -> Word8 -> (Bool, Word8)
incChar False c   = (False, c)
incChar True  122 = (True, 97)     -- z -> a
incChar True  c   = (False, c + 1)

incPasswd :: ByteString -> ByteString
incPasswd = snd . BS.mapAccumR incChar True

check :: ByteString -> Bool
check pw = any (`BS.isInfixOf` pw) [ BS.pack [c,c+1,c+2] | c <- [97..120] ]
        && not (any (`BS.elem` pw) [105,111,108])
        && 2 <= length (filter (`BS.isInfixOf` pw)
           [ BS.pack [c,c] | c <- [97..122] ])

main = do
  let passwords = filter check $ iterate incPasswd "vzbxkghb"
  print $ head passwords
  print $ passwords !! 1

