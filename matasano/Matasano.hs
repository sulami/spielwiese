{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a lot of utility for use in the matasano crypto
-- challenges. One of the main concerns is proper type-safe handling of
-- hex-strings.

module Matasano where

import           Control.Arrow ((&&&), second)
import           Data.Bits (xor)
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Hex (hex, unhex)
import           Data.Word (Word8)

-- | Describes a lazy 'ByteString' that is valid hex encoded data
newtype HexByteString = Hex ByteString
  deriving (Eq, Show, IsString)

-- | Encode to hex
toHex :: ByteString -> HexByteString
toHex = Hex . hex

-- | Decode from hex. Can fail if the data is invalid (e.g. contains characters
-- outside of [0-9a-fA-F]
fromHex :: HexByteString -> Maybe ByteString
fromHex (Hex bs) = unhex bs

-- | Decode from hex. Has a more convinient type if you know your data is valid
-- hex but will 'error' if it is not
unsafeFromHex :: HexByteString -> ByteString
unsafeFromHex (Hex bs) = fromMaybe (error "Failed to unhex") $ unhex bs

-- | Check if a hex encoded bytestring is valid
checkHex :: HexByteString -> Bool
checkHex = isJust . fromHex

-- | 'xor' two hex encoded bytestrings. Caution: Assumes valid input data
xorByteString :: HexByteString -> HexByteString -> HexByteString
xorByteString a b = toHex . BS.pack $
  BS.zipWith xor (unsafeFromHex a) (unsafeFromHex b)

-- | Score a string based on letter-frequency compared to average English.
-- Lower scores are better, and generally English should have a score below 0.6
score :: ByteString -> Float
score s = let countLetters = fromIntegral . (`BS.count` cleanedInput)
              occurences = map (id &&& countLetters) [97..122]
          in sum (map compare occurences) / floatLength
  where
    -- Remove all characters we do not account for
    cleanedInput :: ByteString
    cleanedInput = BS.filter (`elem` [97..122]) s

    -- We need this in a couple of places
    floatLength :: Float
    floatLength = fromIntegral $ BS.length cleanedInput

    -- The model distribution of characters in the English language, based off
    -- an analysis of 45406 common words
    expected :: [(Word8, Float)]
    expected = [ (101,42689), (105,31450), (115,29639), ( 97,28965),
                 (114,27045), (110,26975), (116,24599), (111,21588),
                 (108,19471), ( 99,15002), (100,13849), (117,11715),
                 (103,10339), (112,10063), (109,9803),  (104,7808),
                 ( 98,7368),  (121,6005),  (102,4926),  (118,3971),
                 (107,3209),  (119,3073),  (122,1631),  (120,1053),
                 (106,727),   (113,682) ]

    -- The model distribution scaled down to the length of the input
    relativeBaseline :: [(Word8, Float)]
    relativeBaseline = map (second (floatLength / 363645 *)) expected

    -- Compare the observed character frequency with the on we would expect in
    -- English text
    compare :: (Word8, Float) -> Float
    compare (w,n) = abs . (n -) . fromMaybe 0 $ lookup w relativeBaseline

