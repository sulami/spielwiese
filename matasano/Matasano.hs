{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides a lot of utility for use in the matasano crypto
-- challenges. One of the main concerns is proper type-safe handling of
-- hex-strings.

module Matasano where

import           Data.Bits (xor)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Hex (hex, unhex)

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

-- | 'xor' two hex encoded bytestrings. Caution: Assumes valid input data
xorByteString :: HexByteString -> HexByteString -> HexByteString
xorByteString a b = toHex . BS.pack $
  BS.zipWith xor (unsafeFromHex a) (unsafeFromHex b)

