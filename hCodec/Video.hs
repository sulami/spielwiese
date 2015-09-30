-- | This module implements a mpeg-4-like video codec that uses incremental
-- frame differences and iframes.

module Video (
) where

import           Data.List (unfoldr)

-- | The datatype used for single colour channels. Possibly subject to changes.
-- Colours are expected to be between 0 and 255.
type Channel = Int

-- | A simple RGB colour.
newtype Colour = Colour (Channel, Channel, Channel)
  deriving (Eq, Show)

-- | Average colours.
avgColour :: [Colour] -> Colour
avgColour [] = error "Empty list"
avgColour cs = r (length cs) $ foldr1 a cs
  where
    a :: Colour -> Colour -> Colour
    a (Colour (r0,g0,b0)) (Colour (r1,g1,b1)) = Colour (r0+r1,g0+g1,b0+b1)

    r :: Int -> Colour -> Colour
    r l (Colour (rc,gc,bc)) = Colour (rc `div` l, gc `div` l, bc `div` l)

type Pixel = Colour

-- | A chunk that is several pixels big and has a single colour.
data Chunk = Chunk
  { chunkWidth  :: Int
  , chunkHeight :: Int
  , chunkColour :: Colour
  }

instance Show Chunk where
  show (Chunk w h c) = "Chunk " ++ show w ++ "x" ++ show h ++ ": " ++ show c

-- | A single frame, composed of Pixels.
data PixelFrame = PixelFrame
  { pframeWidth  :: Int
  , pframeHeight :: Int
  , pframePixels :: [Colour]
  }

instance Show PixelFrame where
  show (PixelFrame w h _) = "PixelFrame " ++ show w ++ " " ++ show h

-- | A single frame, composed of Chunks.
data ChunkFrame = ChunkFrame
  { cframeWidth  :: Int
  , cframeHeight :: Int
  , cframeChunks :: [Chunk]
  }

instance Show ChunkFrame where
  show (ChunkFrame w h _) = "ChunkFrame " ++ show w ++ " " ++ show h

-- | Get the size of the chunks in a frame. Fails if used with a PixelFrame or
-- an empty ChunkFrame.
frameChunkSize :: ChunkFrame -> (Int, Int)
frameChunkSize (ChunkFrame _ _ []   ) = error "Empty frame"
frameChunkSize (ChunkFrame _ _ (c:_)) = (chunkWidth c, chunkHeight c)

-- | Convert a PixelFrame to a ChunkFrame. This needs the desired chunksize.
frameToChunks :: (Int, Int) -> PixelFrame -> ChunkFrame
frameToChunks (cw,ch) (PixelFrame w h ps) = ChunkFrame w h $ toChunks ps
  where
    toChunks :: [Pixel] -> [Chunk]
    -- FIXME set the dimensions properly for the bottom/right border.
    toChunks = map (Chunk cw ch . avgColour) . chunks

    chunks :: [a] -> [[a]]
    chunks = chunks' . unfoldr toRows

    toRows :: [a] -> Maybe ([a], [a])
    toRows [] = Nothing
    toRows l  = Just $ splitAt w l

    chunks' :: [[a]] -> [[a]]
    chunks' [] = []
    chunks' rs = chunks'' (take ch rs) ++ chunks' (drop ch rs)

    chunks'' :: [[a]] -> [[a]]
    chunks'' ([]:_) = []
    chunks'' rs = concatMap (take cw) rs : chunks'' (map (drop cw) rs)

-- | Convert a ChunkFrame to a PixelFrame.
frameToPixels :: ChunkFrame -> PixelFrame
frameToPixels = undefined

