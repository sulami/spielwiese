-- | This module implements a mpeg-4-like video codec that uses incremental
-- frame differences and iframes.

module Video (
) where

-- | A simple RGB colour.
newtype Colour = Colour (Int, Int, Int)
  deriving (Eq, Show)

type Pixel = Colour

-- | A chunk that is several pixels big and has a single colour.
data Chunk = Chunk
  { chunkWidth  :: Int
  , chunkHeight :: Int
  , chunkColour :: Colour
  }

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
    toChunks = undefined

-- | Convert a ChunkFrame to a PixelFrame.
frameToPixels :: ChunkFrame -> PixelFrame
frameToPixels = undefined

