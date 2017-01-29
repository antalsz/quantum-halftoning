{-# LANGUAGE RecordWildCards, LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Mutable (
  build, refresh, randomIndex, refreshRandom
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Image
import Graphics.QuantumHalftoning.Random

import Data.Word
import qualified Data.Vector.Storable.Mutable as MV

--------------------------------------------------------------------------------

build ∷ Image ℝ → IO (MV.IOVector Word8)
build img@Image{..} = do
  vec ← MV.new $ 4*width*height
  forCoordinates_ width height $ refresh img vec
  pure vec

refresh ∷ Image ℝ → MV.IOVector Word8 → Int → Int → IO ()
refresh img vec x y = do
  let index = x + width img * y
  value ← coin (at img x y) <&> \case
             True  → 255
             False → 0
  MV.write vec (4*index + 0) value
  MV.write vec (4*index + 1) value
  MV.write vec (4*index + 2) value
  MV.write vec (4*index + 3) 255

refreshRandom ∷ Image ℝ → MV.IOVector Word8 → IO ()
refreshRandom img vec = uncurry (refresh img vec) =<< randomIndex img
