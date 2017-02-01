{-# LANGUAGE DefaultSignatures, TypeSynonymInstances, ScopedTypeVariables,
             LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Pixels (
  BlackAndWhite(..), Grayscale(..),
  randomBWPixel,
) where

import Graphics.QuantumHalftoning.Util
import Control.Monad.Random
import GHC.Float
import Codec.Picture.Types

--------------------------------------------------------------------------------

class Pixel bw ⇒ BlackAndWhite bw where
  black, white ∷ bw
  default black ∷ Bounded bw ⇒ bw
  default white ∷ Bounded bw ⇒ bw
  black = minBound
  white = maxBound

instance BlackAndWhite Pixel8
instance BlackAndWhite Pixel16
instance BlackAndWhite Pixel32
instance BlackAndWhite PixelF where
  black = 0
  white = 1

class BlackAndWhite gray ⇒ Grayscale gray where
  grayscale ∷ gray → ℝ
  default grayscale ∷ (Integral gray, Bounded gray) ⇒ gray → ℝ
  grayscale g = fromIntegral g / fromIntegral (maxBound ∷ gray)

instance Grayscale Pixel8
instance Grayscale Pixel16
instance Grayscale Pixel32
instance Grayscale PixelF where
  grayscale = float2Double

randomBWPixel ∷ (MonadRandom m, BlackAndWhite bw) ⇒ ℝ → m bw
randomBWPixel p = getRandom <&>
                    \case x | x < p     → black
                            | otherwise → white
