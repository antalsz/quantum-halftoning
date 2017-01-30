{-# LANGUAGE DataKinds, TypeApplications, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeFamilies, TypeSynonymInstances,
             DefaultSignatures, RecordWildCards, LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Mutable (
  build, refresh, refreshRandom, BlackAndWhite(..)
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Image
import Graphics.QuantumHalftoning.Random

import Data.Type.Equality
import Foreign.Storable

import Codec.Picture.Types hiding (Image(..))

--------------------------------------------------------------------------------

class BlackAndWhite bw where
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

build ∷ forall m bw.
        (Mutable m, MutableMonad m ~ IO, BlackAndWhite bw, Storable bw)
      ⇒ Image 'Immutable ℝ → GResult m (Image m bw)
build probs@Image{..} = castWith (sym $ mresult @m @(Image m bw)) $ do
  pixels' ← castWith (mresult @m @(GVector m bw)) . new @m @bw $ width*height
  let bw = probs{pixels=pixels'}
  forCoordinates_ width height $ \x y →
    castWith (mresult @m @()) $ refresh probs bw x y
  pure bw

refresh ∷ forall m bw.
          (Mutable m, MutableMonad m ~ IO, BlackAndWhite bw, Storable bw)
        ⇒ Image 'Immutable ℝ → Image m bw → Int → Int → GResult m ()
refresh probs bw x y = castWith (sym $ mresult @m @()) $ do
  value ← coin (at probs x y) <&> \case
    True  → white
    False → black
  castWith (mresult @m @()) $ set bw x y value

refreshRandom ∷ forall m bw.
                (Mutable m, MutableMonad m ~ IO, BlackAndWhite bw, Storable bw)
              ⇒ Image 'Immutable ℝ → Image m bw → GResult m ()
refreshRandom probs bw =
  castWith (sym $ mresult @m @()) $ castWith (mresult @m @()) .
  uncurry (refresh probs bw) =<< randomIndex probs
