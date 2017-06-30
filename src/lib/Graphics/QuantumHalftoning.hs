{-# LANGUAGE RankNTypes, RecordWildCards, UnicodeSyntax #-}

module Graphics.QuantumHalftoning (
  -- * Types
  Parameters(..), StartingCanvas(..), RefreshStyle(..),
  -- * Quantum halftoning
  quantumHalftone,
  -- * Pixel-manipulation functions
  startingPixel, refresh
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Pixels
import Graphics.QuantumHalftoning.Canvas
import Graphics.QuantumHalftoning.ProbabilityDistributions

import Control.Monad.Primitive
import Control.Monad.Random

import Foreign.Storable
import qualified Codec.Picture as JP

--------------------------------------------------------------------------------

data StartingCanvas = Image
                    | AllWhite
                    | AllBlack
                    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data RefreshStyle = RefreshPixel
                  | RefreshImage
                  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Parameters = Parameters { expansionFactor   ∷ !ℕ
                             , startingCanvas    ∷ !StartingCanvas
                             , refreshStyle      ∷ !RefreshStyle
                             , imageCount        ∷ !ℕ }
                deriving (Eq, Ord, Show, Read)

startingPixel ∷ (MonadRandom m, BlackAndWhite px) ⇒ StartingCanvas → ℝ → m px
startingPixel Image    = randomBWPixel
startingPixel AllWhite = const $ pure white
startingPixel AllBlack = const $ pure black

refresh ∷ (PrimMonad m, MonadRandom m, BlackAndWhite px, Storable px)
        ⇒ RefreshStyle → Canvas c (PrimState m) px → m ()
refresh RefreshPixel = \canvas → refreshPixel canvas =<< randomIndex canvas
refresh RefreshImage = refreshAllPixels

quantumHalftone ∷ ( PrimMonad m, MonadRandom m
                  , Grayscale ipx, BlackAndWhite opx, Storable opx )
                ⇒ Parameters
                → JP.Image ipx
                → (∀c. ℕ → Canvas c (PrimState m) opx → m ())
                → Either String (m ())
quantumHalftone Parameters{..} image consume = do
  probability ← maybe (Left $  "Unknown expansion factor "
                            ++ show expansionFactor)
                      Right
                  $ maximizingMutualInformation (expansionFactor^(2∷ℕ))
  makeCanvas  ← buildCanvas expansionFactor
                            (probability . grayscale)
                            (startingPixel startingCanvas)
                            image
  pure $ do
    Fresh canvas ← makeCanvas
    interspersedTabulateM_ imageCount (refresh refreshStyle canvas) $ \i →
      consume i canvas
