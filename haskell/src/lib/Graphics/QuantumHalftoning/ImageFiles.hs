{-# LANGUAGE DataKinds, TypeSynonymInstances, TypeFamilies,
             RecordWildCards, LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.ImageFiles (
  ToGrayscale(..),
  grayscaleImage, readGrayscale,
  wordGrayscale, plainGrayscaleVector
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Image

import Control.Monad.Except
import qualified Data.Vector.Storable as V
import GHC.Float

import Codec.Picture hiding (Image(..))
import qualified Codec.Picture as JP
import Codec.Picture.Types hiding (Image(..))
import Codec.Picture.Metadata

--------------------------------------------------------------------------------

class Pixel a ⇒ ToGrayscale a where
  grayscale       ∷ a → ℝ
  grayscaleVector ∷ Metadatas → JP.Image a → V.Vector ℝ

instance ToGrayscale Pixel8 where
  grayscale       = wordGrayscale
  grayscaleVector = plainGrayscaleVector

instance ToGrayscale Pixel16 where
  grayscale       = wordGrayscale
  grayscaleVector = plainGrayscaleVector

instance ToGrayscale Pixel32 where
  grayscale       = wordGrayscale
  grayscaleVector = plainGrayscaleVector

instance ToGrayscale PixelF where
  grayscale       = float2Double
  grayscaleVector = plainGrayscaleVector

wordGrayscale ∷ (Integral g, Bounded g) ⇒ g → ℝ
wordGrayscale g = fromIntegral g / fromIntegral (maxBound `asTypeOf` g)
{-# INLINABLE wordGrayscale #-}

plainGrayscaleVector ∷ (ToGrayscale a, a ~ PixelBaseComponent a)
                     ⇒ Metadatas → JP.Image a → V.Vector ℝ
plainGrayscaleVector _ JP.Image{..} = V.map grayscale imageData
{-# INLINABLE plainGrayscaleVector #-}

grayscaleImage ∷ ToGrayscale a ⇒ Metadatas → JP.Image a → Image 'Immutable ℝ
grayscaleImage md img@JP.Image{..} = Image { width  = imageWidth
                                           , height = imageHeight
                                           , pixels = grayscaleVector md img }

readGrayscale ∷ FilePath → ExceptT String IO (Image 'Immutable ℝ)
readGrayscale file = do
  (dimg, md) ← ExceptT $ readImageWithMetadata file
  case dimg of
    ImageY8  img → pure $ grayscaleImage md img
    ImageY16 img → pure $ grayscaleImage md img
    ImageYF  img → pure $ grayscaleImage md img
    _            → throwError "Non-grayscale image format"
