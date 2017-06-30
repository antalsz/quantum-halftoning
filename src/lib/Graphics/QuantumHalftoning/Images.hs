{-# LANGUAGE GADTs, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Images (
  GrayscaleImage(..), toGrayscaleImage, readGrayscaleImage
) where

import Graphics.QuantumHalftoning.Pixels
import Codec.Picture

--------------------------------------------------------------------------------

data GrayscaleImage where
  GrayscaleImage ∷ Grayscale g ⇒ Image g → GrayscaleImage

toGrayscaleImage ∷ DynamicImage → Either String GrayscaleImage
toGrayscaleImage (ImageY8  img) = Right $ GrayscaleImage img
toGrayscaleImage (ImageY16 img) = Right $ GrayscaleImage img
toGrayscaleImage (ImageYF  img) = Right $ GrayscaleImage img
toGrayscaleImage _              = Left "Non-grayscale image format"

readGrayscaleImage ∷ FilePath → IO (Either String GrayscaleImage)
readGrayscaleImage = fmap (toGrayscaleImage =<<) . readImage
