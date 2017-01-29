{-# LANGUAGE RecordWildCards, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Image (
  Image(..),
  at, randomIndex,
  expandWith,
  forCoordinates_,
  toLists
) where

import Graphics.QuantumHalftoning.Util

import System.Random
import Data.Foldable
import Data.List.Split

import Foreign.Storable
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.ST (ST()) -- For SPECIALIZE

--------------------------------------------------------------------------------

data Image a = Image { width  ∷ !Int
                     , height ∷ !Int
                     , pixels ∷ !(V.Vector a) }
             deriving (Eq, Ord, Show, Read)

at ∷ Storable a ⇒ Image a → Int → Int → a
at Image{..} x y
  | x < width && y < height = pixels `V.unsafeIndex` (x + y*width)
  | otherwise               = error $  "Image index "   ++ show (x,y)
                                    ++ " out of range " ++ show (width,height)
{-# INLINABLE at #-}

randomIndex ∷ Image ℝ → IO (Int,Int)
randomIndex Image{..} = (,) <$> randomRIO (0, width-1) <*> randomRIO (0, height-1)

forCoordinates_ ∷ (Enum x, Num x, Enum y, Num y, Applicative f)
                ⇒ x → y → (x → y → f ()) → f ()
forCoordinates_ xSize ySize act =
  for_ [0..ySize-1] $ \y →
    for_ [0..xSize-1] $ \x →
      act x y
{-# INLINABLE forCoordinates_ #-}
{-# SPECIALIZE forCoordinates_ ∷ Int → Int → (Int → Int → ST s ()) → ST s () #-}
{-# SPECIALIZE forCoordinates_ ∷ Int → Int → (Int → Int → IO   ()) → IO   () #-}

expandWith ∷ Storable a ⇒ ℕ → (a → a) → Image a → Image a
expandWith nNat f img@Image{..} =
  let n | nNat > fromIntegral (maxBound ∷ Int) = error "expandWith: scale factor out of range"
        | otherwise                            = fromIntegral nNat
      
      width'  = n*width
      height' = n*height
      pixels' = V.create $ do
                  pixels' ← MV.new $ width' * height'
                  forCoordinates_ width height $ \x y →
                    let value = f $ at img x y
                        dest' = n*x + n*y*width'
                    in forCoordinates_ n n $ \dx dy →
                         MV.write pixels' (dest' + dx + width'*dy) value
                  pure pixels'
  in Image { width = width', height = height', pixels = pixels' }

toLists ∷ Storable a ⇒ Image a → [[a]]
toLists Image{..} = chunksOf width $ V.toList pixels
