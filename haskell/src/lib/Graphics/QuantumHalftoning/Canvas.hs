{-# LANGUAGE RoleAnnotations, GADTs, RecordWildCards, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Canvas (
  -- * The 'Canvas' type
  Canvas(),
  width, height, probabilities, freezePixels, unsafeFreezePixels,
  -- * Indices
  Index(), index, getIndex,
  randomIndex,
  indices,
  -- * Construction
  FreshCanvas(..),
  buildCanvas,
  -- * Indexing
  probabilityAt, readPixel, writePixel, modifyPixel,
  -- * Refreshing pixels
  refreshPixel, refreshAllPixels
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Pixels

import Data.Coerce
import Data.Bifunctor
import Control.Monad
import Control.Monad.Random

import Codec.Picture (Image(..), Pixel(pixelBaseIndex, unsafePixelAt))

import           Foreign.Storable
import           Control.Monad.Primitive
import           Data.Vector.Storable (Vector, MVector)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV

--------------------------------------------------------------------------------

data Canvas c s px = Canvas { c_width         ∷ {-# UNPACK #-} !Int
                            , c_height        ∷ {-# UNPACK #-} !Int
                            , c_probabilities ∷ !(Vector ℝ)
                            , c_pixels        ∷ !(MVector s px) }
type role Canvas nominal nominal nominal
-- Invariant: c_width * c_height = V.length c_probabilities = V.length c_pixels

width ∷ Canvas c s px → Int
width = c_width
{-# INLINABLE width #-}

height ∷ Canvas c s px → Int
height = c_height
{-# INLINABLE height #-}

probabilities ∷ Canvas c s px → Vector ℝ
probabilities = c_probabilities
{-# INLINABLE probabilities #-}

freezePixels ∷ (PrimMonad m, Storable px)
             ⇒ Canvas c (PrimState m) px → m (Vector px)
freezePixels = V.freeze . c_pixels
{-# INLINABLE freezePixels #-}

unsafeFreezePixels ∷ (PrimMonad m, Storable px)
                   ⇒ Canvas c (PrimState m) px → m (Vector px)
unsafeFreezePixels = V.unsafeFreeze . c_pixels
{-# INLINABLE unsafeFreezePixels #-}

--------------------------------------------------------------------------------

newtype Index c = Index Int deriving (Eq, Ord, Show)
type role Index nominal
-- Invariant:
--     Canvas{..} ∷ Canvas c s px
--     Index i    ∷ Index  c
--   ------------------------------
--     0 ≤ i < c_width * c_height

index ∷ Integral i ⇒ Canvas c s px → (i,i) → Maybe (Index c)
index canvas (x',y')
  | inRange width x && inRange height y = Just . Index $ x + width canvas * y
  | otherwise                           = Nothing
  where inRange bound i = 0 <= i && i < bound canvas
        x = fromIntegral x'
        y = fromIntegral y'
{-# INLINABLE  index #-}
{-# SPECIALIZE index ∷ Canvas c s px → (Int,Int) → Maybe (Index c) #-}
{-# SPECIALIZE index ∷ Canvas c s px → (ℕ,ℕ)     → Maybe (Index c) #-}

getIndex ∷ Integral i ⇒ Canvas c s px → Index c → (i,i)
getIndex canvas (Index i) = join bimap fromIntegral $ i `quotRem` width canvas
{-# INLINABLE  getIndex #-}
{-# SPECIALIZE getIndex ∷ Canvas c s px → Index c → (Int,Int) #-}
{-# SPECIALIZE getIndex ∷ Canvas c s px → Index c → (ℕ,ℕ)     #-}

randomIndex ∷ MonadRandom m ⇒ Canvas c s px → m (Index c)
randomIndex canvas = do
  let randomIx bound = getRandomR (0, bound canvas - 1)
  x ← randomIx width
  y ← randomIx height
  pure . Index $ x + width canvas * y
{-# INLINABLE randomIndex #-}

indices ∷ Canvas c s px → [Index c]
indices canvas = coerce [0 .. V.length (probabilities canvas) - 1]
{-# INLINABLE indices #-}

--------------------------------------------------------------------------------

data FreshCanvas s px where
  Fresh ∷ !(Canvas c s px) → FreshCanvas s px

buildCanvas ∷ (PrimMonad m, Pixel ipx, Storable opx)
            ⇒ ℕ → (ipx → ℝ) → (ℝ → m opx) → Image ipx
            → Either String (m (FreshCanvas (PrimState m) opx))
buildCanvas nNat prob pixel img@Image{..} = do
  let toNat what x | x < 0     = Left  $ "Invalid negative image " ++ what
                   | otherwise = Right $ fromIntegral x
      
      toInt what x | x > fromIntegral (maxBound ∷ Int) =
                     Left $ what ++ show x ++ " is out of range"
                   | otherwise =
                     Right $ fromIntegral x
  
  widthNat  ← toNat "width"  imageWidth
  heightNat ← toNat "height" imageHeight
  
  n      ← toInt "Expansion factor"        nNat
  width  ← toInt "Expanded image width"  $ nNat*widthNat
  height ← toInt "Expanded image height" $ nNat*heightNat
  
  pure $ do
    let probabilities =
          V.generate (width * height) $ \oix →
            let (oy,ox) = oix `quotRem` width
                iix     = pixelBaseIndex img (ox `quot` n) (oy `quot` n)
            in prob $ imageData `unsafePixelAt` iix
    pixels ← V.unsafeThaw =<< V.mapM pixel probabilities
    pure $ Fresh Canvas { c_width         = width
                        , c_height        = height
                        , c_probabilities = probabilities
                        , c_pixels        = pixels }

--------------------------------------------------------------------------------

probabilityAt ∷ Canvas c s px → Index c → ℝ
probabilityAt canvas (Index i) = probabilities canvas `V.unsafeIndex` i
{-# INLINABLE probabilityAt #-}

readPixel ∷ (PrimMonad m, Storable px)
          ⇒ Canvas c (PrimState m) px → Index c → m px
readPixel canvas (Index i) = c_pixels canvas `MV.unsafeRead` i
{-# INLINABLE readPixel #-}

writePixel ∷ (PrimMonad m, Storable px)
           ⇒ Canvas c (PrimState m) px → Index c → px → m ()
writePixel canvas (Index i) px = MV.unsafeWrite (c_pixels canvas) i px
{-# INLINABLE writePixel #-}

modifyPixel ∷ (PrimMonad m, Storable px)
            ⇒ Canvas c (PrimState m) px → Index c → (px → px) → m ()
modifyPixel canvas (Index i) upd = MV.unsafeModify (c_pixels canvas) upd i
{-# INLINABLE modifyPixel #-}

refreshPixel ∷ (PrimMonad m, MonadRandom m, BlackAndWhite px, Storable px)
             ⇒ Canvas c (PrimState m) px → Index c → m ()
refreshPixel canvas i =
  writePixel canvas i =<< randomBWPixel (canvas `probabilityAt` i)
{-# INLINABLE refreshPixel #-}

refreshAllPixels ∷ (MonadRandom m, PrimMonad m, BlackAndWhite px, Storable px)
                 ⇒ Canvas c (PrimState m) px → m ()
refreshAllPixels canvas = mapM_ (refreshPixel canvas) (indices canvas)
{-# LANGUAGE refreshAllPixels #-}
