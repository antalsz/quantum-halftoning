{-# LANGUAGE KindSignatures, DataKinds, GADTs, TypeApplications,
             AllowAmbiguousTypes, ScopedTypeVariables, TypeFamilies,
             TypeOperators, FlexibleContexts, RecordWildCards, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Image (
  Image(..),
  GVector, GResult,
  at, set, randomIndex,
  expandWith,
  forCoordinates_,
  toLists,
  Mutability(..), SMutability(..), KnownMutability(..), Mutable(..)
) where

import Graphics.QuantumHalftoning.Util

import System.Random
import Data.Foldable
import Data.List.Split
import Data.Type.Equality

import Foreign.Storable
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.ST

--------------------------------------------------------------------------------

data Mutability k = Immutable | MutableST k | MutableIO
  deriving (Eq, Ord, Show, Read)

data SMutability (m ∷ Mutability ★) where
  SImmutable ∷ SMutability 'Immutable
  SMutableST ∷ SMutability ('MutableST s)
  SMutableIO ∷ SMutability 'MutableIO

class KnownMutability (m ∷ Mutability ★) where smutability ∷ SMutability m
instance KnownMutability 'Immutable      where smutability = SImmutable
instance KnownMutability ('MutableST s)  where smutability = SMutableST
instance KnownMutability 'MutableIO      where smutability = SMutableIO

class Monad (MutableMonad m) ⇒ Mutable (m ∷ Mutability ★) where
  type MutableMonad m ∷ ★ → ★
  type StateToken   m ∷ ★
  new     ∷ Storable a ⇒ Int → GResult m (GVector m a)
  doST    ∷ ST (StateToken m) a → GResult m a
  mvector ∷ GVector m a :~: MV.MVector (StateToken m) a
  mresult ∷ GResult m a :~: MutableMonad m a

instance Mutable ('MutableST s) where
  type MutableMonad ('MutableST s) = ST s
  type StateToken   ('MutableST s) = s
  new     = MV.new
  doST    = id
  mvector = Refl
  mresult = Refl
  {-# INLINABLE new #-}
  {-# INLINABLE doST #-}
  {-# INLINABLE mvector #-}
  {-# INLINABLE mresult #-}

instance Mutable 'MutableIO where
  type MutableMonad 'MutableIO = IO
  type StateToken   'MutableIO = RealWorld
  new     = MV.new
  doST    = stToIO
  mvector = Refl
  mresult = Refl
  {-# INLINABLE new #-}
  {-# INLINABLE doST #-}
  {-# INLINABLE mvector #-}
  {-# INLINABLE mresult #-}

--------------------------------------------------------------------------------

type family GVector (m ∷ Mutability ★) ∷ ★ → ★ where
  GVector 'Immutable     = V.Vector
  GVector ('MutableST s) = MV.STVector s
  GVector 'MutableIO     = MV.IOVector

type family GResult (m ∷ Mutability ★) (a ∷ ★) ∷ ★ where
  GResult 'Immutable     a = a
  GResult ('MutableST s) a = ST s a
  GResult 'MutableIO     a = IO a

--------------------------------------------------------------------------------

data Image (m ∷ Mutability ★) (a ∷ ★) =
  Image { width  ∷ !Int
        , height ∷ !Int
        , pixels ∷ !(GVector m a) }

internal_index ∷ Image m a → Int → Int → Int
internal_index Image{..} x y
  | x < width && y < height = x + y*width
  | otherwise               = error $  "Image index "   ++ show (x,y)
                                    ++ " out of range " ++ show (width,height)
{-# INLINABLE internal_index #-}

at ∷ forall m a. (KnownMutability m, Storable a)
   ⇒ Image m a → Int → Int → GResult m a
at img x y =
  let unsafeAt = case smutability @m of
                   SImmutable → V.unsafeIndex
                   SMutableST → MV.unsafeRead
                   SMutableIO → MV.unsafeRead
  in pixels img `unsafeAt` internal_index img x y
{-# INLINABLE at #-}

set ∷ forall m a. (Mutable m, Storable a)
    ⇒ Image m a → Int → Int → a → GResult m ()
set img x y a =
  doST @m $ MV.unsafeWrite (castWith (mvector @m) $ pixels img)
                           (internal_index img x y)
                           a
{-# INLINABLE set #-}

randomIndex ∷ Image m ℝ → IO (Int,Int)
randomIndex Image{..} =
  (,) <$> randomRIO (0, width-1) <*> randomRIO (0, height-1)

forCoordinates_ ∷ (Enum x, Num x, Enum y, Num y, Applicative f)
                ⇒ x → y → (x → y → f ()) → f ()
forCoordinates_ xSize ySize act =
  for_ [0..ySize-1] $ \y →
    for_ [0..xSize-1] $ \x →
      act x y
{-# INLINABLE forCoordinates_ #-}
{-# SPECIALIZE forCoordinates_ ∷
                 Int → Int → (Int → Int → ST s ()) → ST s () #-}
{-# SPECIALIZE forCoordinates_ ∷
                 Int → Int → (Int → Int → IO   ()) → IO   () #-}

expandWith ∷ forall m a. (KnownMutability m, Storable a)
           ⇒ ℕ → (a → a) → Image 'Immutable a
           → GResult m (Image m a)
expandWith nNat f img@Image{..} =
  let n | nNat > fromIntegral (maxBound ∷ Int) =
          error "expandWith: scale factor out of range"
        | otherwise =
          fromIntegral nNat
      
      width'  = n*width
      height' = n*height
      pixels' ∷ ST s (MV.STVector s a)
      pixels' = do pixels' ← MV.new $ width' * height'
                   forCoordinates_ width height $ \x y →
                     let value = f $ at img x y
                         dest' = n*x + n*y*width'
                     in forCoordinates_ n n $ \dx dy →
                          MV.write pixels' (dest' + dx + width'*dy) value
                   pure pixels'
      image' px = Image { width = width', height = height', pixels = px }
  in case smutability @m of
       SImmutable → image' $ V.create pixels'
       SMutableST → image' <$> pixels'
       SMutableIO → image' <$> stToIO pixels'

toLists ∷ forall m a. (KnownMutability m, Storable a)
        ⇒ Image m a → GResult m [[a]]
toLists Image{..} =
  let lists ∷ V.Vector a → [[a]]
      lists = chunksOf width . V.toList
  in case smutability @m of
       SImmutable → lists pixels
       SMutableST → lists <$> V.freeze pixels
       SMutableIO → lists <$> V.freeze pixels
