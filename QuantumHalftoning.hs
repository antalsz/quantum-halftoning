{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts,
             TypeSynonymInstances, DefaultSignatures,
             GeneralizedNewtypeDeriving,
             DeriveFunctor, DeriveFoldable, DeriveTraversable,
             UnicodeSyntax #-}

import Data.Coerce
import Data.List
import Data.List.Split
import Numeric.Natural
import GHC.Float
import Control.Monad
import System.Random

--------------------------------------------------------------------------------

type 𝔹 = Bool
type ℕ = Natural
type ℝ = Double

genericReplicateM ∷ (Integral i, Monad m) ⇒ i → m a → m [a]
genericReplicateM = replicateM . fromIntegral

--------------------------------------------------------------------------------

newtype Image a = Image { getImage ∷ [[a]] }
                deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

expand ∷ ℕ → a → Image a
expand n x = Image $ genericReplicate n (genericReplicate n x)

coalesce ∷ forall a. Image (Image a) → Image a
coalesce = coerce (concatMap $ fmap concat . transpose ∷ [[ [[a]] ]] → [[a]])

view ∷ Image Bool → String
view = unlines . getImage . fmap (\x → if x then '#' else ' ')

--------------------------------------------------------------------------------

newtype Q a = Q { runQ ∷ IO a }
            deriving (Functor, Applicative, Monad)

coin ∷ ℝ → Q 𝔹
coin p = Q $ (< p) <$> randomIO

--------------------------------------------------------------------------------

probability ∷ ℕ → ℝ → ℝ
probability 1 x | x < 1/2   = 0
                | otherwise = 1
probability 2 x | x < 15/34 = 0
                | x < 19/34 = 0.5
                | otherwise = 1

noise ∷ ℝ → ℕ → Q (Image 𝔹)
noise p n = Image <$> genericReplicateM n (genericReplicateM n $ coin p)

qhalftone ∷ ℕ → Image ℝ → Q (Image 𝔹)
qhalftone n = fmap coalesce . traverse (\p → noise (probability (n*n) p) n)
