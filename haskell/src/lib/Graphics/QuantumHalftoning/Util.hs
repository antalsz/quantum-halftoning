{-# LANGUAGE UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Util (
  𝔹, ℕ, ℝ, (<&>), curry3, uncurry3
) where

import Numeric.Natural

type 𝔹 = Bool
type ℕ = Natural
type ℝ = Double

(<&>) ∷ Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)
infixl 1 <&>
{-# INLINABLE (<&>) #-}

curry3 ∷ ((a,b,c) → d) → (a → b → c → d)
curry3 f = \a b c → f (a,b,c)
{-# INLINABLE curry3 #-}

uncurry3 ∷ (a → b → c → d) → ((a,b,c) → d)
uncurry3 f = \ ~(a,b,c) → f a b c
{-# INLINABLE uncurry3 #-}
