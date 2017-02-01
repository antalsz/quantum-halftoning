{-# LANGUAGE UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Util (
  ℕ, ℝ, (<&>), interspersedTabulateM_
) where

import Numeric.Natural

type ℕ = Natural
type ℝ = Double

(<&>) ∷ Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)
infixl 1 <&>
{-# INLINABLE (<&>) #-}

interspersedTabulateM_ ∷ Applicative f ⇒ ℕ → f () → (ℕ → f ()) → f ()
interspersedTabulateM_ n sep act = go 1 where
  go i | i == n    = act i
       | i <  n    = act i *> sep *> go (i+1)
       | otherwise = pure ()
