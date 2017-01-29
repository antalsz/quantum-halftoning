{-# LANGUAGE UnicodeSyntax #-}

module Graphics.QuantumHalftoning.Random (coin) where

import Graphics.QuantumHalftoning.Util
import System.Random

coin ∷ ℝ → IO 𝔹
coin p = (< p) <$> randomIO
