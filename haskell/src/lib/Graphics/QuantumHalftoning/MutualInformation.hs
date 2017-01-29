{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.MutualInformation (probability) where

import Graphics.QuantumHalftoning.Util

--------------------------------------------------------------------------------

probability ∷ ℕ → Maybe (ℝ → ℝ)
probability 1  = Just $
  \case x | x < 1/2   → 0
          | otherwise → 1

probability 2  = Just $
  \case x | x < 0.4411763 → 0.0000000
          | x < 0.5588237 → 0.5000000
          | otherwise     → 1.0000000

probability 4  = Just $
  \case x | x < 0.3700282 → 0.0000000
          | x < 0.6299718 → 0.5000000
          | otherwise     → 1.0000000

probability 9  = Just $
  \case x | x < 0.2887258 → 0.0000000
          | x < 0.4735139 → 0.2536613
          | x < 0.5264861 → 0.5000000
          | x < 0.7112742 → 0.7463387
          | otherwise     → 1.0000000

probability 16 = Just $
  \case x | x < 0.2343332 → 0.0000000
          | x < 0.3820798 → 0.1446826
          | x < 0.5000000 → 0.3812571
          | x < 0.6179202 → 0.6187429
          | x < 0.7656668 → 0.8553174
          | otherwise     → 1.0000000

probability 25 = Just $
  \case x | x < 0.1975705 → 0.0000000
          | x < 0.3237746 → 0.0952646
          | x < 0.4289575 → 0.2667862
          | x < 0.5000000 → 0.4382326
          | x < 0.5710425 → 0.5617674
          | x < 0.6762254 → 0.7332138
          | x < 0.8024295 → 0.9047354
          | otherwise     → 1.0000000

probability 36 = Just $
  \case x | x < 0.1706287 → 0.0000000
          | x < 0.2791773 → 0.0666800
          | x < 0.3720039 → 0.1911963
          | x < 0.4580385 → 0.3406649
          | x < 0.5419615 → 0.5000000
          | x < 0.6279961 → 0.6593351
          | x < 0.7208227 → 0.8088037
          | x < 0.8293713 → 0.9333200
          | otherwise     → 1.0000000

probability _  = Nothing
