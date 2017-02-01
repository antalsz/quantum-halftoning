{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.ProbabilityDistributions (
  maximizingMutualInformation
) where

import Graphics.QuantumHalftoning.Util

maximizingMutualInformation ∷ ℕ → Maybe (ℝ → ℝ)

maximizingMutualInformation 1  = Just $
  \case x | x < 1/2   → 1
          | otherwise → 0

maximizingMutualInformation 2  = Just $
  \case x | x < 0.4411763 → 1.0000000
          | x < 0.5588237 → 0.5000000
          | otherwise     → 0.0000000

maximizingMutualInformation 4  = Just $
  \case x | x < 0.3700282 → 1.0000000
          | x < 0.6299718 → 0.5000000
          | otherwise     → 0.0000000

maximizingMutualInformation 9  = Just $
  \case x | x < 0.2887258 → 1.0000000
          | x < 0.4735139 → 0.7463387
          | x < 0.5264861 → 0.5000000
          | x < 0.7112742 → 0.2536613
          | otherwise     → 0.0000000

maximizingMutualInformation 16 = Just $
  \case x | x < 0.2343332 → 1.0000000
          | x < 0.3820798 → 0.8553174
          | x < 0.5000000 → 0.6187429
          | x < 0.6179202 → 0.3812571
          | x < 0.7656668 → 0.1446826
          | otherwise     → 0.0000000

maximizingMutualInformation 25 = Just $
  \case x | x < 0.1975705 → 1.0000000
          | x < 0.3237746 → 0.9047354
          | x < 0.4289575 → 0.7332138
          | x < 0.5000000 → 0.5617674
          | x < 0.5710425 → 0.4382326
          | x < 0.6762254 → 0.2667862
          | x < 0.8024295 → 0.0952646
          | otherwise     → 0.0000000

maximizingMutualInformation 36 = Just $
  \case x | x < 0.1706287 → 1.0000000
          | x < 0.2791773 → 0.9333200
          | x < 0.3720039 → 0.8088037
          | x < 0.4580385 → 0.6593351
          | x < 0.5419615 → 0.5000000
          | x < 0.6279961 → 0.3406649
          | x < 0.7208227 → 0.1911963
          | x < 0.8293713 → 0.0666800
          | otherwise     → 0.0000000

maximizingMutualInformation _  = Nothing
