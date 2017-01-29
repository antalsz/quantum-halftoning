{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.CLI (
  mainWith, main,
  CommandLineError(..), parseArgs) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Image
import Graphics.QuantumHalftoning.Mutable
import Graphics.QuantumHalftoning.MutualInformation
import Graphics.QuantumHalftoning.ImageFiles

import Control.Monad
import Control.Monad.Except
import Control.Concurrent
import qualified Data.Vector.Storable.Mutable as MV

import Graphics.Gloss

import System.Environment
import System.Exit
import Text.Read

--------------------------------------------------------------------------------

mainWith ∷ ℕ → Int → FilePath → IO ()
mainWith n freq file = do
  weightedCoin  ← maybe (die "Unknown expansion factor") pure $ probability (n*n)
  probabilities ← either die (pure . expandWith n weightedCoin)
                    =<< runExceptT (readGrayscale file)
  bitVector     ← build probabilities
  let bitmap = bitmapOfForeignPtr (width probabilities) (height probabilities)
                                  (BitmapFormat TopToBottom PxRGBA)
                                  (fst $ MV.unsafeToForeignPtr0 bitVector)
                                  False
  
  void . forkIO . forever $ do
    threadDelay   freq
    refreshRandom probabilities bitVector
  
  animate (InWindow "Quantum Halftoning"
                    (width probabilities, height probabilities)
                    (100,100))
          (greyN 0.5)
          (const bitmap)

data CommandLineError = ParseError String
                      | UsageError
                      deriving (Eq, Ord, Show, Read)

parseArgs ∷ [String] → Either CommandLineError (ℕ, Int, FilePath)
parseArgs [nStr, freqStr, file] =
  let parse what = maybe (Left $ ParseError what) Right . readMaybe
  in (,,) <$> parse "expansion factor" nStr
          <*> parse "refresh rate"     freqStr
          <*> pure                     file
parseArgs _ =
  Left UsageError

main ∷ IO ()
main = uncurry3 mainWith =<< either (die <=< err) pure . parseArgs =<< getArgs
  where err (ParseError what) =
          pure $ "Could not parse " ++ what
        err UsageError        = do
          name ← getProgName
          pure $ "Usage: " ++ name ++ " N FREQ FILE"
