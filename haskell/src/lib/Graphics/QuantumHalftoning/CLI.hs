{-# LANGUAGE DataKinds, TypeApplications, LambdaCase, UnicodeSyntax #-}

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
import qualified Data.Vector.Storable as V

import qualified Codec.Picture as JP
import Codec.Picture hiding (Image(Image))

import System.Environment
import System.Exit
import Text.Read

--------------------------------------------------------------------------------

mainWith ∷ ℕ → FilePath → FilePath → IO ()
mainWith n infile outfile = do
  weightedCoin  ← maybe (die "Unknown expansion factor") pure
                    $ probability (n*n)
  probabilities ← either die (pure . expandWith @'Immutable n weightedCoin)
                    =<< runExceptT (readGrayscale infile)
  halftoned     ← build @'MutableIO probabilities
  halftonedData ← V.unsafeFreeze $ pixels halftoned
  writePng @Pixel8
           outfile
           JP.Image { imageWidth  = width halftoned
                    , imageHeight = height halftoned
                    , imageData   = halftonedData }

data CommandLineError = ParseError String
                      | UsageError
                      deriving (Eq, Ord, Show, Read)

parseArgs ∷ [String] → Either CommandLineError (ℕ, FilePath, FilePath)
parseArgs [nStr, infile, outfile] =
  let parse what = maybe (Left $ ParseError what) Right . readMaybe
  in (,,) <$> parse "expansion factor" nStr
          <*> pure                     infile
          <*> pure                     outfile
parseArgs _ =
  Left UsageError

main ∷ IO ()
main = uncurry3 mainWith =<< either (die <=< err) pure . parseArgs =<< getArgs
  where err (ParseError what) =
          pure $ "Could not parse " ++ what
        err UsageError        = do
          name ← getProgName
          pure $ "Usage: " ++ name ++ " N INFILE OUTFILE"
