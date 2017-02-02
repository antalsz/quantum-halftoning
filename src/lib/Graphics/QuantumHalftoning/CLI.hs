{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications, RecordWildCards, LambdaCase, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.CLI (
  -- * Running the quantum halftoning algorithm
  Options(..),
  mainWith, main,
  -- * Parsing command-line options
  optionsInfo, optionsParser, parametersParser,
  -- ** Option readers
  positive, readStartingCanvas, readRefreshStyle, readFilePattern
) where

import Graphics.QuantumHalftoning.Util
import Graphics.QuantumHalftoning.Images
import Graphics.QuantumHalftoning.Canvas
import Graphics.QuantumHalftoning
import Graphics.QuantumHalftoning.CLI.FilePattern

import qualified Codec.Picture as JP

import Data.Char
import Options.Applicative
import System.Exit

data Options = Options { parameters        ∷ !Parameters
                       , inputFile         ∷ !FilePath
                       , outputFilePattern ∷ !FilePattern }
             deriving (Eq, Ord, Show, Read)

mainWith ∷ Options → IO ()
mainWith Options{..} = do
  let outputFile = filePatternGenerator (imageCount parameters)
                                        outputFilePattern
  GrayscaleImage image ← either die pure =<< readGrayscaleImage inputFile
  either die id $ quantumHalftone parameters image $ \i canvas → do
    pixels ← unsafeFreezePixels canvas
    JP.writePng @JP.Pixel8
                (outputFile i)
                JP.Image { imageWidth  = width  canvas
                         , imageHeight = height canvas
                         , imageData   = pixels }

main ∷ IO ()
main = mainWith =<< execParser optionsInfo

--------------------------------------------------------------------------------

positive ∷ ReadM ℕ
positive = auto >>= \case
             0 → readerError "must be positive"
             n → pure n

readStartingCanvas ∷ ReadM StartingCanvas
readStartingCanvas = eitherReader $ \sc → case map toLower sc of
  "image" → Right Image
  "img"   → Right Image
  "i"     → Right Image
  "white" → Right AllWhite
  "w"     → Right AllWhite
  "black" → Right AllBlack
  "b"     → Right AllBlack
  _       → Left $ "unknown starting canvas mode `" ++ sc ++ "'"

readRefreshStyle ∷ ReadM RefreshStyle
readRefreshStyle = eitherReader $ \rs → case map toLower rs of
  "pixel" → Right RefreshPixel
  "px"    → Right RefreshPixel
  "p"     → Right RefreshPixel
  "image" → Right RefreshImage
  "img"   → Right RefreshImage
  "i"     → Right RefreshImage
  _       → Left $ "unknown starting canvas mode `" ++ rs ++ "'"

readFilePattern ∷ ReadM FilePattern
readFilePattern = eitherReader $ \str →
  maybe (Left $ "could not parse value: `" ++ str ++ "'") Right
    $ parseFilePattern str

--------------------------------------------------------------------------------

parametersParser ∷ Parser Parameters
parametersParser =
  Parameters <$> option auto
                 (  long    "expand-by"
                 <> short   'e'
                 <> metavar "N"
                 <> help    "Expand each grayscale inpixel to an N×N grid of \
                            \black and white outpixels" )
             <*> option readStartingCanvas
                 (  long    "start"
                 <> short   's'
                 <> metavar "CANVAS"
                 <> help    "Start with the image (\"image\"), a blank white \
                            \canvas (\"white\"), or a blank black canvas \
                            \(\"black\"); can be abbreviated")
             <*> option readRefreshStyle
                 (  long    "refresh"
                 <> short   'r'
                 <> metavar "STYLE"
                 <> help    "Refresh the image one pixel at a time (\"pixel\") \
                            \or all at once (\"image\"); can be abbreviated")
             <*> option positive
                 (  long    "count"
                 <> short   'n'
                 <> metavar "COUNT"
                 <> help    "Produce COUNT output images" )

optionsParser ∷ Parser Options
optionsParser =
  Options <$> parametersParser
          <*> strArgument
              (  metavar "INPUT-FILE"
              <> help    "The grayscale image to read in" )
          <*> argument readFilePattern
              (  metavar "OUTPUT-PATTERN"
              <> help    "The halftoned images to print out; should contain \
                         \one placeholder for the image number, which is of \
                         \the form \"*\", \"?…?\", \"%3d\", or \"%*d\" (the \
                         \\"*\" forms will pad with zeros to the appropriate \
                         \length; the explicit forms will pad with zeros to \
                         \the requested length)" )
                  
optionsInfo ∷ ParserInfo Options
optionsInfo =
  info (helper <*> optionsParser)
  $  fullDesc
  <> header   "quantum-halftone - \
              \Grayscale images to B&W, maximizing the mutual information"
  <> progDesc "Quantum halftoning, as in \"Schrödinger's Zebra\""
