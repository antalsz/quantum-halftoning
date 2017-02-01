{-# LANGUAGE TypeApplications, TypeFamilies, UnicodeSyntax #-}

module Graphics.QuantumHalftoning.CLI.FilePattern (
  -- * File patterns
  FilePattern, 
  parseFilePattern,
  filePatternGenerator,
  -- * Parsers
  filePattern, literal, placeholder,
  -- * Digit/string manipulation
  digits, padded
) where

import Graphics.QuantumHalftoning.Util

import Data.Functor
import Data.Maybe

import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.Prim (MonadParsec())

--------------------------------------------------------------------------------

type FilePattern = (String, Maybe ℕ, String)

literal ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m String
literal = many $   noneOf "*?%\\"
              <|> '%' <$ string "%%"
              <|> char '\\' *> anyChar

placeholder ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m (Maybe ℕ)
placeholder = star <|> questions <|> printf where
  star      = Nothing                      <$  char '*'
  questions = Just . fromIntegral . length <$> some (char '?')
  printf    = char '%' *> width <* oneOf "di"
  width     = option (Just 0) $ do
                void $ many (char '0')
                Nothing <$ char '*' <|> Just . fromInteger <$> integer

filePattern ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m FilePattern
filePattern = (,,) <$> literal <*> placeholder <*> literal

parseFilePattern ∷ String → Maybe FilePattern
parseFilePattern str = parseMaybe @Dec (filePattern <* eof) str

digits ∷ Num n ⇒ ℕ → n
digits n | n < 10    = 1
         | otherwise = 1 + digits (n `quot` 10)
{-# SPECIALIZE digits ∷ ℕ → ℕ       #-}
{-# SPECIALIZE digits ∷ ℕ → Integer #-}
{-# SPECIALIZE digits ∷ ℕ → Int     #-}

padded ∷ ℕ → ℕ → String
padded w n = replicate (fromIntegral w - digits n) '0' ++ show n

filePatternGenerator ∷ ℕ → FilePattern → (ℕ → String)
filePatternGenerator bound (pre,placeholder,post) =
  let width = fromMaybe (digits bound) placeholder
  in \i → pre ++ padded width i ++ post
