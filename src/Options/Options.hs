-- |

module Options.Options
  ( execSearchOptionsParser
  )
where

import           Options.Applicative
import           Internal.Options
import           Data.ByteString.Lazy           ( ByteString )
import           Text.Read                      ( readMaybe )

linkParser :: Parser ByteString
linkParser =
  strOption (long "link" <> short 'l' <> help "A Goodreads page link.")

fromParser :: Parser (Maybe Int)
fromParser = readMaybe <$> strOption
  (long "from" <> short 'f' <> help "Number of link to process from.")

toParser :: Parser (Maybe Int)
toParser = readMaybe <$> strOption
  (long "to" <> short 't' <> help "Number of link to process to/till.")

strictnessParser :: Parser Bool
strictnessParser = switch
  (long "strictness" <> short 's' <> help
    "To match genres strictly (even one genre mismatch disqualifies a book)."
  )

genresParser :: Parser [Genre]
genresParser =
  many $ strOption (long "genres" <> short 'g' <> help "Genres to match.")

ungenresParser :: Parser [Genre]
ungenresParser = many
  $ strOption (long "notgenres" <> short 'n' <> help "Genres NOT to match.")

parseSearchOptions :: Parser SearchOptions
parseSearchOptions =
  SearchOptions
    <$> linkParser
    <*> fromParser
    <*> toParser
    <*> strictnessParser
    <*> genresParser
    <*> ungenresParser

execSearchOptionsParser :: IO SearchOptions
execSearchOptionsParser = execParser $ info
  (parseSearchOptions <**> helper)
  (  fullDesc
  <> progDesc "Find books according to your favourite genres!"
  <> header "BookRecon - Terminal book reconnaissance."
  )
