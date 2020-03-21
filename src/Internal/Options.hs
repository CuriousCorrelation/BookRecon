-- |

module Internal.Options where

import           Data.ByteString.Lazy           ( ByteString )

type Genre = ByteString

data SearchOptions = SearchOptions { link :: ByteString
                                   , from :: Maybe Int
                                   , to :: Maybe Int
                                   , strictness :: Bool
                                   , genres :: [Genre]
                                   } deriving Show
