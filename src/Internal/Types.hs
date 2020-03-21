-- |

module Internal.Types where

import           Data.ByteString.Lazy           ( ByteString )

type Link = ByteString

type Genre = ByteString

data BookInfo = BookInfo { bookTitle :: ByteString
                         , bookURL :: ByteString
                         , bookGenre :: [Maybe ByteString]
                         } deriving Show
