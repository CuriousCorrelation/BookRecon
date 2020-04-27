{-# LANGUAGE OverloadedStrings #-}

-- |

module Search.ShelfLinks
  ( getShelfLinks
  )
where

import           System.Console.AsciiProgress   ( ProgressBar
                                                , tick
                                                )
import           Data.ByteString.Internal       ( w2c )
import           Data.Word8                    as W8
import           Data.ByteString.Lazy          as BL
                                                ( ByteString
                                                , isPrefixOf
                                                , dropWhile
                                                , takeWhile
                                                , append
                                                , unpack
                                                )
import           Network.HTTP.Conduit           ( simpleHttp )
import           Text.HTML.TagSoup              ( Tag
                                                , fromAttrib
                                                , isTagOpenName
                                                , parseTags
                                                )

import           Internal.Types                 ( Link )

bookLinkCheck :: Tag ByteString -> Bool
bookLinkCheck = isTagOpenName "a"

filterEditionLinks :: [Tag ByteString] -> [ByteString]
filterEditionLinks =
  filter (isPrefixOf "/work/editions/")
    . fmap (fromAttrib "href")
    . filter bookLinkCheck

fixUpShelfLinks :: [Tag ByteString] -> ByteString
fixUpShelfLinks =
  head
    . fmap
        (BL.append "/work/shelves/" . BL.takeWhile W8.isNumber . BL.dropWhile
          (not . W8.isNumber)
        )
    . filterEditionLinks

getShelfLinks :: ProgressBar -> Link -> IO ByteString
getShelfLinks pb link = do
  tick pb
  fixUpShelfLinks . parseTags <$> simpleHttp (w2c <$> BL.unpack link)
