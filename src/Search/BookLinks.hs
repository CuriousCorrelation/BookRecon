{-# LANGUAGE OverloadedStrings #-}

-- |
module Search.BookLinks
  ( getBookLinks
  , getBookTitles
  )
where

import           Data.ByteString.Internal       ( w2c )
import           Data.ByteString.Lazy          as BL
                                                ( ByteString
                                                , isPrefixOf
                                                , unpack
                                                )
import           Data.List                      ( nub )
import           Internal.Types                 ( Link )
import           Network.HTTP.Conduit           ( simpleHttp )
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match

bookLinkCheck :: Tag ByteString -> Bool
bookLinkCheck = isTagOpenName "a"

filterBookLinks :: [Tag ByteString] -> [ByteString]
filterBookLinks =
  filter (isPrefixOf "/book/show/")
    . fmap (fromAttrib "href")
    . filter bookLinkCheck

fixUpBookLinks :: [Tag ByteString] -> [ByteString]
fixUpBookLinks = filterBookLinks

getBookLinks :: Link -> IO [ByteString]
getBookLinks link =
  nub . fixUpBookLinks . parseTags <$> simpleHttp (w2c <$> BL.unpack link)

-- | --
bookTitleCheck :: Tag ByteString -> Bool
bookTitleCheck = tagOpen
  (== "img")
  (\a ->
    elem ("class", "bookImage") a
      || elem ("class", "gr-box--withShadow responsiveBook__img") a
  )

filterBookTitles :: [Tag ByteString] -> [ByteString]
filterBookTitles = fmap (fromAttrib "alt") . filter bookTitleCheck

fixUpBookTitles :: [Tag ByteString] -> [ByteString]
fixUpBookTitles = filterBookTitles

getBookTitles :: Link -> IO [ByteString]
getBookTitles link =
  nub . fixUpBookTitles . parseTags <$> simpleHttp (w2c <$> BL.unpack link)
