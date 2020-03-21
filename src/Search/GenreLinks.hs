{-# LANGUAGE OverloadedStrings #-}

-- |

module Search.GenreLinks
  ( getGenreLinks
  )
where

import           Data.ByteString.Internal       ( w2c )
import           Data.ByteString.Lazy          as BL
                                                ( ByteString
                                                , unpack
                                                , stripPrefix
                                                , pack
                                                )
import           Network.HTTP.Conduit           ( simpleHttp )
import           Text.HTML.TagSoup              ( Tag
                                                , fromAttrib
                                                , parseTags
                                                )
import           Text.HTML.TagSoup.Match        ( tagOpenAttrLit )
import           Internal.Types                 ( Link )
import           Data.Word8                     ( toLower )

toLowerByteString :: ByteString -> ByteString
toLowerByteString = pack . map toLower . unpack

genreLinkCheck :: Tag ByteString -> Bool
genreLinkCheck = tagOpenAttrLit "a" ("class", "mediumText actionLinkLite")

filterGenreLinks :: [Tag ByteString] -> [Maybe ByteString]
filterGenreLinks =
  fmap (stripPrefix "/genres/" . toLowerByteString . fromAttrib "href")
    . filter genreLinkCheck

getGenreLinks :: Link -> IO [Maybe ByteString]
getGenreLinks link = do
  putStrLn $ "Finding genres from - " ++ (w2c <$> BL.unpack link)
  filterGenreLinks . parseTags <$> simpleHttp (w2c <$> BL.unpack link)
