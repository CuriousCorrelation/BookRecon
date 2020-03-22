-- |
module Printer.Printer
  ( boxBookInfo
  , printBookInfoList
  )
where

import           Data.ByteString.Internal      as BI
import           Data.ByteString.Lazy          as BL
import           Data.Maybe                     ( catMaybes )
import           Internal.Types                 ( BookInfo(..)
                                                , Genre
                                                )
import           Search.SearchRecon             ( addBaseLink )
import           Text.PrettyPrint.Boxes         ( Box
                                                , (//)
                                                , (/+/)
                                                , (<+>)
                                                , emptyBox
                                                , left
                                                , printBox
                                                , right
                                                , text
                                                , vcat
                                                )

getBookURL :: BookInfo -> BL.ByteString
getBookURL = bookURL

getBookTitle :: BookInfo -> BL.ByteString
getBookTitle = bookTitle

getTop5BookGenres :: BookInfo -> [BL.ByteString]
getTop5BookGenres = Prelude.take 5 . catMaybes . bookGenre

getMatchedGenres :: [BL.ByteString] -> BookInfo -> [BL.ByteString]
getMatchedGenres genresToMatch bookInfo =
  [ y | y <- genresToMatch, x <- catMaybes (bookGenre bookInfo), x == y ]

fromLazyByteStringToString :: BL.ByteString -> String
fromLazyByteStringToString = fmap BI.w2c . BL.unpack

boxText :: BL.ByteString -> Box
boxText = text . fromLazyByteStringToString

breaker :: Int -> Box
breaker i = text $ Prelude.replicate i '─'

breaker50 :: Box
breaker50 = breaker 50

boxBookInfo :: [Genre] -> BookInfo -> Box
boxBookInfo genresToMatch bookInfo =
  breaker50 // boxBookTitle // breaker50 // vcat
    left
    [ text "URL - " <+> boxBookURL
    , (text "Top 5 Genres" /+/ boxBookTop5Genres)
    <+> emptyBox 1 1
    <+> (text "Matched Genres" /+/ boxBookMatchedGenres)
    ]

 where
  boxBookURL        = boxText $ addBaseLink $ getBookURL bookInfo
  boxBookTitle      = boxText $ getBookTitle bookInfo
  boxBookTop5Genres = vcat left $ boxText <$> getTop5BookGenres bookInfo
  boxBookMatchedGenres =
    vcat right $ boxText <$> getMatchedGenres genresToMatch bookInfo

printBookInfoList :: [Genre] -> [BookInfo] -> IO ()
printBookInfoList genresToMatch =
  printBox . vcat left . fmap (boxBookInfo genresToMatch)
