{-# LANGUAGE OverloadedStrings #-}
-- |

module Search.SearchRecon where

import           Control.Concurrent.ParallelIO.Global
                                                ( parallel
                                                , stopGlobalPool
                                                )
import           Search.BookLinks               ( getBookLinks
                                                , getBookTitles
                                                )
import           Search.ShelfLinks              ( getShelfLinks )
import           Search.GenreLinks              ( getGenreLinks )
import           Internal.Types                 ( Link
                                                , BookInfo(..)
                                                , Genre
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , append
                                                , stripPrefix
                                                , unpack
                                                , pack
                                                )
import           Data.List                      ( intersect )
import           Data.Word8                     ( isNumber )
import           Data.Maybe                     ( catMaybes )

type From = Int
type To = Int

subset :: Eq a => [a] -> [a] -> Bool
subset x y = or $ (==) <$> x <*> y

properSubset :: Eq a => [a] -> [a] -> Bool
properSubset x y = intersect x y == x

addBaseLink :: ByteString -> ByteString
addBaseLink = append "https://www.goodreads.com"

inGenresStrict :: [Genre] -> BookInfo -> Bool
inGenresStrict genresToMatch bookinfo =
  genresToMatch `properSubset` (catMaybes . bookGenre) bookinfo

inGenresUnStrict :: [Genre] -> BookInfo -> Bool
inGenresUnStrict genresToMatch bookinfo =
  genresToMatch `subset` (catMaybes . bookGenre) bookinfo

recon' :: Link -> From -> To -> IO [BookInfo]
recon' link from to = do

  let from' = if to > from then from else to
      to'   = if to > from then to - from else from - to

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Processing..."

  bookLinks  <- take to' . drop from' <$> getBookLinks link
  bookTitles <- take to' . drop from' <$> getBookTitles link

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Found link for - "

  mapM_ print bookTitles
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  shelfLinks <- parallel (getShelfLinks . addBaseLink <$> bookLinks)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  genres <- parallel (getGenreLinks . addBaseLink <$> shelfLinks)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  stopGlobalPool
  return $ zipWith3 BookInfo bookTitles bookLinks genres


bookLinkToName :: ByteString -> Maybe ByteString
bookLinkToName =
  fmap (pack . tail . dropWhile isNumber . unpack) . stripPrefix "/book/show/"

reconStrict :: Link -> From -> To -> [Genre] -> IO [BookInfo]
reconStrict link from to genresToMatch =
  filter (inGenresStrict genresToMatch) <$> recon' link from to

reconUnStrict :: Link -> From -> To -> [Genre] -> IO [BookInfo]
reconUnStrict link from to genresToMatch =
  filter (inGenresUnStrict genresToMatch) <$> recon' link from to