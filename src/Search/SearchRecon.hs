{-# LANGUAGE OverloadedStrings #-}

-- |
module Search.SearchRecon where

import           System.Console.AsciiProgress
import           Control.Concurrent.ParallelIO.Global
                                                ( parallel
                                                , stopGlobalPool
                                                )
import           Data.ByteString.Lazy           ( ByteString
                                                , append
                                                , pack
                                                , stripPrefix
                                                , unpack
                                                )
import           Data.List                      ( intersect )
import           Data.Maybe                     ( catMaybes )
import           Data.Word8                     ( isNumber )
import           Internal.Types                 ( BookInfo(..)
                                                , Genre
                                                , Link
                                                )
import           Search.BookLinks               ( getBookLinks
                                                , getBookTitles
                                                )
import           Search.GenreLinks              ( getGenreLinks )
import           Search.ShelfLinks              ( getShelfLinks )

type From = Int

type To = Int

subset :: Eq a => [a] -> [a] -> Bool
subset x y = or $ (==) <$> x <*> y

properSubset :: Eq a => [a] -> [a] -> Bool
properSubset x y = intersect x y == x

mutuallyExclusive :: Eq a => [a] -> [a] -> Bool
mutuallyExclusive x y = (not . or) $ (==) <$> x <*> y

addBaseLink :: ByteString -> ByteString
addBaseLink = append "https://www.goodreads.com"

outGenre :: [Genre] -> BookInfo -> Bool
outGenre withoutGenre bookinfo =
  withoutGenre `mutuallyExclusive` (catMaybes . bookGenre) bookinfo

inGenresStrict :: [Genre] -> BookInfo -> Bool
inGenresStrict genresToMatch bookinfo =
  genresToMatch `properSubset` (catMaybes . bookGenre) bookinfo

inGenresUnStrict :: [Genre] -> BookInfo -> Bool
inGenresUnStrict genresToMatch bookinfo =
  genresToMatch `subset` (catMaybes . bookGenre) bookinfo

recon' :: Link -> From -> To -> IO [BookInfo]
recon' link from to = displayConsoleRegions $ do


  let from' = if to > from then from else to
      to'   = if to > from then to - from else from - to

  progressBar <- newProgressBar def
    { pgWidth        = 100
    , pgOnCompletion = Just "Printing results (this may take a few moments)."
    , pgTotal        = fromIntegral $ to' - from'
    }

  bookLinks <- take to' . drop from' <$> getBookLinks link
  tick progressBar

  bookTitles <- take to' . drop from' <$> getBookTitles link
  tick progressBar

  shelfLinks <- parallel (getShelfLinks progressBar . addBaseLink <$> bookLinks)
  tick progressBar

  genres <- parallel (getGenreLinks progressBar . addBaseLink <$> shelfLinks)
  tick progressBar

  stopGlobalPool

  tick progressBar
  complete progressBar

  return $ zipWith3 BookInfo bookTitles bookLinks genres

bookLinkToName :: ByteString -> Maybe ByteString
bookLinkToName =
  fmap (pack . tail . dropWhile isNumber . unpack) . stripPrefix "/book/show/"

reconStrict :: Link -> From -> To -> [Genre] -> [Genre] -> IO [BookInfo]
reconStrict link from to genresToMatch genresToUnMatch =
  filter (outGenre genresToUnMatch)
    .   filter (inGenresStrict genresToMatch)
    <$> recon' link from to

reconUnStrict :: Link -> From -> To -> [Genre] -> [Genre] -> IO [BookInfo]
reconUnStrict link from to genresToMatch genresToUnMatch =
  filter (outGenre genresToUnMatch)
    .   filter (inGenresUnStrict genresToMatch)
    <$> recon' link from to
