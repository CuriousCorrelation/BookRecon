{-# LANGUAGE OverloadedStrings #-}

module Recon
  ( recon
  ) where

import Data.Maybe (catMaybes, fromMaybe)
import Internal.Options (SearchOptions(..))
import Internal.Types (BookInfo(..), Genre)
import Options.Options (execSearchOptionsParser)
import Printer.Printer (printBookInfoList)
import Search.SearchRecon (addBaseLink, reconStrict, reconUnStrict, subset)
import Data.ByteString.Lazy (ByteString)

safeFrom :: SearchOptions -> Int
safeFrom = fromMaybe 0 . from

safeTo :: SearchOptions -> Int
safeTo = fromMaybe 10 . to

safeLink :: SearchOptions -> ByteString
safeLink = link

safeGenres :: SearchOptions -> [ByteString]
safeGenres = genres

safeStrictness :: SearchOptions -> Bool
safeStrictness = strictness

recon :: IO ()
recon = do
  options <- execSearchOptionsParser
  let link' = safeLink options
      from' = safeFrom options
      to' = safeTo options
      strictness' = safeStrictness options
      genres' = safeGenres options
  if strictness'
    then printBookInfoList genres' =<< reconStrict link' from' to' genres'
    else printBookInfoList genres' =<< reconUnStrict link' from' to' genres'
