{-# LANGUAGE OverloadedStrings #-}

module Recon
  ( recon
  )
where

import           GHC.IO.Encoding                ( setLocaleEncoding
                                                , utf8 )
import           Data.Maybe                     ( fromMaybe )
import           Internal.Options               ( SearchOptions(..) )
import           Options.Options                ( execSearchOptionsParser )
import           Printer.Printer                ( printBookInfoList )
import           Search.SearchRecon             ( addBaseLink
                                                , reconStrict
                                                , reconUnStrict
                                                , subset
                                                )
import           Data.ByteString.Lazy           ( ByteString )

safeFrom :: SearchOptions -> Int
safeFrom = fromMaybe 0 . from

safeTo :: SearchOptions -> Int
safeTo = fromMaybe 10 . to

safeLink :: SearchOptions -> ByteString
safeLink = link

safeGenres :: SearchOptions -> [ByteString]
safeGenres = matchGenres

safeNotGenres :: SearchOptions -> [ByteString]
safeNotGenres = unmatchGenres

safeStrictness :: SearchOptions -> Bool
safeStrictness = strictness

recon :: IO ()
recon = do
  setLocaleEncoding utf8
  options <- execSearchOptionsParser

  let link'          = safeLink options
      from'          = safeFrom options
      to'            = safeTo options
      strictness'    = safeStrictness options
      matchgenres'   = safeGenres options
      unmatchgenres' = safeNotGenres options


  if strictness'
    then
      printBookInfoList matchgenres'
        =<< reconStrict link' from' to' matchgenres' unmatchgenres'
    else
      printBookInfoList matchgenres'
        =<< reconUnStrict link' from' to' matchgenres' unmatchgenres'
