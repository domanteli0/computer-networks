{-# LANGUAGE OverloadedStrings #-}

module Utils(
    splitURL
  , listStrToBS
  ) where

import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8

splitURL :: String -> (String, String)
splitURL = Bi.second ensureSlash . span (/= '/') . stripProtocol

ensureSlash :: String -> String
ensureSlash ('/':str) = '/':str
ensureSlash str = '/':str

stripProtocol :: String -> String
stripProtocol ( 'h':'t':'t':'p':':':'/':'/':str ) = str
stripProtocol ( 'h':'t':'t':'p':'s':':':'/':'/':str ) = str
stripProtocol str = str

listStrToBS :: [String] -> BS.ByteString
listStrToBS = foldr (BS.append . BC8.pack) ""
