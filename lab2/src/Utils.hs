{-# LANGUAGE OverloadedStrings #-}

module Utils(
    splitURL
  , listStrToBS
  ) where

import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8

-- removeProt :: Sting -> String
-- removeProt = 

splitURL :: String -> (String, String)
splitURL = Bi.second ensureSlash . span (/= '/')

ensureSlash :: String -> String
ensureSlash ('/':str) = '/':str
ensureSlash str = '/':str

listStrToBS :: [String] -> BS.ByteString
listStrToBS = foldr (BS.append . BC8.pack) ""
