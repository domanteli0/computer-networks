{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Header(
    Header(..)
  , HeaderWarn(..)
  , parseHeaders
  , runReadP
  , getHContentLength
  ) where

import qualified Data.Time as T
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Data.List.Extra as LE
import qualified Data.Tuple.Extra as TE

import Control.Applicative (Alternative((<|>)))
import Data.Maybe as M

import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (find)

data Header =
    HContentType String
  | HContentLength Int
  | HDate T.UTCTime
  | HServer String
  -- TODO: fix www-authenticate
  -- it is more complex than a string
  | HWWWAuthenticate [String]     -- TODO: parser for this header
  -- TOOD: fix set-cookie, ditto
  | HSetCookie [(String, String)] -- TODO: parser for this header
  | HValueless [String]           -- Represents fields with missing values
                                  -- it is not a part of a http spec
  | HOther (String, String)
  deriving (Show)

-- instance Show Header where
--   show (HContentLength len) = "Content-Length: " ++ show len
--   show _ = undefined

getHContentLength :: [Header] -> Maybe Int
getHContentLength hdrs = 
    fromJust
  . toMaybeHContentLenght
  <$> find (isJust . toMaybeHContentLenght) hdrs

toMaybeHContentLenght :: Header -> Maybe Int
toMaybeHContentLenght (HContentLength len) = Just len
toMaybeHContentLenght _ = Nothing

getHeader :: [Header] -> String -> Maybe String
getHeader hdrs hdr = Just "5"
  where
    hrd' = map C.toLower hdr

    getHeader' :: [Header] -> String -> Maybe a
    getHeader' hdrs hrd = undefined

-- getHeaderValue :: Header -> a
-- getHeaderValue

data HeaderWarn =
    HWarnDuplicateHeaders [(String, [Int])]
  | HWarnMissingValue [(Int, String)]
  deriving (Show)

-- TODO: Errors and warnings
parseHeaders :: BS.ByteString -> ([HeaderWarn], [Header])
parseHeaders =
    bimap
      ( validateHeaders . map BS8.toString)
      ( mapMaybe (parseHeader . BS8.toString) )
  . TE.dupe
  . BSS.split "\r\n"

validateHeaders _ = []
-- validateHeaders :: [Header] -> ([HeaderWarn], [Header])
-- validateHeaders = undefined

parseHeader :: String -> Maybe Header
parseHeader = runReadP $
        hDateP
  P.<++ hContentLengthP
  P.<++ (HContentType <$> hKVPValueP "Content-Type:")
  P.<++ (HServer <$> hKVPValueP "Server:")
  P.<++ hOtherP

  where
    hContentLengthP :: P.ReadP Header
    hContentLengthP = HContentLength <$> (stringCaseInP "Content-Length:" *> P.skipSpaces *> intP)

    hOtherP :: P.ReadP Header
    hOtherP = HOther <$> hKVPP ':'

    hDateP :: P.ReadP Header
    hDateP = do
      str <- stringCaseInP "Date:" *> P.skipSpaces *> takeTillCrOrNlP
      date <- T.parseTimeM True T.defaultTimeLocale  "%a, %0d %b %0Y %X" str
      return $ HDate date

hKVPP :: Char -> P.ReadP (String, String)
hKVPP sep =
      ( (,) <$> (P.munch1 (/= sep) <* P.char sep <* P.skipSpaces) )
  <*> takeTillCrOrNlP

hKVPValueP :: String -> P.ReadP String
hKVPValueP str = stringCaseInP str *> takeTillCrOrNlP

takeTillCrOrNlP :: P.ReadP String
takeTillCrOrNlP = LE.trim <$> P.many (P.satisfy (\c -> c /= '\n' || c /= '\r'))

charWoCaseP :: Char -> P.ReadP Char
charWoCaseP c = P.char (C.toLower c) <|> P.char (C.toUpper c)

stringCaseInP :: String -> P.ReadP String
stringCaseInP = mapM charWoCaseP

runReadP :: P.ReadP a -> String -> Maybe a
runReadP p = fmap fst . lastOrNothing . P.readP_to_S p
  where
    lastOrNothing :: [a] -> Maybe a
    lastOrNothing [] = Nothing
    lastOrNothing xs = (Just . last) xs

intP :: P.ReadP Int
intP = do
    digits <- P.many1 $ P.satisfy C.isDigit
    return $ read digits
