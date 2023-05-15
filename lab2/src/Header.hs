{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Header(
    Header(..)
  , HeaderWarn(..)
  , ResponseHead
  , version
  , statusCode
  , headers
  , Version
  , StatusCode
  , Headers
  , RequestHead(..)
  , parseHeaders
  , runReadP
  , getHContentLength
  , getHLocation
  , parseResponseHead
  ) where

import StatusCode as SC

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
import qualified Data.List as L
import qualified Debug.Trace as DT

data Method = GET | POST | HEAD | PUT | DELETE | CONNECT | OPTIONS | TRACE | PATCH | Other String

data Version = HTTP10 | HTTP11 -- | HTTP2 | HTTP3 not relevent
instance Show Version where
  show HTTP10 = "HTTP/1.0"
  show HTTP11 = "HTTP/1.1"

type URL = String
type Headers = [Header]

data RequestHead = RequestHead Method URL Version [Header]
data ResponseHead = ResponseHead { 
    version :: Version
  , statusCode :: StatusCode
  , headers :: Headers
}

data Header =
    HContentType String
  | HContentLength Int
  | HDate T.UTCTime
  | HServer String
  | HLocation String
  -- TODO: fix www-authenticate
  -- it is more complex than a string
  | HWWWAuthenticate [String]     -- TODO: parser for this header
  -- TOOD: fix set-cookie, ditto
  | HSetCookie [(String, String)] -- TODO: parser for this header
  | HValueless [String]           -- Represents fields with missing values
                                  -- it is not a part of a http spec
  | HOther (String, String)
  deriving (Show)

getHContentLength :: [Header] -> Maybe Int
getHContentLength = getHeaderBuilder toMaybeHContentLenght

getHLocation :: [Header] -> Maybe String
getHLocation = getHeaderBuilder toMaybeHLocation

getHeaderBuilder :: (Header -> Maybe a) -> [Header] -> Maybe a
getHeaderBuilder f hdrs = fromJust . f <$> L.find (isJust . f) hdrs

toMaybeHContentLenght :: Header -> Maybe Int
toMaybeHContentLenght (HContentLength len) = Just len
toMaybeHContentLenght _ = Nothing

toMaybeHLocation :: Header -> Maybe String
toMaybeHLocation (HLocation loc) = Just loc
toMaybeHLocation _ = Nothing

-- TODO
getHeader :: [Header] -> String -> Maybe String
getHeader hdrs hdr = Just "5"
  where
    hrd' = map C.toLower hdr

    getHeader' :: [Header] -> String -> Maybe a
    getHeader' hdrs hrd = undefined

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

parseResponseHead :: BS.ByteString -> Maybe ResponseHead
parseResponseHead str = do
  let (firstLine, headersStr) = BS.breakSubstring "\r\n" str

  (ver, statCode) <- runReadP parseFirstLine ( BS8.toString firstLine )
  let headers = ( mapMaybe (parseHeader . BS8.toString) . BSS.split "\r\n" ) headersStr

  return $ ResponseHead ver statCode headers
  where

    parseFirstLine :: P.ReadP (Version, StatusCode)
    parseFirstLine = do
      _ <- P.string "HTTP/"
      ver <- ( HTTP11 <$ P.string "1.1" ) P.<++ ( HTTP10 <$ P.string "1.0" )
      _ <- P.skipSpaces
      code <- intP
      _ <- P.skipSpaces
      str' <- P.munch (/= '\r')
      return (ver, SC.fromIntStr code str')

    ( ||| ) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    p ||| q = \a -> p a || p a


parseHeader :: String -> Maybe Header
parseHeader = runReadP $
        hDateP
  P.<++ hContentLengthP
  P.<++ (HContentType <$> hKVPValueP "Content-Type:")
  P.<++ (HServer      <$> hKVPValueP "Server:")
  P.<++ (HLocation    <$> hKVPValueP "Location:")
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
