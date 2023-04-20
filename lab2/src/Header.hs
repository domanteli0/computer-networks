module Header(Header(..), HeaderWarn(..), parseHeader, validateHeaders, runReadP) where

import qualified Data.Time as T
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Char as C
import qualified Data.List.Extra as E
import Control.Applicative (Alternative((<|>)))

data Header =
    HContentType String
  | HContentLength Int
  | HDate T.UTCTime
  | HOther (String, String)
  deriving (Show)

data HeaderWarn =
    HWarnDuplicateHeaders [(Int, String, String)]
  | HWarnDoubleDiffCaseHeaders [(Int, String, String)]
  | HWarnMissingColon [(Int, String)]

validateHeaders :: String -> [HeaderWarn]
validateHeaders = undefined

parseHeader :: String -> Maybe Header
parseHeader = runReadP $ 
    hDateP
  P.<++ hContentLengthP
  P.<++ hContentTypeP
  P.<++ hOtherP

  where
    hContentTypeP :: P.ReadP Header
    hContentTypeP = HContentType <$> (
        stringCaseInP "Content-Type:" *>
         takeTillCrOrNlP
      )

    hContentLengthP :: P.ReadP Header
    hContentLengthP = HContentLength <$> (stringCaseInP "Content-Length:" *> P.skipSpaces *> intP)
    hOtherP :: P.ReadP Header
    hOtherP =
          (curry HOther <$> (P.munch1 (/= ':') <* P.skipSpaces))
      <*> (E.trim . drop 1 <$> P.many (P.satisfy (const True)))

    hDateP :: P.ReadP Header 
    hDateP = do
      str <- stringCaseInP "Date:" *> P.skipSpaces *> takeTillCrOrNlP
      date <- T.parseTimeM True T.defaultTimeLocale  "%a, %0d %b %0Y %X" str
      return $ HDate date

 
takeTillCrOrNlP :: P.ReadP String
takeTillCrOrNlP = E.trim <$> P.many (P.satisfy (\c -> c /= '\n' || c /= '\r'))

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
