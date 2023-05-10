{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Error.Util
import Control.Monad.Except
import Control.Exception.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.UTF8 as BS8
import Header
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import Utils

import qualified Data.Maybe as M
import Data.ByteString (append)

import qualified MySocket as MS

newtype AppError = AppError String
  deriving (Show)

instance Exception AppError where
  -- toException = undefined

crlf :: BS.ByteString
crlf = "\r\n"

headerEnd :: BS.ByteString
headerEnd = "\r\n\r\n"

lToBS :: [String] -> BS.ByteString
lToBS = foldr (BS.append . BC8.pack) ""

usageInfo :: String
usageInfo = "USAGE: stack run -- <URL> [PORT]"

-- TODO:
-- - Socket type, holds the buffer
-- - Getting after headerEnd
-- - Chuncked https://stackoverflow.com/questions/19907628/transfer-encoding-chunked
-- - redirect (if flag is passed)
-- - multiple same key headers: https://stackoverflow.com/questions/3241326/set-more-than-one-http-header-with-the-same-name

main :: IO ()
main = do
  validArgs <- runExceptT getAppArgs
  case validArgs of
    Left errMsg -> print errMsg
    Right (url, maybePort) -> do
      let (host, path) = splitURL url
      hostInfo <- head <$> resolve host maybePort
      let sockAddr = addrAddress hostInfo
      -- let sockAddr = SockAddrInte6 -- TODO: IPv6
      -- let port = PortNumber sockAddr
      -- let port = addr

      sock <- socket AF_INET Stream 0 -- IPv4
      -- sock <- socket AF_INET6 Stream 0
      connect sock sockAddr

      sock <- 
        MS.runMySock
        (do liftIO $ MS.untry $ MS.fromHostnameTCP4 host maybePort)
        ()
      
      

      let req = lToBS ["GET ", path, " HTTP/1.1\r\nHost: ", host, "\r\n\r\n"]
      print req
      -- `sendAll` raises an error upon failure
      -- TODO: rework with `try`
      -- https://www.reddit.com/r/haskell/comments/z5ezoa/why_network_requests_throw_exceptions_instead_of/
      sendAll sock req

      (header, afterHeader) <- recvUntil sock headerEnd
      let headers = parseHeaders header
      let maybeCLen = getHContentLength $ snd headers

      print headers

      _ <- ioIfJust maybeCLen ( \len -> do
          content <- recv sock len
          print $ afterHeader `append` content
          return ()
        ) (print "NO CONTENT LEN")

      -- case maybeCLen of
      --   Just len -> do
      --     content <- recv sock len
      --     print $ afterHeader `append` content 
      --   Nothing -> do
      --     print "NO CONTENT LEN\n"

      close sock

doMain :: MS.MySock (BS.ByteString, Socket) SomeException ()
doMain = do
  validArgs <- lift getAppArgs

  return ()
 

ioIfJust :: Maybe a -> (a -> IO ()) -> IO () -> IO ()
ioIfJust (Just s) fun _ = fun s
ioIfJust _ _ fun = fun

-- recvTake :: Socket -> Int -> IO (BS.ByteString, BS.ByteString)
-- recvTake sock noBytes = recvUntil' "" sock ""
--   where
--     recvLen = 1024

--     recvUntil' :: BS.ByteString -> Socket -> Int -> IO (BS.ByteString, BS.ByteString)
--     recvUntil' acc sock' noBytes' = do
--       if 0 == min noBytes' 0 
--         then do return acc
--         else do 
--           msg <- recv sock' recvLen

--           retrun undefined

recvUntil :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvUntil = recvUntil' ""
  where
    recvLen = 1024

    recvUntil' :: BS.ByteString -> Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
    recvUntil' acc sock' untilBS' = do
      msg <- recv sock' recvLen

      let (bef, aft) = BS.breakSubstring untilBS' $ acc `BC8.append` msg
      if BS.null aft
        then recvUntil' bef sock' untilBS'
        else return (bef, BS.drop (BS.length untilBS') aft)

resolve :: String -> Maybe String -> IO [AddrInfo]
resolve host maybeServiceName = do
  -- let hints = defaultHints { addrFlags = [AI_ALL], addrSocketType = Stream }
  let hints = defaultHints {addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream} -- IPv4
  getAddrInfo (Just hints) (Just host) (Just ( M.fromMaybe "http" maybeServiceName ))

getAppArgs :: ExceptT SomeException IO (String, Maybe String)
getAppArgs = do
  args <- lift getArgs
  hoistEither $ validArgs args
  where
    validArgs :: [String] -> Either SomeException (String, Maybe String)
    validArgs [url] = Right (url, Nothing)
    validArgs [url, port] = Right (url, Just port)
    validArgs _ = Left $ toException $ AppError usageInfo
