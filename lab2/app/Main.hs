{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Utils

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8

import System.Environment

import Data.List.Extra

import Control.Monad.Except
import Control.Error.Util

import Control.Concurrent

import Data.Bifunctor

type Error = String

crlf :: BS.ByteString
crlf = "\r\n"
headerEnd :: BS.ByteString
headerEnd = "\r\n\r\n"

bsa = BS.append
bp = BC8.pack

usageInfo :: String
usageInfo = "USAGE: stack run -- <URL>"

main :: IO ()
main = do
  validArgs <- runExceptT getAppArgs
  case validArgs of
    Left errMsg -> putStrLn errMsg
    Right url -> do
      let (host, path) = splitURL url
      hostInfo <- head <$> resolve host
      let addr = addrAddress hostInfo 

      sock <- socket AF_INET Stream 0 -- IPv4
      -- sock <- socket AF_INET6 Stream 0 
      connect sock addr

      -- let req =  "GET " `bsa` bp path `bsa` " HTPP/1.1" `bsa` crlf `bsa` "Host: " `bsa` bp host `bsa` crlf `bsa` crlf
      let req = "GET " `bsa` bp path `bsa` " HTTP/1.1\r\nHost: " `bsa` bp host `bsa`"\r\n\r\n"
      print req
      sendAll sock req 

      (header, after_header) <- recvUntil sock headerEnd
      print header
      print after_header

      close sock

recvUntil :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvUntil = recvUntil' ""
      where
        recvLen = 3

        recvUntil' :: BS.ByteString -> Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
        recvUntil' acc sock' untilBS' = do
          msg <- recv sock' recvLen

          let (bef, aft) = BS.breakSubstring untilBS' $ acc `BC8.append` msg
          if BS.null aft then
            recvUntil' bef sock' untilBS'
          else
            return (bef, BS.drop (BS.length untilBS') aft)

resolve :: String -> IO [AddrInfo]
resolve host = do
  -- let hints = defaultHints { addrFlags = [AI_ALL], addrSocketType = Stream }
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream } -- IPv4

  getAddrInfo (Just hints) (Just host) (Just "http")

getAppArgs :: ExceptT Error IO String
getAppArgs = do
  args <- lift getArgs
  hoistEither $ validArgs args

    where
      validArgs :: [String] -> Either Error String
      validArgs [url] = do
        return url 
      validArgs _ = Left usageInfo


