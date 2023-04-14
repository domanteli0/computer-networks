{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Utils

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8

import System.Environment

import Control.Monad.Except
import Control.Error.Util

type Error = String

crlf :: BS.ByteString
crlf = "\r\n"

bsa = BS.append
bp = BC8.pack

usageInfo :: String
usageInfo = "USAGE: stack run -- <URL>"

main = do
  validArgs <- runExceptT getAppArgs
  case validArgs of
    Left errMsg -> putStrLn errMsg
    Right url -> do
      let (host, path) = splitURL url

      sock <- socket AF_INET Stream 0
      hostInfo <- head <$> resolve host
      connect sock $ addrAddress hostInfo

      -- let req =  "GET " `bsa` bp path `bsa` " HTPP/1.1" `bsa` crlf `bsa` "Host: " `bsa` bp host `bsa` crlf `bsa` crlf
      let req = "GET " `bsa` bp path `bsa` " HTTP/1.1\r\nHost: " `bsa` bp host `bsa`"\r\n\r\n"
      print req
      sendAll sock req 
      msg <- recv sock 1024

      

      close sock
      print msg

resolve :: String -> IO [AddrInfo]
resolve host = do
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream }

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


