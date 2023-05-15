{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Error.Util as ErrUtil
import Control.Monad.Except
import Control.Exception.Base
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Header as H
import Network.Socket
import Network.Socket.ByteString
import qualified System.Environment as Env
import qualified Utils

import qualified Data.Maybe as M
import qualified Nuke as N

data AppError = 
    AppError String
  | AppSomeException SomeException
  deriving (Show)

instance Exception AppError where
  -- toException = undefined

type AppState = (BS.ByteString, Socket) 

crlf :: BS.ByteString
crlf = "\r\n"
crlf2 = "\r\n\r\n"

usageInfo :: String
usageInfo = "USAGE: stack run -- <URL> [PORT]"

-- TODO:
-- - Chuncked https://stackoverflow.com/questions/19907628/transfer-encoding-chunked
-- - redirect (if flag is passed)
-- - multiple same key headers: https://stackoverflow.com/questions/3241326/set-more-than-one-http-header-with-the-same-name

main :: IO ()
main = do
  sock <- mkSocket 0
  s <- N.runNukeT 
    (N.finally doMain (\(_, a) -> close a))
    ("", sock)
  case s of
    Right _ -> putStrLn "DONE"
    Left e -> print e

doMain :: N.NukeT AppState AppError Socket
doMain = do
  (url, maybePort) <- lift getAppArgs

  let (host, path) = Utils.splitURL url
  hostInfo <- N.replaceErr
    ( head <$> N.tryNuke @SomeException ( resolve host maybePort ) )
    ( AppError ( "Could not resolve the hostname [" ++ host ++ "]" ) ) 
  let sockAddr = addrAddress hostInfo
  --     -- let sockAddr = SockAddrInte6 -- TODO: IPv6
  --     -- let port = PortNumber sockAddr
  --     -- let port = addr
  N.replaceErr ( do
    sock <- N.tryNuke @SomeException $ socket AF_INET Stream 0 -- IPv4
    _ <- N.tryNuke $ connect sock sockAddr
    put ("", sock) )
    (AppError "Failed to connect to host")
  (_, sock) <- get


  let req = Utils.listStrToBS ["GET ", path, " HTTP/1.1\r\nHost: ", host, "\r\n\r\n"]
  liftIO $ print req

  _ <- N.tryNuke $ sendAll sock req
  response <- N.replaceErr
    ( recvUntil crlf2 )
    ( AppError "Couldn't retrieve a response" ) 

  liftIO $ putStrLn "RESPONSE: "
  liftIO $ print response
  responseHead <- N.maybeNukeT 
      ( H.parseResponseHead response ) 
    $ AppError "The host responeded with malformed headers"

  (buf, _) <- get
  liftIO $ putStrLn "buf: "
  liftIO $ print buf
  
  let hLen = H.getHContentLength $ H.headers responseHead
  if M.isJust hLen
    then do
      body <- N.mapErr
        ( recvTake (M.fromJust hLen) )
        AppSomeException
      liftIO $ print "BODY:"
      liftIO $ print body
      return sock
    else do 
      N.throwNuke $ AppError "No Content-Length in response headers"

-- handleBody = [ handleBodyWithContentLen ]

handleBodyWithContentLen
  :: H.ResponseHead
  -> N.NukeT AppState AppError (Maybe BS.ByteString)
handleBodyWithContentLen msg = do undefined
--   if get

recvUntil :: BS.ByteString -> N.NukeT AppState SomeException BS.ByteString
recvUntil sub = do
  (buf, sock) <- get

  let (ret, buf') = BS.breakSubstring sub buf

  if not $ BS.null buf'
    then do
      put (BS.drop (BS.length sub) buf', sock)
      return ret
    else do
      msg <- N.tryNuke ( recv sock recvLen )
      put (buf `BS.append` msg , sock)
      recvUntil sub

  where
    recvLen :: Int
    recvLen = 128

recvTake :: Int -> N.NukeT AppState SomeException BS.ByteString
recvTake len = do
  (buf, sock) <- get
  let bufLen = BS.length buf
  if len <= bufLen
    then do
      let (ret, buf') = BS.splitAt len buf 
      put (buf', sock)
      return ret
    else do
      msg <- N.tryNuke ( recv sock recvLen )
      put (buf `BS.append` msg , sock)
      recvTake len

  where
    recvLen :: Int
    recvLen = 128

resolve :: String -> Maybe String -> IO [AddrInfo]
resolve host maybeServiceName = do
  -- let hints = defaultHints { addrFlags = [AI_ALL], addrSocketType = Stream }
  let hints = defaultHints {addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream} -- IPv4
  getAddrInfo (Just hints) (Just host) (Just ( M.fromMaybe "http" maybeServiceName ))

getAppArgs :: ExceptT AppError IO (String, Maybe String)
getAppArgs = do
  args <- lift Env.getArgs
  ErrUtil.hoistEither $ validArgs args
  where
    validArgs :: [String] -> Either AppError (String, Maybe String)
    validArgs [url] = Right (url, Nothing)
    validArgs [url, port] = Right (url, Just port)
    validArgs _ = Left $ AppError usageInfo
