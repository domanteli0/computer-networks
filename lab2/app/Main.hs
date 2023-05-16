{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Error.Util as ErrUtil
import Control.Monad.Except
import qualified Control.Exception.Base as Exp
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

import qualified System.IO as IO
import qualified Args

data AppError =
    AppError String
  | AppUsageError String
  | AppSomeException Exp.SomeException
  deriving (Show)

instance Exp.Exception AppError where
  -- toException = undefined

type AppState = (BS.ByteString, Socket, AppArgs)

data AppArgs = AppArgs {
    followRedirects :: Bool
  , output :: IO.Handle
} deriving (Show)

defaultAppArgs = AppArgs False IO.stdout

crlf :: BS.ByteString
crlf = "\r\n"
crlf2 = "\r\n\r\n"
-- TODO:
-- - Chuncked https://stackoverflow.com/questions/19907628/transfer-encoding-chunked
-- - redirect (if flag is passed)
-- - save to file
-- - multiple same key headers: https://stackoverflow.com/questions/3241326/set-more-than-one-http-header-with-the-same-name


main :: IO ()
main = do
  sock <- mkSocket 0
  s <- N.runNukeT
    (N.finally doMain (\(_, a, h) -> do close a; hCloseIfNotStdout (output h)))
    ("", sock, defaultAppArgs)
  case s of
    Right _ -> putStrLn "DONE"
    Left e -> print e
  where
    hCloseIfNotStdout :: IO.Handle -> IO ()
    hCloseIfNotStdout h =
      if h == IO.stdout then ioDoNothing else IO.hClose h

    ioDoNothing :: IO ()
    ioDoNothing = do return ()

argMap :: (AppArgs -> AppArgs) -> N.NukeT AppState e ()
argMap argF = do
  (buf, sock, appArgs) <- get
  put (buf, sock, argF appArgs)

getArgs :: N.NukeT AppState e AppArgs
getArgs = do
  (_, _, args) <- get
  return args

doMain :: N.NukeT AppState AppError Socket
doMain = do
  rawArgs <- N.mapErr
    ( lift Args.getAppArgs )
    AppUsageError

  handle <- N.tryNuke ( case Args.outputRaw rawArgs of
    Just filepath -> liftIO $ IO.openBinaryFile filepath IO.WriteMode
    Nothing -> liftIO $ do return IO.stdout )

  argMap (\args -> 
    args { 
        output = handle
      , followRedirects = Args.followRedirectsRaw rawArgs
    })
  
  let url = Args.host rawArgs
  let maybePort = Args.port rawArgs

  let (host, path) = Utils.splitURL url
  hostInfo <- N.replaceErr
    ( head <$> N.tryNuke @Exp.SomeException ( resolve host maybePort ) )
    ( AppError ( "Could not resolve the hostname [" ++ host ++ "]" ) )
  let sockAddr = addrAddress hostInfo
  --     -- let sockAddr = SockAddrInte6 -- TODO: IPv6
  --     -- let port = PortNumber sockAddr
  --     -- let port = addr
  N.replaceErr ( do
    sock <- N.tryNuke @Exp.SomeException $ socket AF_INET Stream 0 -- IPv4
    _ <- N.tryNuke $ connect sock sockAddr
    put ("", sock, undefined) )
    (AppError "Failed to connect to host")
  (_, sock, _) <- get


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

  let hLen = H.getHContentLength $ H.headers responseHead
  if M.isJust hLen
    then do
      body <- N.mapErr
        ( recvTake (M.fromJust hLen) )
        AppSomeException
      liftIO $ print "BODY:"
      -- liftIO $ 
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

recvUntil :: BS.ByteString -> N.NukeT AppState Exp.SomeException BS.ByteString
recvUntil sub = do
  (buf, sock, args) <- get

  let (ret, buf') = BS.breakSubstring sub buf

  if not $ BS.null buf'
    then do
      put (BS.drop (BS.length sub) buf', sock, args)
      return ret
    else do
      msg <- N.tryNuke ( recv sock recvLen )
      put (buf `BS.append` msg , sock, args)
      recvUntil sub

  where
    recvLen :: Int
    recvLen = 128

recvTake :: Int -> N.NukeT AppState Exp.SomeException BS.ByteString
recvTake len = do
  (buf, sock, args) <- get
  let bufLen = BS.length buf
  if len <= bufLen
    then do
      let (ret, buf') = BS.splitAt len buf
      put (buf', sock, args)
      return ret
    else do
      msg <- N.tryNuke ( recv sock recvLen )
      put (buf `BS.append` msg , sock, args)
      recvTake len

  where
    recvLen :: Int
    recvLen = 128

resolve :: String -> Maybe String -> IO [AddrInfo]
resolve host maybeServiceName = do
  -- let hints = defaultHints { addrFlags = [AI_ALL], addrSocketType = Stream }
  let hints = defaultHints {addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream} -- IPv4
  getAddrInfo (Just hints) (Just host) (Just ( M.fromMaybe "http" maybeServiceName ))
