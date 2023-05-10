{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module MySocket(
    fromHostnameTCP4
  , MySock
  , runMySock
  , untry
) where

import qualified Data.ByteString as BS
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Data.Maybe as M
import Control.Monad.Trans.Except
import qualified Network.Socket.ByteString as S
import Control.Exception (try, Exception, throwIO, SomeException)
import Control.Monad.Trans.State.Strict (get, put, StateT, evalStateT, execStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Error.Safe as ES

type MyState = ( BS.ByteString, S.Socket )

newtype SocketError = SocketError String
  deriving (Show)

instance Exception SocketError

-- | MySock encapsulates state, IO and possible failure
-- | State -> IO -> Either
type MySock s e a = StateT s (ExceptT e IO) a

runMySock :: MySock s e a -> s -> IO (Either e a)
runMySock m = run'
  where
    run' = runExceptT . evalStateT m

sendAll :: Exception SomeException => BS.ByteString -> MySock MyState SomeException ()
sendAll payload = do
  (_, sock) <- get
  liftIO $ untry $ trySomeE $ S.sendAll sock payload

trySomeE :: IO a -> IO (Either SomeException a)
trySomeE = try
-- sendAll :: S.Socket -> BS.ByteString -> MySock

-- https://stackoverflow.com/a/69365449
untry :: Exception e => IO (Either e a) -> IO a
untry action = action >>= either throwIO pure

-- data Sock 

-- data Sock = ExceptT SocketError IO ()

-- instance Functor MySocket where
--   fmap fun s = MySocket {
--       buf = buf s
--     , sock = sock s
--     , phantom =  fun $ phantom s
--   }

-- instance Applicative MySocket where
--   pure a = MySocket {
--       buf = ""
--     , sock = undefined
--     , phantom = a
--   }
--   (<*>) fun s = MySocket {
--           buf = ""
--         , sock = undefined
--         , phantom = phantom fun ( phantom s )
--       }

-- instance Monad MySocket where
--   (>>=) s fun = undefined


-- attempts to open a socket and connect to the host
fromHostnameTCP4
  :: String
  -> Maybe String
  -> MySock () SocketError S.Socket
fromHostnameTCP4 host maybeServiceName = do
  hostInfos <- try $ resolve host maybeServiceName
  hostInfos' <- lift hostInfos
  hostInfo <- head hostInfos
  let sockAddr = S.addrAddress hostInfo
  -- let sockAddr = SockAddrInte6 -- TODO: IPv6

  sock <- S.socket S.AF_INET S.Stream 0 -- IPv4
  -- sock <- S.socket AF_INET6 Stream 0
  S.connect sock sockAddr

  return $ return sock

-- sendAll :: MySocket -> Either 

resolve :: String -> Maybe String -> IO [S.AddrInfo]
resolve host maybeServiceName = do
  let hints = S.defaultHints {
    S.addrFlags = [S.AI_ADDRCONFIG], S.addrSocketType = S.Stream
  } -- IPv4

  S.getAddrInfo
    (Just hints)
    (Just host)
    (Just ( M.fromMaybe "http" maybeServiceName ))
