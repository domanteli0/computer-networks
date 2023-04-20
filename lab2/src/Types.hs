module Types() where
-- module Types(MySocketError, MySocket(..)) where

-- import Control.Monad.Trans.Except
-- import Network.Socket
-- import Network.Socket.ByteString

-- type MySocketError = String

-- data MySocket a = MySocket { 
--     runSocket :: Socket -> ExceptT MySocketError IO a
--   , recvLen :: Int
--   }

-- recvMySocket :: 

-- instance Functor MySocket where
--   fmap f (MySocket runS recvLen) = 
--     MyScoket $ \sock -> do
--       msg <- recv sock recvLen

--       return $ f msg
    