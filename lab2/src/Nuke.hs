module Nuke (
    NukeT(..)
  , runNukeT
  , throwNuke
  , mapErr
  , replaceErr
  , onFailDo
  , finally
  , tryNuke
  , tryExc
  , maybeNukeT
) where

import Control.Error.Util
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import qualified Control.Exception as EX

type IOExceptT e a = ExceptT e IO a
type NukeT s e a = StateT s (ExceptT e IO) a

runNukeT :: NukeT s e a -> s -> IO (Either e a)
runNukeT m = runExceptT . evalStateT m

throwNuke :: e -> NukeT s e a
throwNuke e = StateT $ do return $ throwE e

tryExc :: EX.Exception e => IO a -> ExceptT e IO a
tryExc = ExceptT . EX.try

tryNuke :: EX.Exception e => IO a -> NukeT s e a
tryNuke a = StateT (\s -> do
    a' <- tryExc a
    return (a', s)
  )

onFailDo :: NukeT s e a -> (e -> IO ()) -> NukeT s e a
onFailDo n io = do
  s <- get
  val <- liftIO $ runExceptT $ runStateT n s
  case val of
    Right val'' -> StateT $ do return $ hoistEither $ Right val''
    Left e -> do
      lift $ lift $ io e
      StateT $ do return $ hoistEither $ Left e


finally :: NukeT s e a -> (s -> IO ()) -> NukeT s e a
finally n io = do
  s <- get
  val' <- liftIO $ runExceptT $ runStateT n s
  liftIO $ io s
  case val' of
    Right val'' -> StateT $ do return $ hoistEither $ Right val''
    Left  e     -> StateT $ do return $ hoistEither $ Left e


mapErr :: NukeT s e a -> (e -> e') -> NukeT s e' a
mapErr n emap = do
  s <- get
  val <- liftIO $ runExceptT $ runStateT n s
  case val of
    Left e -> StateT $ do return $ hoistEither $ Left $ emap e 
    Right val'' -> StateT $ do return $ hoistEither $ Right val''

replaceErr :: NukeT s e a -> e' -> NukeT s e' a
replaceErr n e' = mapErr n (const e')

maybeNukeT :: Maybe a -> e -> NukeT s e a
maybeNukeT m e = StateT $ do
  case m of
    Just a -> \s -> ExceptT $ do return $ Right (a, s)
    Nothing -> \_ -> throwE e


-- onFail n e = do
--   n' <- n
--   return ()