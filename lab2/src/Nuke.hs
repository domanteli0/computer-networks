module Nuke(NukeT(..), runNukeT) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

type NukeT s e a = ExceptT e (StateT s IO) a

runNukeT :: NukeT s e a -> s -> IO (Either e a)
runNukeT = evalStateT . runExceptT