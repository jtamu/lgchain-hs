module Lgchain.Core.Agents where

import Control.Monad.IO.Class (MonadIO)

class (MonadIO m) => AgentNode m a b where
  run :: a -> b -> m b
