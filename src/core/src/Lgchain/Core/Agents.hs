module Lgchain.Core.Agents where

import Lgchain.Core.Clients (ExceptIO)

class AgentNode a b where
  run :: a -> b -> ExceptIO b
