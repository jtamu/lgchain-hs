module Lgchain.Core.Agents where

class AgentNode a b where
  run :: a -> b -> IO b
