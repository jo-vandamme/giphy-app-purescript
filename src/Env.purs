module Giphy.Env where

import Prelude

type Env
  = { logLevel :: LogLevel
    , apiKey :: String
    }

data LogLevel
  = Dev
  | Prod

derive instance eqLogLevel :: Eq LogLevel

derive instance ordLogLevel :: Ord LogLevel
