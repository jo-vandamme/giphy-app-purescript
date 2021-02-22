module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Giphy.AppM (runAppM)
import Giphy.Component.App as App
import Giphy.Env (LogLevel(..), Env)
import Giphy.Foreign.Dotenv (getApiKey)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  mbApiKey <- getApiKey
  case mbApiKey of
    Nothing -> log "Cannot read env variables."
    Just apiKey ->
      HA.runHalogenAff do
        body <- HA.awaitBody
        let
          environment :: Env
          environment = { logLevel: Dev, apiKey }

          rootComponent :: forall q i msg. H.Component HH.HTML q i msg Aff
          rootComponent = H.hoist (runAppM environment) App.component
        runUI rootComponent unit body
