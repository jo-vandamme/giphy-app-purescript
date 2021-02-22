module Giphy.Foreign.Dotenv (getApiKey) where

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import getApiKeyImpl :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe String)

getApiKey :: Effect (Maybe String)
getApiKey = getApiKeyImpl Just Nothing
