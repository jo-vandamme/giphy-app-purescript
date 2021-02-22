module Giphy.Api.Request (requestGifs) where

import Prelude
import Affjax (printError, get)
import Affjax.ResponseFormat as AXRF
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Giphy.Capability.LogMessages (class LogMessages, logError)
import Giphy.Capability.Now (class Now)
import Giphy.Capability.Resource.Gifs (GifsParams)
import Giphy.Data.Gifs (Gif, giphyResponseCodec)
import Giphy.Data.PaginatedArray (PaginatedArray)
import Giphy.Env (Env)

requestGifs :: forall m. MonadAsk Env m => Now m => LogMessages m => MonadAff m => GifsParams -> m (Maybe (PaginatedArray Gif))
requestGifs { search, offset } = do
  url <- getGiphySearchUrl search offset
  resp <- liftAff $ get AXRF.json url
  case resp of
    Left e -> (logError $ printError e) *> pure Nothing
    Right v -> decode giphyResponseCodec v.body

getGiphySearchUrl :: forall m. MonadAsk Env m => String -> Int -> m String
getGiphySearchUrl search offset = do
  apiKey <- asks _.apiKey
  pure $ searchEndpoint <> "?api_key=" <> apiKey <> "&q=" <> search <> "&offset=" <> show offset
  where
  searchEndpoint = "https://api.giphy.com/v1/gifs/search"

decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Json -> m (Maybe a)
decode codec json = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
