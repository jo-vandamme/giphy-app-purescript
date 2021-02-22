module Giphy.AppM where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Type.Equality (class TypeEquals, from)
import Giphy.Api.Request (requestGifs)
import Giphy.Capability.LogMessages (class LogMessages)
import Giphy.Capability.Now (class Now)
import Giphy.Capability.Resource.Gifs (class ManageGifs)
import Giphy.Data.Log as Log
import Giphy.Env (Env, LogLevel(..))

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance manageGifsAppM :: ManageGifs AppM where
  getGifs = requestGifs
