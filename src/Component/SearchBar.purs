module Giphy.Component.SearchBar (component, Message(..), Input(..)) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State
  = { search :: String
    , debounceTime :: Milliseconds
    , debounceRef :: Maybe (Ref (Maybe Debouncer))
    }

data Action
  = Search String
  | SendSearch
  | Initialize

data Message
  = Searched String

data Input
  = DebounceTime Milliseconds

type Debouncer
  = { var :: AVar Unit
    , fiber :: Fiber Unit
    }

component :: forall q m. MonadAff m => H.Component HH.HTML q Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

initialState :: Input -> State
initialState (DebounceTime dt) =
  { search: ""
  , debounceTime: dt
  , debounceRef: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div [ HP.class_ $ ClassName "search-bar" ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Search..."
        , HP.value s.search
        , HE.onValueInput $ Just <<< Search
        ]
    , HH.button [ HE.onClick \_ -> Just SendSearch ] [ HH.text "Search" ]
    ]

handleAction ∷ forall m. MonadAff m => Action → H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Initialize -> do
    ref <- H.liftEffect $ Ref.new Nothing
    H.modify_ _ { debounceRef = Just ref }
  Search str -> do
    s <- H.get
    ref <- H.liftEffect $ map join $ traverse Ref.read s.debounceRef
    H.modify_ _ { search = str }
    case ref of
      Nothing -> do
        var <- H.liftAff AVar.empty
        fiber <-
          H.liftAff
            $ forkAff do
                delay s.debounceTime
                AVar.put unit var
        void
          $ H.fork do
              H.liftAff $ AVar.take var
              H.liftEffect $ traverse_ (Ref.write Nothing) s.debounceRef
              handleAction SendSearch
        H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) s.debounceRef
      Just debouncer@{ var } -> do
        H.liftAff $ killFiber (error "Timeout") debouncer.fiber
        fiber <-
          H.liftAff
            $ forkAff do
                delay s.debounceTime
                AVar.put unit var
        H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) s.debounceRef
  SendSearch -> do
    s <- H.get
    when (not $ null s.search) do
      H.raise $ Searched s.search
