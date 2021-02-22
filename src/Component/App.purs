module Giphy.Component.App where

import Prelude
import Data.Array (length)
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Giphy.Capability.Resource.Gifs (class ManageGifs, getGifs)
import Giphy.Component.Gifs as Gifs
import Giphy.Component.SearchBar as SBar
import Giphy.Data.Gifs (Gif)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State
  = { gifs :: Array Gif
    , nextOffset :: Int
    , totalCount :: Int
    , search :: String
    }

data Action
  = Search String
  | Next

_searchBar :: SProxy "searchBar"
_searchBar = SProxy

_gifs :: SProxy "gifs"
_gifs = SProxy

type SearchBarSlot
  = H.Slot (Const Void) SBar.Message Unit

type GifsSlot
  = H.Slot (Const Void) Void Unit

type ChildSlots
  = ( searchBar :: SearchBarSlot
    , gifs :: GifsSlot
    )

component :: forall q i o m. ManageGifs m => MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { gifs: [], nextOffset: 0, totalCount: 0, search: "" }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render s =
  HH.div [ HP.class_ $ ClassName "app" ]
    [ HH.slot _searchBar unit SBar.component (SBar.DebounceTime (Milliseconds 250.0)) handleMessage
    , HH.slot _gifs unit Gifs.component s.gifs (const Nothing)
    , if canShowNext then HH.button [ HE.onClick \_ -> Just Next ] [ HH.text "Next" ] else HH.text ""
    ]
  where
  handleMessage (SBar.Searched search) = Just $ Search search

  canShowNext = length s.gifs > 0 && length s.gifs < s.totalCount

handleAction :: forall o m. ManageGifs m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Search search -> do
    H.modify_ _ { search = search }
    getGifs { search, offset: 0 } >>= traverse_ (\r -> updateGifs r.body r.total)
  Next -> do
    { gifs, search, nextOffset } <- H.get
    getGifs { search, offset: nextOffset } >>= traverse_ (\r -> updateGifs (gifs <> r.body) r.total)
  where
  updateGifs gifs totalCount = H.modify_ _ { gifs = gifs, totalCount = totalCount, nextOffset = length gifs }
