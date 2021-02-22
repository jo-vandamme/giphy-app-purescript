module Giphy.Component.Gifs where

import Prelude
import CSS (fromString, height, key, px, width)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (fixed, toStringWith)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Math (ceil)
import Giphy.Data.Gifs (Gif)
import Giphy.Foreign.GridStyle (GridStyle, getGridStyle)

type State
  = { gifs :: Array Gif
    , gridStyle :: Maybe GridStyle
    }

type Input
  = Array Gif

data Action
  = ReceiveGifs (Array Gif)
  | Initialize

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: \gifs -> { gifs: gifs, gridStyle: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = \gifs -> Just $ ReceiveGifs gifs
              , initialize = Just Initialize
              }
    }

render :: forall a m. State -> H.ComponentHTML a () m
render s =
  HH.div
    [ HP.class_ $ ClassName "gifs"
    , HP.ref (H.RefLabel "grid")
    ]
    $ maybe ([ HH.text "loading" ]) renderGifs s.gridStyle
  where
  renderGifs grid = map (renderGif grid) s.gifs

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  ReceiveGifs gifs -> H.modify_ _ { gifs = gifs }
  Initialize -> H.getHTMLElementRef (H.RefLabel "grid") >>= traverse_ setEl
    where
    setEl el = (H.liftEffect $ getGridStyle el) >>= \s -> H.modify_ _ { gridStyle = s }

renderGif :: forall w i. GridStyle -> Gif -> HH.HTML w i
renderGif grid gif =
  HH.div
    [ HP.class_ $ ClassName "gif"
    , CSS.style do
        width $ px gif.fixedWidth.width
        height $ px gif.fixedWidth.height
        key (fromString "grid-row-end") ("span " <> toStringWith (fixed 0) rowSpan)
    ]
    [ HH.img [ HP.src gif.fixedWidth.url, HP.alt gif.title ] ]
  where
  rowSpan = ceil $ (gif.fixedWidth.height + grid.gridRowGap) / (grid.gridAutoRows + grid.gridRowGap)
