module Giphy.Foreign.GridStyle (GridStyle, getGridStyle) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.HTML (HTMLElement)

type GridStyle
  = { gridAutoRows :: Number, gridRowGap :: Number }

foreign import getGridStyleImpl :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> HTMLElement -> Effect (Maybe GridStyle)

getGridStyle :: HTMLElement -> Effect (Maybe GridStyle)
getGridStyle = getGridStyleImpl Just Nothing
