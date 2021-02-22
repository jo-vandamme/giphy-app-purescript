module Giphy.Capability.Resource.Gifs where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Giphy.Data.Gifs (Gif)
import Giphy.Data.PaginatedArray (PaginatedArray)

type GifsParams
  = { offset :: Int
    , search :: String
    }

-- | This capability represents the ability to manage gifs in our system.
class
  Monad m <= ManageGifs m where
  getGifs :: GifsParams -> m (Maybe (PaginatedArray Gif))

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageGifsHalogenM :: ManageGifs m => ManageGifs (HalogenM st act slots msg m) where
  getGifs = lift <<< getGifs
