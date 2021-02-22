module Giphy.Capability.Now where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime)
import Halogen (HalogenM)

class
  Monad m <= Now m where
  nowDateTime :: m DateTime

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance nowHalogenM :: Now m => Now (HalogenM st act slots msg m) where
  nowDateTime = lift nowDateTime
