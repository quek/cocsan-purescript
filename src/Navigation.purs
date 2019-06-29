module Coc.Navigation where

import Prelude

import Effect.Class (class MonadEffect)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Query.HalogenM (HalogenM)
import Routing.PushState (makeInterface)

data Message = UrlChanged String

go :: forall m slots action state.
      MonadEffect m => String -> HalogenM state action slots Message m Unit
go path = do
  H.liftEffect do
    nav <- makeInterface
    nav.pushState (unsafeToForeign {}) path
  H.raise (UrlChanged path)
