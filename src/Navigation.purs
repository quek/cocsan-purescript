module Coc.Navigation where

import Prelude

import Foreign (unsafeToForeign)
import Halogen as H
import Routing.PushState (makeInterface)

data Message = UrlChanged String

go path = do
  H.liftEffect do
    nav <- makeInterface
    nav.pushState (unsafeToForeign {}) path
  H.raise (UrlChanged path)
