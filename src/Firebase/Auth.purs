module Coc.Firebase.Auth where

import Prelude

foreign import data User :: Type

foreign import currentUser :: Effect (Maybe User)

