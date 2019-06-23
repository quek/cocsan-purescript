module Coc.Firebase.Auth where

import Effect (Effect)

foreign import data User :: Type

foreign import currentUser :: Effect User

foreign import uid :: User -> String
