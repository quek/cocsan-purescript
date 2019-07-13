module Coc.Store where

import Prelude

import Coc.Firebase.Auth as Auth
import Effect (Effect)

uid :: Effect String
uid = do
  user <- Auth.currentUser
  pure $ Auth.uid user
