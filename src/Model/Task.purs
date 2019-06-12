module Coc.Model.Task where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Task = Task
  { name :: String
  , done :: Boolean
  }

derive instance genericTask :: Generic Task _
instance showTask :: Show Task where show = genericShow

