module Coc.Model.Task where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type Task =
  { name :: String
  , done :: Boolean
  }

newtype GTask = GTask Task

derive instance genericTask :: Generic GTask _
instance showTask :: Show GTask where show = genericShow

