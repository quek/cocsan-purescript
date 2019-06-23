module Coc.Model.Task where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type TaskX x =
  { name :: String
  , done :: Boolean
  | x
  }

type TaskData = TaskX ()

type Task = TaskX (ref :: Firestore.DocumentReference)

newtype GTask = GTask TaskData

derive instance genericTask :: Generic GTask _
instance showTask :: Show GTask where show = genericShow

