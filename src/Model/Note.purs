module Coc.Model.Note where

import Prelude

import Coc.Model.Base (BaseData, BaseDoc)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type NoteBase x =
  { body :: String
  | x
  }

type NoteData = NoteBase (BaseData)

type Note = NoteBase (BaseDoc)

newtype GNote = GNote NoteData

derive instance genericNote :: Generic GNote _
instance showNote :: Show GNote where show = genericShow

