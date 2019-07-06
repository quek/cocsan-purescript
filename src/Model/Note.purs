module Coc.Model.Note where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type NoteX x =
  { body :: String
  | x
  }

type NoteData = NoteX ()

type Note = NoteX (ref :: Firestore.DocumentReference)

newtype GNote = GNote NoteData

derive instance genericNote :: Generic GNote _
instance showNote :: Show GNote where show = genericShow

