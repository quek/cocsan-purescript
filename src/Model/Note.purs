module Coc.Model.Note where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model (class Find, class Model, find', findImpl)
import Coc.Model.Base (BaseData, BaseDoc, deleteRef, encode, insertRef)
import Coc.Model.DateTime (DateTime(..))
import Coc.Store (userNotes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Type.Proxy (Proxy(..))

type NoteBase x =
  { body :: String
  | x
  }

type NoteData = NoteBase (BaseData)

type Note = NoteBase (BaseDoc)

newtype GNote = GNote NoteData

derive instance genericNote :: Generic GNote _
instance showNote :: Show GNote where show = genericShow

find :: String -> Aff Note
find id = do
  { ref, data: (GNote noteData) } <- find' id
  pure $ insertRef ref noteData

update :: Note -> Aff Unit
update note = do
  now <- liftEffect nowDateTime
  let note' = deleteRef note
  let doc = encode $ GNote note' { updatedAt = DateTime now }
  Firestore.update doc note.ref

instance modelNote :: Model GNote where
  collection' _ = userNotes

instance findNote :: Find GNote where
  find' = findImpl (Proxy :: Proxy GNote)
