module Coc.Model.Note where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (BaseData, BaseDoc, decode, deleteRef, encode, insertRef)
import Coc.Model.DateTime (DateTime(..))
import Coc.Store (userNotes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Record as Record

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
  userNotes' <- liftEffect userNotes
  documentSnapshot <- (Firestore.get $
    Firestore.doc id
    userNotes') :: Aff Firestore.DocumentSnapshot
  let (GNote noteData) = decode $ Firestore.data' documentSnapshot
  pure $ insertRef (Firestore.ref documentSnapshot) noteData

update :: Note -> Aff Unit
update note = do
  now <- liftEffect nowDateTime
  let note' = deleteRef note
  let doc = encode $ GNote note' { updatedAt = DateTime now }
  Firestore.update doc note.ref
