module Coc.Model.Note where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (BaseData, BaseDoc)
import Coc.Model.DateTime (DateTime(..))
import Coc.Store (userNotes)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromJust)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Partial.Unsafe (unsafePartial)
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
  let opts = defaultOptions {unwrapSingleConstructors = true}
  userNotes' <- liftEffect userNotes
  documentSnapshot <- ((Firestore.get $
    Firestore.doc id $
    userNotes') :: (Aff Firestore.DocumentSnapshot))
  let maybeDoc = hush $ runExcept $ genericDecode opts $ Firestore.data' documentSnapshot
  let (GNote noteData) = unsafePartial fromJust maybeDoc
  pure $ Record.insert _ref (Firestore.ref documentSnapshot) noteData

update :: Note -> Aff Unit
update note = do
  now <- liftEffect nowDateTime
  let note' = Record.delete _ref note
  let doc = genericEncode defaultOptions (
        GNote (note' { updatedAt = DateTime now })
        )
  Firestore.update doc note.ref

_ref = SProxy :: SProxy "ref"
