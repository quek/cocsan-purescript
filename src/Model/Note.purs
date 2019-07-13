module Coc.Model.Note where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (BaseData, BaseDoc)
import Coc.Store (userNotes)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromJust)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Generic (defaultOptions, genericDecode)
import Partial.Unsafe (unsafePartial)

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
  pure $ { ref: Firestore.ref documentSnapshot
         , body: noteData.body
         , createdAt: noteData.createdAt
         , updatedAt: noteData.updatedAt
         }

