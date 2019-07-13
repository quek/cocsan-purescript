module Coc.Component.Notes where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, MyRoute(..), logMessage, navigate)
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Note (GNote(..), Note)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff.Class (class MonadAff)
import Foreign.Generic (defaultOptions, genericDecode)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Web.UIEvent.MouseEvent (MouseEvent)

type Slot p = forall query. H.Slot query Void p

type State = { notes :: Array Note }

data Action
  = Initialize
  | GoToNoteNew MouseEvent
  | GoToNoteEdit Note MouseEvent

component :: forall query m
             . MonadAff m
             => LogMessages m
             => Navigate m
             => H.Component HH.HTML query Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }
  where
  initialState _ = { notes: [] }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "notes" ]
      [ HH.ul_
        do
          note <- state.notes
          pure $ HH.li
            [ HE.onClick (Just <<< (GoToNoteEdit note)) ]
            [ HH.text note.body, HH.text $ show note.updatedAt ]
      , HH.button
          [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< GoToNoteNew) ]
          [ HH.text "+" ]
      ]

  handleAction :: Action → H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> initialize
    GoToNoteNew _ -> do
      navigate NoteNew
    GoToNoteEdit note event -> do
      let id = Firestore.id note.ref
      navigate (NoteEdit id)
    where
    initialize = do
      logMessage "Notes 初期化"
      user <- H.liftEffect Auth.currentUser
      let uid = Auth.uid user
      firestore <- H.liftEffect Firestore.firestore
      let collection = firestore
                       # Firestore.collection "users"
                       # Firestore.doc uid
                       # Firestore.collection "notes"
      querySnapshot <- H.liftAff $ Firestore.get collection
      let
        opts = defaultOptions {unwrapSingleConstructors = true}
        notes = do
          doc <- Firestore.docs querySnapshot
          let documentData = Firestore.data' doc
          let maybeDoc = hush $ runExcept $ genericDecode opts documentData
          guard $ isJust maybeDoc
          let (GNote noteData) = unsafePartial fromJust maybeDoc
          pure $ { ref: Firestore.ref doc
                 , body: noteData.body
                 , createdAt: noteData.createdAt
                 , updatedAt: noteData.updatedAt
                 }
      H.modify_ (_ { notes = notes })
