module Coc.Component.Notes where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, MyRoute(..), logMessage, navigate)
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (decode)
import Coc.Model.Note (GNote(..), Note)
import Coc.Store.Collection as Collection
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
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
            [ HH.text note.body ]
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
                       # Firestore.collection Collection.users
                       # Firestore.doc uid
                       # Firestore.collection Collection.notes
      querySnapshot <- H.liftAff $ Firestore.get collection
      let
        notes = do
          doc <- Firestore.docs querySnapshot
          let documentData = Firestore.data' doc
          let (GNote noteData) = decode documentData
          pure $ Record.insert _ref (Firestore.ref doc) noteData
      H.modify_ (_ { notes = notes })

_ref = SProxy :: SProxy "ref"
