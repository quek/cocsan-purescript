module Coc.Component.NoteNew where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, MyRoute(..), logMessage, navigate)
import Coc.Store.Collection as Collection
import Coc.Component.EditorComponent as EditorComponent
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Note (GNote(..))
import Coc.Model.DateTime (DateTime(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import Foreign.Generic (defaultOptions, genericEncode)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Slot p = forall query. H.Slot query Void p

type State =
  { body :: String
  }

data Action
  = Initialize
  | Save MouseEvent
  | HandleAceUpdate EditorComponent.Output

type ChildSlots =
  ( ecitor :: EditorComponent.Slot Unit
  )

_ecitor = SProxy :: SProxy "ecitor"


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
  initialState _ = { body: "" }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "notes" ]
      [ HH.slot _ecitor unit EditorComponent.component "" (Just <<< HandleAceUpdate)
      , HH.button
          [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Save) ]
          [ HH.text "Save" ]
      ]

  handleAction = case _ of
    Initialize -> initialize
    Save event -> do
      user <- H.liftEffect Auth.currentUser
      let uid = Auth.uid user
      state <- H.get
      now <- H.liftEffect nowDateTime
      let doc = genericEncode defaultOptions (
        GNote { body: state.body
              , createdAt: DateTime now
              , updatedAt: DateTime now
              }
        )
      firestore <- H.liftEffect Firestore.firestore
      _ <- firestore
        # Firestore.collection Collection.users
        # Firestore.doc uid
        # Firestore.collection Collection.notes
        # Firestore.add doc
        # H.liftAff
      navigate NoteIndex
    HandleAceUpdate (EditorComponent.TextChanged text) ->
       H.modify_ (_ { body = text })
    where
    initialize = do
      logMessage "NoteNew 初期化"
