module Coc.Component.NoteEdit where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, DocumentPathId, MyRoute(..), logMessage, navigate)
import Coc.Component.EditorComponent as EditorComponent
import Coc.Model.Note (Note)
import Coc.Model.Note as Note
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Slot p = forall query. H.Slot query Void p

type State =
  { id :: DocumentPathId
  , note :: Maybe Note
  , body :: String
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
             => H.Component HH.HTML query DocumentPathId Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }
  where
  initialState id = { id, note: Nothing, body: "" }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    case state.note of
      Just note ->
        HH.div
          [ HP.class_ $ H.ClassName "notes" ]
          [ HH.slot _ecitor unit EditorComponent.component note.body (Just <<< HandleAceUpdate)
          , HH.button
              [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Save) ]
              [ HH.text "Save" ]
          ]
      Nothing -> HH.div_ []

  handleAction = case _ of
    Initialize -> do
      state <- H.get
      note <- H.liftAff $ Note.find state.id
      logMessage "NoteEdit Initialize"
      logMessage note.body
      H.modify_ (_ { note = Just note })
    Save event -> do
      navigate NoteIndex
    HandleAceUpdate (EditorComponent.TextChanged text) ->
       H.modify_ (_ { body = text })
