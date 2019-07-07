module Coc.Component.NoteEdit where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, MyRoute(..), DocumentPathId, logMessage, navigate)
import Coc.Component.EditorComponent as EditorComponent



import Data.Maybe (Maybe(..))
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
  initialState id = { id, body: "" }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "notes" ]
      [ HH.slot _ecitor unit EditorComponent.component unit (Just <<< HandleAceUpdate)
      , HH.button
          [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Save) ]
          [ HH.text "Save edit" ]
      ]

  handleAction = case _ of
    Initialize -> initialize
    Save event -> do
      navigate NoteIndex
    HandleAceUpdate (EditorComponent.TextChanged text) ->
       H.modify_ (_ { body = text })
    where
    initialize = do
      state <- H.get
      logMessage $ "NoteEdit 初期化 " <> state.id
