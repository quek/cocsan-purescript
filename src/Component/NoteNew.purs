module Coc.Component.NoteNew where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, logMessage)
import Coc.Model.Note (Note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Slot p = forall query. H.Slot query Void p

type State = { note :: Maybe Note }

data Action = Initialize | Xxx MouseEvent

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
  initialState _ = { note: Nothing }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "notes" ]
      [ HH.p_ [ HH.text "フォーム" ]
      , HH.button
          [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Xxx) ]
          [ HH.text "+" ]
      ]

  handleAction :: Action → H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> initialize
    Xxx event -> do
      pure unit
    where
    initialize = do
      logMessage "NoteNew 初期化"
