module Coc.Component.Nav where

import Prelude

import Coc.AppM (class Navigate, navigate)
import Coc.Navigation as Navigation
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Message = Navigation.Message

type Slot p = forall query. H.Slot query Message p

type State = {}

type Path = String

data Action = Go Path MouseEvent


component :: forall q m
             . MonadAff m
             => Navigate m
             => H.Component HH.HTML q Unit Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: Unit -> State
  initialState _ = {}

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.button
      [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Go "/tasks/new") ]
      [ HH.text "+" ]

  handleAction :: Action → H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    Go path event -> do
      H.liftEffect $ event # toEvent # preventDefault
      -- Navigation.go path
      navigate path
