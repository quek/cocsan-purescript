module Coc.Component.Nav where

import Prelude

import Coc.Navigation (Message, go)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot = H.Slot Query Message

data Query a = Dummy a

type State = {}

type Path = String

data Action = Go Path MouseEvent


component :: forall i q. H.Component HH.HTML q i Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = {}

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.button
    [ HP.class_ $ H.ClassName "add-button", HE.onClick (Just <<< Go "/tasks/new") ]
    [ HH.text "+" ]

handleAction :: Action → H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  Go path event -> do
    H.liftEffect $ event # toEvent # preventDefault
    go path
