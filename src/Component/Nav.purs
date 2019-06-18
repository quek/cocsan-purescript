module Coc.Component.Nav where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.PushState (makeInterface)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot = H.Slot Query Message

data Query a = Dummy a

data Message = Changed String

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
  HH.ul_
    [ HH.li_ [ HH.a [ HP.href "/foo", HE.onClick (Just <<< (Go "/foo")) ] [ HH.text "foo" ] ]
    , HH.li_ [ HH.a [ HP.href "/bar", HE.onClick (Just <<< (Go "/bar")) ] [ HH.text "bar" ] ]
    ]

handleAction :: Action → H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  Go path event -> do
    H.liftEffect do
      event # toEvent # preventDefault
      nav <- makeInterface
      nav.pushState (unsafeToForeign {}) path
    H.raise (Changed path)

