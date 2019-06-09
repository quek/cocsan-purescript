module Coc.App (component) where

import Prelude

import Assets (assets)
import Coc.Component.List as CList
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- import Coc.Firestore as Firestore

type State = { enabled :: Boolean }

data Action = Toggle | Initialize

type ChildSlots = ( list :: CList.Slot Unit )

_list = SProxy :: SProxy "list"

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div_
      [ HH.slot _list unit CList.component unit absurd
      , HH.button
          [ HP.title label
          , HE.onClick \_ -> Just Toggle
          ]
          [ HH.text label ]
      , HH.p_ [ HH.text "ねこ" ]
      , HH.img [ HP.src $ assets "1.png" ]
      ]

handleAction :: forall o. Action → H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }
  Initialize ->
    -- Firestore.collection "tasks"
    H.liftEffect $ log "初期化です！！！"
