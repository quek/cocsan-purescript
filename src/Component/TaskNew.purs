module Coc.Component.TaskNew where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot Query Void

data Query a = Dummy a

type State = {}

data Action = Foo

component :: forall i message q. H.Component HH.HTML q i message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = {}

render :: forall action m. State -> H.ComponentHTML action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "新規だよ" ]
    ]

handleAction :: forall message. Action → H.HalogenM State Action () message Aff Unit
handleAction = case _ of
  Foo ->
    pure unit
