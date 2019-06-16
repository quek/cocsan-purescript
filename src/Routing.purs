module Coc.Routing where

import Prelude

import Coc.Tasks as Tasks
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

data Query a = ChangeRoute String a

type State = { history :: Array String }

type ChildSlots = ( tasks :: Tasks.Slot Unit )
_tasks = SProxy :: SProxy "tasks"

component :: forall i o. H.Component HH.HTML Query i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

initialState :: forall i. i -> State
initialState _ = { history: [] }

render :: forall act. State -> H.ComponentHTML act ChildSlots Aff
render state =
  HH.div_
    [ HH.slot _tasks unit Tasks.component unit absurd
    , HH.p_ [ HH.text "history" ]
    , HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.history
    ]

handleQuery :: forall act o m a. Query a -> H.HalogenM State act ChildSlots o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ \st -> { history: st.history `snoc` msg }
    pure (Just a)
