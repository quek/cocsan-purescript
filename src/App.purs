module Coc.App (component) where

import Prelude

import Assets (assets)
import Coc.Component.List as CList
import Coc.Firestore as Firestore
import Coc.Model.Task (Task)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log, logShow)
import Foreign (F)
import Foreign.Generic (defaultOptions, genericDecode)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)

type State = { enabled :: Boolean
             , tasks :: Array Task
             }

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
initialState _ = { enabled: false, tasks: [] }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div_
      [ HH.slot _list unit CList.component unit absurd
      , HH.ul_
        do
          task <- state.tasks
          pure $ HH.li_ [ HH.text $ show task ]
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
  Initialize -> do
    H.liftEffect $ log "初期化です！！！"
    H.liftEffect $ log $ Firestore.id $ Firestore.collection "tasks"
    querySnapshot <- H.liftAff $ Firestore.get $ Firestore.collection "tasks"
    H.liftEffect $ logShow $ Firestore.size querySnapshot
    let
      opts = defaultOptions {unwrapSingleConstructors = true}
      tasks = do
        doc <- Firestore.docs querySnapshot
        let documentData = Firestore.documentData doc
        let maybeTask = hush $ runExcept $ (genericDecode opts documentData) :: F Task
        guard $ isJust maybeTask
        pure $ unsafePartial fromJust maybeTask
    H.modify_ (_ { tasks = tasks})
