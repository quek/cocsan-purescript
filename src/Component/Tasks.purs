module Coc.Component.Tasks where

import Prelude

import Assets (assets)
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Task (GTask(..), Task)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff (Aff)
import Effect.Console (log, logShow)
import Foreign (F, unsafeToForeign)
import Foreign.Generic (defaultOptions, genericDecode)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Routing.PushState (makeInterface)

type Slot = H.Slot Query Void

data Query a = IsOn (Boolean -> a)

type State = { enabled :: Boolean
             , tasks :: Array Task
             }

data Action = Toggle | Initialize

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

render :: State -> H.ComponentHTML Action () Aff
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div_
      [ HH.ul_
        do
          task <- state.tasks
          pure $ HH.li_ [ HH.text task.name ]
      , HH.div [ HP.class_ $ ClassName "yarn" ]
          [ HH.img [ HP.src $ assets "1.png" ]
        ]
      ]

handleAction :: forall o. Action → H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Toggle -> do
    state <- H.get
    H.liftEffect do
      nav <- makeInterface
      nav.pushState (unsafeToForeign {}) (if state.enabled then "/foo" else "/bar")
    H.modify_ \st -> st { enabled = not st.enabled }
  Initialize -> do
    H.liftEffect $ log "初期化です！！！"
    user <- H.liftEffect Auth.currentUser
    let uid = Auth.uid user
    let collection = Firestore.subCollection "tasks" $
      Firestore.doc uid $
      Firestore.collection "users"
    querySnapshot <- H.liftAff $ Firestore.get collection
    H.liftEffect $ logShow $ Firestore.size querySnapshot
    let
      opts = defaultOptions {unwrapSingleConstructors = true}
      tasks = do
        doc <- Firestore.docs querySnapshot
        let documentData = Firestore.documentData doc
        let maybeTask = hush $ runExcept $ (genericDecode opts documentData) :: F GTask
        guard $ isJust maybeTask
        let (GTask rawTask) = unsafePartial fromJust maybeTask
        pure $ rawTask
    H.modify_ (_ { tasks = tasks})

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  IsOn k -> do
    state <- H.get
    pure (Just (k state.enabled))
