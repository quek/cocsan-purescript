module Coc.Component.Tasks where

import Prelude

import Assets (assets)
import Coc.AppM (class Behaviour, class Navigate, logMessage, startLoading, stopLoading)
import Coc.Component.Nav as Nav
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (decode)
import Coc.Model.Task (GTask(..), Task)
import Coc.Store (userTasks)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

type Slot = H.Slot Query Void

data Query a = Void a

type State = { tasks :: Array Task
             }

data Action = Initialize | Done Task

type ChildSlots =
  ( nav :: Nav.Slot Unit
  )

_nav = SProxy :: SProxy "nav"


component
  :: forall q m
     . MonadAff m
     => Behaviour m
     => Navigate m
     => H.Component HH.HTML q Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }
  where
  initialState _ = { tasks: [] }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div [ HP.class_ $ ClassName "tasks" ]
      [ HH.ul_
        do
          task <- state.tasks
          pure $ HH.li
            [ HE.onClick \_ -> Just $ Done task ]
            [ HH.text task.name ]
      , HH.slot _nav unit Nav.component unit absurd
      , HH.div [ HP.class_ $ ClassName "yarn" ]
          [ HH.img [ HP.src $ assets "1.png" ]
        ]
      ]

  handleAction :: Action → H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> initialize
    Done task -> do
      _ <- H.liftAff $ Firestore.delete task.ref
      initialize
    where
      initialize = do
        startLoading
        logMessage "初期化です"
        user <- H.liftEffect Auth.currentUser
        let uid = Auth.uid user
        firestore <- H.liftEffect Firestore.firestore
        userTasks' <- H.liftEffect userTasks
        querySnapshot <- H.liftAff $ Firestore.get userTasks'
        H.liftEffect $ logShow $ Firestore.size querySnapshot
        let
          tasks = do
            doc <- Firestore.docs querySnapshot
            let documentData = Firestore.data' doc
            let (GTask taskData) = decode documentData
            pure $ Record.insert _ref (Firestore.ref doc) taskData
        H.modify_ (_ { tasks = tasks})
        stopLoading

_ref = SProxy :: SProxy "ref"
