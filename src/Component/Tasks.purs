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
import Foreign.Generic (defaultOptions, genericDecode)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)

type Slot = H.Slot Query Void

data Query a = Void a

type State = { tasks :: Array Task
             }

data Action = Initialize | Done Task

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
initialState _ = { tasks: [] }

render :: State -> H.ComponentHTML Action () Aff
render state =
  HH.div [ HP.class_ $ ClassName "tasks" ]
    [ HH.ul_
      do
        task <- state.tasks
        pure $ HH.li
          [ HE.onClick \_ -> Just $ Done task ] 
          [ HH.text task.name ]
    , HH.div [ HP.class_ $ ClassName "yarn" ]
        [ HH.img [ HP.src $ assets "1.png" ]
      ]
    ]

handleAction :: forall o. Action → H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> initialize
  Done task -> do
    _ <- H.liftAff $ Firestore.delete task.ref
    initialize
  where
    initialize = do
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
          let maybeTask = hush $ runExcept $ genericDecode opts documentData
          guard $ isJust maybeTask
          let (GTask taskData) = unsafePartial fromJust maybeTask
          pure $ { ref: Firestore.ref doc, name: taskData.name, done: taskData.done }
      H.modify_ (_ { tasks = tasks})
