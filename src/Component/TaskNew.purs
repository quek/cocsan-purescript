module Coc.Component.TaskNew where

import Prelude

import Coc.Component.Nav as Nav
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Task (GTask(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericEncode)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)

-----------------------------------------------------------------------------
-- form
type TaskInput = { name :: String }

data NameError = Required

newtype TaskForm r f = TaskForm (
  r
  --          error     input  output
  ( name :: f NameError String String )
  )

derive instance nettypeTaskForm :: Newtype (TaskForm r f) _

input :: forall m. Monad m => F.Input' TaskForm m
input =
  { initialInputs: Nothing      -- same as: Just (F.wrapInputFields { name: "" })
  , validators: TaskForm
      { name: F.hoistFnE_ \str -> case str of
           "" -> Left Required
           _ -> Right str
      }
  }

-----------------------------------------------------------------------------
-- component
type Slot = H.Slot Query Nav.Message

data Query a = Dummy a

type State = {}

data Action = HandleSubmit TaskInput

component :: forall i q. H.Component HH.HTML q i Nav.Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ = {}

  render state =
    HH.div
      [ HP.class_ $ H.ClassName "TaskNew" ]
      [ HH.slot F._formless unit formComponent unit (Just <<< HandleSubmit)
      ]

  handleAction = case _ of
    HandleSubmit task -> do
      user <- H.liftEffect Auth.currentUser
      let uid = Auth.uid user
      H.liftEffect $ log $ uid
      let d = genericEncode defaultOptions (GTask { name: task.name, done: false })
      _ <- H.liftAff $
        Firestore.add d $
        Firestore.subCollection "tasks" $
        Firestore.doc uid $
        Firestore.collection "users"
      H.liftEffect $ log $ show task
      H.raise (Nav.Changed "/tasks")

  formComponent =
    F.component (const input) $ F.defaultSpec
      { render = renderForm
      , handleEvent = F.raiseResult
      , handleAction = handleFormAction
      }
    where
    renderForm st@{ form } =
      HH.div_
        [ HH.input
            [ HP.value $ F.getInput _name form
            , HP.placeholder "内容"
            , HE.onValueInput $ Just <<< F.set _name
            , HE.onKeyUp (\event -> Just (F.injAction (KeyUp event)))
            ]
        , HH.text case F.getError _name form of
            Nothing -> ""
            Just Required -> "必須です。"
        , HH.button
            [ HE.onClick \_ -> Just F.submit ]
            [ HH.text "OK" ]
        ]
      where
      _name = SProxy :: SProxy "name"
    handleFormAction = case _ of
      KeyUp event -> do
        H.liftEffect $ log $ key event
        case key event of
          "Enter" ->
            F.handleAction handleFormAction F.raiseResult F.submit
          _ ->
            pure unit

data MyFormAction = KeyUp KeyboardEvent
