module Coc.Component.TaskNew where

import Prelude

import Coc.AppM (class Navigate, MyRoute(..), navigate)
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Coc.Model.Task (GTask(..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericEncode)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (focus)
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
type Slot p = forall query. H.Slot query Void p

type State = {}

data Action = HandleSubmit TaskInput

component :: forall q m
             . MonadAff m
             => Navigate m
             => H.Component HH.HTML q Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
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
      firestore <- H.liftEffect Firestore.firestore
      _ <- firestore
           # Firestore.collection "users"
           # Firestore.doc uid
           # Firestore.collection "tasks"
           # Firestore.add d
           # H.liftAff
      H.liftEffect $ log $ show task
      navigate TaskIndex

  formComponent =
    F.component (const input) $ F.defaultSpec
      { render = renderForm
      , initialize = Just InitializeForm
      , handleEvent = F.raiseResult
      , handleAction = handleFormAction
      }
    where
    inputRef = H.RefLabel "input"
    renderForm st@{ form } =
      HH.div_
        [ HH.input
            [ HP.value $ F.getInput _name form
            , HP.placeholder "内容"
            , HE.onValueInput $ Just <<< F.set _name
            , HE.onKeyUp (\event -> Just (F.injAction (KeyUp event)))
            , HP.ref inputRef
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
      InitializeForm -> do
        H.getHTMLElementRef inputRef >>= traverse_ \element -> do
          H.liftEffect $ focus element
      KeyUp event -> do
        case key event of
          "Enter" ->
            F.handleAction handleFormAction F.raiseResult F.submit
          _ ->
            pure unit

data MyFormAction = InitializeForm | KeyUp KeyboardEvent
