module Coc.Component.TaskNew where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
type Slot = H.Slot Query Void

data Query a = Dummy a

type State = {}

data Action = HandleSubmit TaskInput

component :: forall i message q. H.Component HH.HTML q i message Aff
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
    HH.div_
      [ HH.p_ [ HH.text "新規だよ" ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleSubmit)
      ]

  handleAction = case _ of
    HandleSubmit task -> do
      H.liftEffect $ log $ show task
      pure unit

  formComponent = F.component (const input) $ F.defaultSpec { render = renderForm, handleEvent = F.raiseResult }
    where
    renderForm st@{ form } =
      HH.div_
        [ HH.input
            [ HP.value $ F.getInput _name form
            , HP.placeholder "内容"
            , HE.onValueInput $ Just <<< F.set _name
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

