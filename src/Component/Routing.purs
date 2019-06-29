module Coc.Component.Routing where

import Prelude

import Coc.Component.TaskNew as TaskNew
import Coc.Component.Tasks as Tasks
import Coc.Navigation as Navigation
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing (match)
import Routing.Match (Match, end, lit, root)
import Effect.Aff.Class (class MonadAff)

data Query a = ChangeRoute String a

type Message = Navigation.Message

data Action = HandleNav Message

type ChildSlots =
  ( tasks :: Tasks.Slot Unit
  , taskNew :: TaskNew.Slot Unit
  )

_tasks = SProxy :: SProxy "tasks"
_taskNew = SProxy :: SProxy "taskNew"
_nav = SProxy :: SProxy "nav"

data MyRoute
  = TaskIndex
  | TaskNew

myRoute :: Match MyRoute
myRoute = root *> oneOf
  [ TaskNew <$ (lit "tasks" <* lit "new" <* end)
  , TaskIndex <$  (lit "tasks" <* end)
  ]

type State =
  { route :: MyRoute }

component :: forall o m. MonadAff m => H.Component HH.HTML Query String o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery, handleAction = handleAction }
    }
  where
  initialState :: String -> State
  initialState path =
    { route: case match myRoute path of
         Right newRoute -> newRoute
         Left _ -> TaskIndex
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "body" ]
      [ case state.route of
          TaskIndex ->
            HH.slot _tasks unit Tasks.component unit (Just <<< HandleNav)
          TaskNew ->
            HH.slot _taskNew unit TaskNew.component unit (Just <<< HandleNav)
      ]

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    ChangeRoute path a -> do
      updateRoute path
      pure (Just a)

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleNav (Navigation.UrlChanged path) -> do
      updateRoute path

  updateRoute path =
    case match myRoute path of
      Right newRoute -> do
        H.modify_ \st -> st { route = newRoute }
      Left e -> H.liftEffect $ log e
