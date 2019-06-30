module Coc.Component.Routing where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, Env, GlobalMessage(..), logMessage)
import Coc.Component.TaskNew as TaskNew
import Coc.Component.Tasks as Tasks
import Coc.Navigation as Navigation
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing (match)
import Routing.Match (Match, end, lit, root)

data Query a = ChangeRoute String a

type Message = Navigation.Message

data Action
  = HandleNav Message
  | Initialize

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

component :: forall o m
             .  MonadAff m
             => LogMessages m
             => Navigate m
             => MonadAsk Env m
             => H.Component HH.HTML Query Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { route: TaskIndex }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery
                                     , handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    }
  where
  -- initialState :: String -> State
  -- initialState path =
  --   { route: case match myRoute path of
  --        Right newRoute -> newRoute
  --        Left _ -> TaskIndex
  --   }

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
    Initialize -> do
      logMessage "初期化 Routing.purs"
      let
        f = do
          globalMessage <- asks _.globalMessage
          query <- H.liftAff $ AVar.take globalMessage
          case query of
            NavigateG page -> do
              updateRoute page
              pure unit
          f
      void $ H.fork f
    HandleNav (Navigation.UrlChanged path) -> do
      updateRoute path

  updateRoute path =
    case match myRoute path of
      Right newRoute -> do
        H.modify_ \st -> st { route = newRoute }
      Left e -> H.liftEffect $ log e
