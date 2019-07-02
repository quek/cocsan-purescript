module Coc.Component.Routing where

import Prelude

import Coc.AppM (class LogMessages, class Navigate, Env, GlobalMessage(..), MyRoute(..), logMessage, routeToPath)
import Coc.Component.TaskNew as TaskNew
import Coc.Component.Tasks as Tasks
import Coc.Component.Notes as Notes
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing (match)
import Routing.Match (Match, end, lit, root)
import Web.HTML (window)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location)

data Query a = ChangeRoute String a

data Action = Initialize

type ChildSlots =
  ( tasks :: Tasks.Slot Unit
  , taskNew :: TaskNew.Slot Unit
  , notes :: Notes.Slot Unit
  )

_tasks = SProxy :: SProxy "tasks"
_taskNew = SProxy :: SProxy "taskNew"
_notes = SProxy :: SProxy "notes"

myRoute :: Match MyRoute
myRoute = root *> oneOf
  [ TaskNew <$ (lit "tasks" <* lit "new" <* end)
  , TaskIndex <$ (lit "tasks" <* end)
  , NoteIndex <$ (lit "notes" <* end)
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
  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ $ H.ClassName "body" ]
      [ case state.route of
          TaskIndex ->
            HH.slot _tasks unit Tasks.component unit absurd
          TaskNew ->
            HH.slot _taskNew unit TaskNew.component unit absurd
          NoteIndex ->
            HH.slot _notes unit Notes.component unit absurd
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
      path <- H.liftEffect $ window >>= location >>= pathname
      updateRoute path
      void $ H.fork globalMessageLoop

  globalMessageLoop = do
    globalMessage <- asks _.globalMessage
    query <- H.liftAff $ AVar.take globalMessage
    case query of
      NavigateG route -> do
        pushState route
        H.modify_ \st -> st { route = route }
        pure unit
    globalMessageLoop

  pushState route = do
    pushStateInterface <- asks _.pushStateInterface
    H.liftEffect $ pushStateInterface.pushState (unsafeToForeign {}) $ routeToPath route

  updateRoute path = do
    case match myRoute path of
      Right newRoute -> do
        H.modify_ \st -> st { route = newRoute }
      Left e -> H.liftEffect $ log e
