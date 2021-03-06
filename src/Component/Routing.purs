module Coc.Component.Routing where

import Prelude

import Coc.AppM (class Behaviour, class Navigate, DocumentPathId, Env, GlobalMessage(..), MyRoute(..), logMessage, routeToPath)
import Coc.Component.NoteEdit as NoteEdit
import Coc.Component.NoteNew as NoteNew
import Coc.Component.Notes as Notes
import Coc.Component.TaskNew as TaskNew
import Coc.Component.Tasks as Tasks
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
import Routing.Match (Match, end, lit, root, str)
import Web.HTML (window)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location)

data Query a = ChangeRoute String a

data Action = Initialize

type ChildSlots =
  ( tasks :: Tasks.Slot Unit
  , taskNew :: TaskNew.Slot Unit
  , notes :: Notes.Slot Unit
  , noteNew :: NoteNew.Slot Unit
  , noteEdit :: NoteEdit.Slot DocumentPathId
  )

_tasks = SProxy :: SProxy "tasks"
_taskNew = SProxy :: SProxy "taskNew"
_notes = SProxy :: SProxy "notes"
_noteNew = SProxy :: SProxy "noteNew"
_noteEdit = SProxy :: SProxy "noteEdit"

myRoute :: Match MyRoute
myRoute = root *> oneOf
  [ TaskNew <$ (lit "tasks" <* lit "new" <* end)
  , TaskIndex <$ (lit "tasks" <* end)
  , NoteIndex <$ (lit "notes" <* end)
  , NoteNew <$ (lit "notes" <* lit "new" <* end)
  , NoteEdit <$> (lit "notes" *> str <* lit "edit" <* end)
  ]

type State =
  { route :: Maybe MyRoute
  , loading :: Int
  }

component :: forall o m
             .  MonadAff m
             => Behaviour m
             => Navigate m
             => MonadAsk Env m
             => H.Component HH.HTML Query Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing, loading: 0 }
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
      [ case state.loading of
          0 -> HH.text "" 
          _ -> HH.div [ HP.class_ $ H.ClassName "loading" ] [] 
      , case state.route of
          Just TaskIndex ->
            HH.slot _tasks unit Tasks.component unit absurd
          Just TaskNew ->
            HH.slot _taskNew unit TaskNew.component unit absurd
          Just NoteIndex ->
            HH.slot _notes unit Notes.component unit absurd
          Just NoteNew ->
            HH.slot _noteNew unit NoteNew.component unit absurd
          Just (NoteEdit id) ->
            HH.slot _noteEdit id NoteEdit.component id absurd
          Nothing -> HH.text ""
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
      void $ H.fork globalMessageLoop
      path <- H.liftEffect $ window >>= location >>= pathname
      updateRoute path

  globalMessageLoop = do
    globalMessage <- asks _.globalMessage
    query <- H.liftAff $ AVar.take globalMessage
    -- 再起だと H.modify_ (_ { route = Just route }) でなぜかハングするの
    void $ H.fork globalMessageLoop
    case query of
      NavigateG route -> do
        pushState route
        logMessage "navigate H.modify_ start"
        H.modify_ (_ { route = Just route })
        logMessage "navigate H.modify_ end"
        pure unit
      StartLoadingG -> do
        H.modify_ \st -> st { loading = st.loading + 1 }
      StopLoadingG -> do
        H.modify_ \st -> st { loading = st.loading - 1 }

  pushState route = do
    pushStateInterface <- asks _.pushStateInterface
    H.liftEffect $ pushStateInterface.pushState (unsafeToForeign {}) $ routeToPath route

  updateRoute path = do
    case match myRoute path of
      Right newRoute -> do
        H.modify_ \st -> st { route = Just newRoute }
      Left e -> H.liftEffect $ log e
