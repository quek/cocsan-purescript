{-
https://dev.to/rinn7e/global-message-passing-inside-purescript-halogen-30ol
https://github.com/thomashoneyman/purescript-halogen-realworld/blob/halogen-5/src/AppM.purs
-}
module Coc.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (lift)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Routing.PushState (PushStateInterface)
import Type.Equality (class TypeEquals, from)

data GlobalMessage
  = NavigateG MyRoute

type Env =
  { globalMessage :: AVar GlobalMessage
  , pushStateInterface :: PushStateInterface
  }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

data MyRoute
  = TaskIndex
  | TaskNew
  | NoteIndex

routeToPath :: MyRoute -> String
routeToPath = case _ of
  TaskIndex -> "/tasks"
  TaskNew -> "/tasks/new"
  NoteIndex -> "/notes"

class Monad m <= Navigate m where
  navigate :: MyRoute -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (H.HalogenM st act slots msg m) where
  navigate = lift <<< navigate

instance navigateAppM :: Navigate AppM where
  navigate route = do
    globalMessage <- asks _.globalMessage
    liftAff $ put (NavigateG route) globalMessage


class Monad m <= LogMessages m where
  logMessage :: String -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (H.HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    H.liftEffect $ Console.log log
