module Coc.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, lift, runReaderT)
import Effect.AVar (AVar, put)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Routing.PushState (PushStateInterface)
import Type.Equality (class TypeEquals, from)

data GlobalMessage 
  = NavigateG String

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

-- raiseG query = do
--   globalMessage <- asks _.globalMessage
--   H.liftAff $ put query globalMessage

class Monad m <= Navigate m where
  navigate :: String -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (H.HalogenM st act slots msg m) where
  navigate path = lift <<< navigate path

instance navigateAppM :: Navigate AppM where
  navigate path = do
    globalMessage <- asks _.globalMessage
    H.liftAff $ put (NavigateG path) globalMessage

{-
https://dev.to/rinn7e/global-message-passing-inside-purescript-halogen-30ol
https://github.com/thomashoneyman/purescript-halogen-realworld/blob/halogen-5/src/AppM.purs
-}