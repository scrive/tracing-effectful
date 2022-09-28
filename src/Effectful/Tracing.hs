{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Tracing
  ( -- * Effect
    Tracing
  -- , MonadTrace (..)

    -- * Handlers
  -- , runTrace

    -- * Functions
  , rootSpan
  , childSpan
  , runTrace
  , runTrace'
  -- , clientSpan
  -- , clientSpanWith
  -- , serverSpan
  -- , serverSpanWith
  -- , producerSpanWith
  -- , consumerSpanWith
  -- , tag
  -- , annotate
  -- , annotateAt
  )
where

-- import Control.Monad.Trace
import Control.Monad.Trace.Class
import Effectful
import Effectful.Dispatch.Static
import Control.Monad.Trace (Tracer)

-- | Provide the ability to log messages via 'MonadLog'.
data Tracing :: Effect

type instance DispatchOf Tracing = Static WithSideEffects
newtype instance StaticRep Tracing = Tracing ()

runTrace
  :: IOE :> es
  => Eff (Tracing : es) a
  -> Tracer
  -> Eff es a
runTrace _action _tracer = undefined

runTrace'
  :: IOE :> es
  => Maybe Tracer
  -> Eff (Tracing : es) a
  -> Eff es a
runTrace'  _tracer = evalStaticRep  $ Tracing ()

----------------------------------------
-- Orphan instance

instance Tracing :> es => MonadTrace (Eff es) where
  trace :: Tracing :> es => Builder -> Eff es a -> Eff es a
  trace = undefined
  activeSpan :: Tracing :> es => Eff es (Maybe Span)
  activeSpan = undefined
  addSpanEntry :: Tracing :> es => Key -> Value -> Eff es ()
  addSpanEntry = undefined
-- deriving via (TraceT m) instance Tracing :> es => MonadTrace (Eff es)
