-- | This module provides a simple interface to the Chez Scheme thread API.
-- | See Chapter 15 of the Chez Scheme User's Guide for more information:
-- | https://cisco.github.io/ChezScheme/csug10.0/threads.html
module Chez.Thread
  ( Thread
  , ThreadId
  , Mutex
  , Condition
  , fork
  , join
  , threadIdToInt
  , makeMutex
  , withMutex
  , makeCondition
  , conditionWait
  , conditionSignal
  , sleep
  ) where

import Prelude

import Data.Int as Int
import Data.Time.Duration (class Duration, Milliseconds(..))
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as Uncurried
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Thread creation/termination

foreign import data ThreadId :: Type

threadIdToInt :: ThreadId -> Int
threadIdToInt = unsafeCoerce

foreign import data Thread :: Type

foreign import forkThreadImpl :: EffectFn1 (EffectFn1 ThreadId Unit) Thread

fork :: (ThreadId -> Effect Unit) -> Effect Thread
fork fn = Uncurried.runEffectFn1 forkThreadImpl (Uncurried.mkEffectFn1 fn)

foreign import joinThreadImpl :: EffectFn1 Thread Unit

join :: Thread -> Effect Unit
join = Uncurried.runEffectFn1 joinThreadImpl

--------------------------------------------------------------------------------
-- Thread syncronisation - mutexes

data Mutex

foreign import makeMutex :: Effect Mutex

foreign import withMutexImpl :: EffectFn2 Mutex (Effect Unit) Unit

withMutex :: Mutex -> Effect Unit -> Effect Unit
withMutex mutex fn = Uncurried.runEffectFn2 withMutexImpl mutex fn

--------------------------------------------------------------------------------
-- Thread syncronisation - conditions (aka semaphores)

data Condition

foreign import makeCondition :: Effect Condition

foreign import conditionWaitImpl :: EffectFn2 Condition Mutex Unit

conditionWait :: Condition -> Mutex -> Effect Unit
conditionWait condition mutex = Uncurried.runEffectFn2 conditionWaitImpl condition mutex

foreign import conditionSignalImpl :: EffectFn1 Condition Unit

conditionSignal :: Condition -> Effect Unit
conditionSignal condition = Uncurried.runEffectFn1 conditionSignalImpl condition

--------------------------------------------------------------------------------
-- Thread sleep

foreign import sleepImpl :: EffectFn2 Int Int Unit

sleep :: forall a. Duration a => a -> Effect Unit
sleep duration = do
  let
    (Milliseconds millis) = Duration.fromDuration duration
    seconds = Int.floor (millis / 1000.0)
    nanoseconds = Int.floor ((millis - Int.toNumber (seconds * 1000)) * 1000000.0)
  Uncurried.runEffectFn2 sleepImpl nanoseconds seconds
