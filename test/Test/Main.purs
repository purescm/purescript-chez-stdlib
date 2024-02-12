module Test.Chez.Main where

import Prelude

import Chez.Thread as Thread
import Data.Array as Array
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  counter <- Ref.new 0

  let
    modifyFn :: Int -> Int
    modifyFn x = unsafePerformEffect do
      Thread.sleep (Milliseconds 10.0)
      pure (x + 1)

    threadFn threadId = do
      for_ (Array.range 0 9) \_ -> do
        newCounter <- Ref.modify modifyFn counter
        log $ "Thread " <> show (Thread.threadIdToInt threadId) <> ": new counter is " <> show
          newCounter
        pure newCounter

  threads <- for (Array.range 0 9) \_ -> Thread.fork threadFn

  for_ threads Thread.join

  finalCount <- Ref.read counter
  assertEqual { actual: finalCount, expected: 100 }
