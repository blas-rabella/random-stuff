{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleContexts #-}
module Derived where

import Control.Monad.Free.TH
import Control.Monad.Free
import System.Exit hiding (ExitSuccess)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map



data OperatorsF x =
  ReadPin Int (Double -> x)
  | WritePin Int Double x
  | SwitchSignal Int x
  | End
  deriving Functor

makeFree ''OperatorsF

type Operation = Free OperatorsF

program :: Free OperatorsF ()
program = do
  writePin 1 0.3
  x <- readPin 1
  writePin 2 x
  switchSignal 4
  switchSignal 1
  end
  writePin 3 x

type Circuit = Map.Map Int Double

runM ::
  MonadState (Circuit) m => OperatorsF (m ()) -> m ()
runM End = return ()
runM (WritePin n sig next) = do
  modify (Map.insert n sig)
  next
runM (ReadPin n next) = do
  x <- gets (Map.findWithDefault 0 n)
  next x
runM (SwitchSignal n next) = do
  x <- gets (Map.findWithDefault 0 n)
  modify (Map.insert n (1-x))
  next


main :: IO ()
--main = run program
main = print $ execState (iterM runM program) Map.empty
