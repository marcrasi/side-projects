{-# LANGUAGE ExistentialQuantification, FlexibleContexts, Rank2Types #-}

module ArrayBuilder where

import Control.Applicative
import Control.Monad

import qualified Control.Monad.ST as ST
import qualified Data.Array as Array
import qualified Data.Array.IArray as IArray
import qualified Data.Array.MArray as MArray
import qualified Data.Array.ST as STArray

newtype DestinationMutation s i e0 e1 r = DestinationMutation { runDestinationMutation :: Array.Array i e0 -> STArray.STArray s i e1 -> ST.ST s r }

instance Functor (DestinationMutation s i e0 e1) where
  fmap = liftM

instance Applicative (DestinationMutation s i e0 e1) where
  pure x = DestinationMutation $ \_ _ -> return x
  (<*>) = ap

instance Monad (DestinationMutation s i e0 e1) where
  return = pure
  (DestinationMutation runDestinationMutation1) >>= f =
    DestinationMutation $ \source destination -> do
      result1 <- runDestinationMutation1 source destination 
      let (DestinationMutation runDestinationMutation2) = f result1
      runDestinationMutation2 source destination 

incrementDestination :: (Num e1, Array.Ix i) => i -> e1 -> DestinationMutation s i e0 e1 ()
incrementDestination index increment = DestinationMutation $ \_ destination -> do
  currentValue <- MArray.readArray destination index
  MArray.writeArray destination index (currentValue + increment)

readSource :: (Array.Ix i) => i -> DestinationMutation s i e0 e1 e0
readSource index = DestinationMutation $ \source _ -> return $ source IArray.! index

runArrayBuilder :: (Num e0, Num e1, Array.Ix i) => (forall s. i -> DestinationMutation s i e0 e1 ()) -> Array.Array i e0 -> Array.Array i e1
runArrayBuilder getDestinationMutation source = STArray.runSTArray $ do
  destination <- MArray.newArray (IArray.bounds source) (fromInteger 0)
  mapM_ (\index -> runDestinationMutation (getDestinationMutation index) source destination) (IArray.indices source)
  return destination 
