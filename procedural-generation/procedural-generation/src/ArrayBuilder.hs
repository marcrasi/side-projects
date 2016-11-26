{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

module ArrayBuilder where

import Control.Applicative
import Control.Monad

import qualified Control.Monad.ST as ST
import qualified Data.Array.IArray as IArray
import qualified Data.Array.MArray as MArray
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as Unboxed

{-
data ArrayBuilder i e r = ArrayBuilder { runArrayBuilder :: Unboxed.UArray i e -> (Unboxed.UArray i e, r) }

instance Functor (ArrayBuilder i e) where
  fmap = liftM

instance Applicative (ArrayBuilder i e) where
  pure x = ArrayBuilder $ \initialArray -> (initialArray, x)
  (<*>) = ap

instance Monad (ArrayBuilder i e) where
  return = pure

  (ArrayBuilder runArrayBuilder1) >>= f =
    ArrayBuilder $ \initialArray ->
      let
        (nextArray, result) = runArrayBuilder1 initialArray
        (ArrayBuilder runArrayBuilder2) = f result
      in
        runArrayBuilder2 nextArray

readElement :: (IArray.Ix i, IArray.IArray (Unboxed.UArray) e) => i -> ArrayBuilder i e e
readElement index =
  ArrayBuilder $ \initialArray -> (initialArray, initialArray IArray.! index)

writeElement :: (IArray.Ix i, IArray.IArray (Unboxed.UArray) e) => i -> e -> ArrayBuilder i e ()
writeElement index element =
  ArrayBuilder $ \initialArray -> (initialArray IArray.// [(index, element)], ())
-}

data ArrayBuilder i e0 e1 = forall s1 s2. FromElementAdders (i -> e0 -> STArray.STUArray s1 i e1 -> ST.ST s2 ())

writeArray :: (IArray.Ix i, MArray.MArray (STArray.STUArray s1) e1 (ST.ST s2)) => i -> e1 -> STArray.STUArray s1 i e1 -> ST.ST s2 ()
writeArray index element array = MArray.writeArray array index element

runArrayBuilder :: (Num e0, Num e1) => ArrayBuilder i e0 e1 -> Unboxed.UArray i e0 -> Unboxed.UArray i e1
runArrayBuilder (FromElementAdders runElementAdder) sourceArray = STArray.runSTUArray $ do
  array <- MArray.newArray (IArray.bounds sourceArray) (fromInteger 0)
  mapM_ (\(index, element) -> runElementAdder index element array) (IArray.assocs sourceArray)
  return array
