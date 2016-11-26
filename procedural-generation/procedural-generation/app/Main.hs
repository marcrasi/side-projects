module Main where

import qualified Data.Array.IArray as IArray
import qualified Data.Array.Unboxed as Unboxed

import qualified ArrayBuilder

arrayBuilder :: ArrayBuilder.ArrayBuilder Int Int Int
arrayBuilder = ArrayBuilder.FromElementAdders $ \index element -> ArrayBuilder.writeArray index (2 * element)

main :: IO ()
main = do
  let foo = IArray.array (1, 10) [(i, i) | i <- [1..10]] :: Unboxed.UArray Int Int
  let foo' = ArrayBuilder.runArrayBuilder arrayBuilder foo
  putStrLn $ show foo'
