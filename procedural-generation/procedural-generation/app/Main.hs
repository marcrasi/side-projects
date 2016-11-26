module Main where

import Control.Monad

import qualified Data.Array.IArray as IArray
import qualified Data.Array as Array 

import qualified ArrayBuilder

arrayBuilder = \index -> do
  element <- ArrayBuilder.readSource index
  let leftIndex = max 1 (index - 1)
  let rightIndex = min 10 (index + 1)
  ArrayBuilder.incrementDestination leftIndex (0.5 * element)
  ArrayBuilder.incrementDestination rightIndex (0.5 * element)

stepAndPrint array _ = do
  let newArray = ArrayBuilder.runArrayBuilder arrayBuilder array
  putStrLn $ show newArray
  return newArray

main :: IO ()
main = do
  let start = IArray.array (1, 10) [(i, fromInteger i) | i <- [1..10]] :: Array.Array Integer Double
  foldM_ stepAndPrint start [1..100]
