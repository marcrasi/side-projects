module Display where

import Data.IORef ( IORef, newIORef, writeIORef )
import Data.Time.Clock ( diffUTCTime, getCurrentTime, UTCTime )
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

data State a = State { rxRef, ryRef :: IORef GLfloat, valueRef :: IORef a, lastUpdateRef :: IORef UTCTime }

makeState :: a -> IO (State a)
makeState initial = do
  rxRef <- newIORef 0
  ryRef <- newIORef 0
  valueRef <- newIORef initial
  lastUpdate <- getCurrentTime
  lastUpdateRef <- newIORef lastUpdate
  return $ State rxRef ryRef valueRef lastUpdateRef

myInit :: IO ()
myInit = do
  clearColor $= Color4 0 0 0 0
  matrixMode $= Projection
  loadIdentity
  ortho 1 0 1 1 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity

  shadeModel $= Smooth
  lighting $= Enabled
  ambient (Light 0) $= Color4 0.5 0.5 0.5 1
  light (Light 0) $= Enabled
  diffuse (Light 0) $= Color4 1 1 1 1
  position (Light 0) $= Vertex4 10 10 0 0
  lightModelTwoSide $= Enabled
  depthFunc $= Just Less

  rowAlignment Unpack $= 1

rotatedDisplay :: State a -> (a -> DisplayCallback) -> DisplayCallback
rotatedDisplay state dcb = do
  clear [ ColorBuffer, DepthBuffer ]
  preservingMatrix $ do
    rx <- get (rxRef state)
    ry <- get (ryRef state)
    rotate rx $ Vector3 0 1 0
    rotate ry $ Vector3 1 0 0
    scale 0.5 0.5 (0.5 :: GLfloat)
    --renderObject Solid (Sphere' 1 20 16)
    value <- get (valueRef state)
    dcb value
    flush
  swapBuffers

keyboard :: State a -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
  'w' -> update rxRef 10
  'a' -> update ryRef (-10)
  's' -> update rxRef (-10)
  'd' -> update ryRef 10
  '\27' -> exitWith ExitSuccess
  _ -> return ()
  where
    update angle inc = do
      angle state $~ (+inc)
      postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

doInitialize :: IO ()
doInitialize = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  _ <- createWindow "Procedural Generation Display"
  myInit

timerCallback :: State a -> (Double -> a -> a) -> IO ()
timerCallback state timeStep = do
  value <- get (valueRef state)
  lastUpdate <- get (lastUpdateRef state)
  currentTime <- getCurrentTime
  let stepSize = diffUTCTime currentTime lastUpdate
  let newValue = timeStep (realToFrac stepSize) value
  writeIORef (valueRef state) newValue
  writeIORef (lastUpdateRef state) currentTime
  postRedisplay Nothing
  addTimerCallback 100 (timerCallback state timeStep)

data ToDisplay a = ToDisplay
  { initial :: a
  , timeStep :: Double -> a -> a
  , dcb :: a -> DisplayCallback
  }

doDisplay :: a -> (Double -> a -> a) -> (a -> DisplayCallback) -> IO ()
doDisplay initial timeStep dcb = do
  state <- makeState initial
  displayCallback $= (rotatedDisplay state dcb)
  keyboardMouseCallback $= Just (keyboard state)
  addTimerCallback 100 (timerCallback state timeStep)
  mainLoop
