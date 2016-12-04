module Display where

import Data.IORef ( IORef, newIORef )
import Graphics.UI.GLUT
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

data State = State { rxRef, ryRef :: IORef GLfloat }

makeState :: IO State
makeState = do
  rx <- newIORef 0
  ry <- newIORef 0
  return $ State rx ry

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
  diffuse (Light 0) $= Color4 1 1 1 1
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 1 1 1 0
  lightModelTwoSide $= Enabled
  depthFunc $= Just Less

  materialAmbient Front $= Color4 1 1 1 1
  materialDiffuse Front $= Color4 1 1 1 1
  materialSpecular Front $= Color4 0 0 0 1
  materialShininess Front $= 0

  rowAlignment Unpack $= 1

  exts <- get glExtensions
  if "GL_EXT_texture_object" `elem` exts
    then putStrLn "It's there!"
    else putStrLn "It's not there :("

rotateDisplay :: State -> DisplayCallback -> DisplayCallback
rotateDisplay state dcb = do
  clear [ ColorBuffer, DepthBuffer ]
  preservingMatrix $ do
    rx <- get (rxRef state)
    ry <- get (ryRef state)
    rotate rx $ Vector3 0 1 0
    rotate ry $ Vector3 1 0 0
    scale 0.5 0.5 (0.5 :: GLfloat)
    --renderObject Solid (Sphere' 1 20 16)
    dcb
    flush
  swapBuffers

keyboard :: State -> KeyboardMouseCallback
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

doDisplay :: DisplayCallback -> IO ()
doDisplay dcb = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  _ <- createWindow "Procedural Generation Display"
  state <- makeState
  myInit
  displayCallback $= (rotateDisplay state dcb)
  keyboardMouseCallback $= Just (keyboard state)
  mainLoop
