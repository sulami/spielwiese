module Main where

import Data.IORef

import Graphics.UI.GLUT

import Engine.Display
import Engine.Input

main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  _window <- createWindow "Hello, World!"
  angle <- newIORef 0
  delta <- newIORef 1
  pos <- newIORef (0,0)
  displayCallback $= display angle pos
  depthFunc $= Just Less
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback $= Just (idle angle delta)
  mainLoop

