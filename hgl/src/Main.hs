module Main where

import Data.IORef

import Graphics.UI.GLUT

import Engine.Display
import Engine.Input

main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello, World!"
  angle <- newIORef 0.0
  displayCallback $= display angle
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  idleCallback $= Just (idle angle)
  mainLoop

