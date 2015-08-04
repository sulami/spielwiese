module Main where

import Graphics.UI.GLUT (
  getArgsAndInitialize, createWindow,
  displayCallback, reshapeCallback, keyboardMouseCallback,
  mainLoop,
  ($=)
  )

import Engine.Display
import Engine.Input

main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello, World!"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop

