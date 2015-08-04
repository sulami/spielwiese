module Main where

import Graphics.UI.GLUT (
  getArgsAndInitialize, createWindow,
  displayCallback, reshapeCallback,
  mainLoop,
  ($=)
  )

import Engine.Display

main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello, World!"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop

