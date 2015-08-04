module Main where

import Graphics.UI.GLUT

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello, World!"
  displayCallback $= display
  mainLoop

