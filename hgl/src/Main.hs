module Main where

import Graphics.UI.GLUT

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [ (sin (2*pi*k/100), cos (2*pi*k/100), 0) | k <- [1..100] ]

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
  flush

main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello, World!"
  displayCallback $= display
  mainLoop

