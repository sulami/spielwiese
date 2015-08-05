module Engine.Input (
  keyboardMouse
) where

import Data.IORef

import Graphics.UI.GLUT

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat)
              -> KeyboardMouseCallback
keyboardMouse a p key down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char 'h') -> p $~! \(x,y) -> (x-0.01,y)
  (Char 'j') -> p $~! \(x,y) -> (x,y-0.01)
  (Char 'k') -> p $~! \(x,y) -> (x,y+0.01)
  (Char 'l') -> p $~! \(x,y) -> (x+0.01,y)
  _ -> return ()

