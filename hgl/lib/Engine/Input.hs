module Engine.Input (
  keyboardMouse
) where

import Graphics.UI.GLUT

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()

