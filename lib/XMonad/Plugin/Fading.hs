module XMonad.Plugin.Fading
  ( pluginFade
  ) where

import XMonad
import XMonad.Hooks.ManageHelpers ( isDialog )
import XMonad.Hooks.FadeWindows
  ( fadeWindowsLogHook
  , fadeWindowsEventHook
  , opaque
  , transparency
  , isUnfocused
  )

pluginFade conf = conf
  { logHook = logHook conf >> fadeWindowsLogHook myFadeHook
  , handleEventHook = handleEventHook conf >> fadeWindowsEventHook
  }

myFadeHook = composeAll -- last rule wins
  [ opaque
  , isUnfocused --> transparency 0.1
  , className =? "Audacious" --> transparency 0.3
  , className =? "Audacious" <&&> isUnfocused --> transparency 0.75
  , className =? "Geeqie" <&&> title =? "Helskärm - Geeqie" --> opaque
  , isDialog --> opaque
  ]

-- vim: ft=haskell:sw=2:sts=2:et:nu
