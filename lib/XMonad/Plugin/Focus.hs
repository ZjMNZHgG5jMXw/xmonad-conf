module XMonad.Plugin.Focus
  ( pluginFocus
  ) where

import XMonad
import XMonad.Actions.UpdatePointer ( updatePointer, PointerPosition ( Relative ) )

pluginFocus conf = conf
   { borderWidth = 0
   , logHook     = updatePointer (Relative 0.61803398 0.61803398) >> logHook conf
   }

-- vim: ft=haskell:sw=2:sts=2:et:nu
