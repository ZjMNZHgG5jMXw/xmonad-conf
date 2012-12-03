module XMonad.Plugin.Java
  ( pluginJava
  ) where

import XMonad
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.ICCCMFocus ( takeTopFocus )

pluginJava conf = conf
  { startupHook = ( startupHook conf >> setWMName "LG3D" )
  , logHook = logHook conf >> takeTopFocus
  }

-- vim: ft=haskell:sw=2:sts=2:et:nu
