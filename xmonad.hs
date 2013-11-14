import XMonad ( xmonad, defaultConfig, focusedBorderColor, terminal )
import XMonad.Hooks.EwmhDesktops ( ewmh )

import XMonad.Plugin.Floats ( pluginFloats )
import XMonad.Plugin.Focus ( pluginFocus )
import XMonad.Plugin.Panel ( withPanel )
import XMonad.Plugin.KeyBindings ( pluginKeyBindings )
import XMonad.Plugin.Layout ( autoLayout )
import XMonad.Plugin.InstantM ( pluginIM )
import XMonad.Plugin.Scratch ( pluginScratch )
import XMonad.Plugin.Fading ( pluginFade )

conf = defaultConfig 
  { focusedBorderColor    = "#0000a0"
  , terminal              = "/usr/bin/urxvtcd"
  }

customise
  = ewmh
  . pluginFade
  . pluginScratch
  . pluginFloats
  . pluginFocus
  . pluginKeyBindings
  . pluginIM

main = autoLayout (withPanel xmonad . customise) conf

-- vim: ft=haskell:sw=2:sts=2:et:nu
