module XMonad.Plugin.Floats
  ( pluginFloats
  ) where

import Control.Monad ( liftM )

import XMonad
import XMonad.Hooks.ManageHelpers ( (/=?), doCenterFloat, doFullFloat )
import XMonad.StackSet as W ( sink )

pluginFloats conf = conf { manageHook = addFloats <+> manageHook conf }

addFloats = composeAll
  [ (className =? "Evolution" <&&> (title /=? "Kalender - Evolution")) --> doCenterFloat
  , (className =? "Evolution-alarm-notify") --> doCenterFloat
  , (className =? "Transmission" <&&> (title /=? "Transmission")) --> doCenterFloat
  , (className =? "Pidgin" <&&> (title =? "Pidgin")) --> doCenterFloat
  , (className =? "Skype" <&&> (takeM 11 title =? "Profile for")) --> doCenterFloat
  , (className =? "Skype" <&&> title =? "Add a Skype Contact") --> doCenterFloat
  , (className =? "Acroread" <&&> title =? "Print") --> doCenterFloat
  , (className =? "java-lang-Thread") --> doCenterFloat
  , (className =? "Personal.bin") --> doCenterFloat
  , (className =? "Brasero" <&&> title =? "Bränner cd") --> doCenterFloat
  , (className =? "Brasero" <&&> title =? "Bränner dvd") --> doCenterFloat
  , (className =? "Brasero" <&&> (takeM 10 title =? "Egenskaper")) --> doCenterFloat
  , (className =? "Display" <&&> (takeM 15 title =? "GraphicsMagick:")) --> doCenterFloat
  , (className =? "Vmware-modconfig") --> doCenterFloat
  , (className =? "Firefox" <&&> title =? "Firefox - Choose User Profile") --> doCenterFloat
  , (className =? "Applet.py" <&&> title =? "Dokumentets utskriftsstatus (mina jobb)") --> doCenterFloat -- print
  , (className =? "Scp-dbus-service.py") --> doCenterFloat -- print
  , (className =? "Gnome-volume-control" <&&> title =? "Ljudinställningar") --> doCenterFloat
  , (className =? "Nm-connection-editor") --> doCenterFloat
  , (className =? "rdesktop") --> doSink
  , (className =? "Gimp" <&&> title =? "Kurvor") --> doCenterFloat
  , (className =? "Gimp") --> doSink
  , (className =? "Geeqie" <&&> title =? "Helskärm - Geeqie") --> doFullFloat
  , (className =? "Plugin-container") --> doFullFloat
  , (className =? "mplayer2") --> doCenterFloat
  , (className =? "Sflphone") --> doCenterFloat
  , (className =? "R") --> doCenterFloat
  ] where takeM n = liftM (take n)

doSink :: ManageHook
doSink = doF . W.sink =<< ask

-- vim: ft=haskell:sw=2:sts=2:et:nu
