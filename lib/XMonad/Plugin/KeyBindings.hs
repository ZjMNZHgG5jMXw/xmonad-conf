module XMonad.Plugin.KeyBindings
  ( pluginKeyBindings
  ) where

import XMonad
import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.NamedScratchpad ( namedScratchpadFilterOutWorkspace )
import XMonad.Hooks.ManageDocks ( ToggleStruts ( ToggleStruts ) )
import XMonad.Hooks.UrgencyHook ( focusUrgent )
import XMonad.Actions.CopyWindow ( runOrCopy, kill1 )
import XMonad.Actions.FloatKeys (keysResizeWindow )
import XMonad.Actions.CycleWS
  ( WSType ( EmptyWS, WSIs )
  , Direction1D ( Prev, Next )
  --, prevWS
  --, nextWS
  --, shiftToPrev
  --, shiftToNext
  --, toggleWS
  , moveTo
  , shiftTo
  )
import XMonad.StackSet
  ( focusUp
  , focusDown
  , swapUp
  , swapDown
  , tag
  , view
  , hidden
  )
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens ( onCurrentScreen, workspaces' )
import XMonad.Plugin.Scratch ( startScratch )
import XMonad.Util.XSelection ( safePromptSelection )

pluginKeyBindings conf = conf { modMask = mod4Mask } `additionalKeysP`
  ([ ("M-b", sendMessage ToggleStruts)
  {--
  , ("M-e", screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ("M-w", screenWorkspace 1 >>= flip whenJust (windows . W.view))
  , ("M-S-e", screenWorkspace 0 >>= flip whenJust (windows . W.shift))
  , ("M-S-w", screenWorkspace 1 >>= flip whenJust (windows . W.shift))
  --}
  , ("C-M1-<L>", prevWS)
  , ("C-M1-<R>", nextWS)
  , ("C-M1-S-<L>", shiftToPrev)
  , ("C-M1-S-<R>", shiftToNext)
  --
  , ("C-M1-<U>", toggleWS)
  , ("C-M1-<D>", moveTo Next EmptyWS)
  , ("C-M1-S-<D>", shiftTo Next EmptyWS)
  --
  , ("M-<L>", windows focusUp)
  , ("M-<R>", windows focusDown)
  , ("M-S-<L>", windows swapUp)
  , ("M-S-<R>", windows swapDown)
  --
  --, ("C-M1-<End>", spawn "gnome-screensaver-command -l")
  , ("C-M1-<End>", spawn "xlock")
  --
  , ("M-n", spawn "firefox")
  , ("M-a", spawn "acroread -openInNewWindow")
  , ("M-c", spawn "evolution -c calendar")
  , ("M-z", spawn "urxvtcd")
  , ("M-m", spawn "urxvtcd -e mutt")
  , ("M-v", spawn "urxvtcd -e vim")
  , ("M-s", startScratch "music")
  , ("M-S-s", spawn "pavucontrol")
  , ("M-o", spawn "loffice")
  , ("M-g", spawn "urxvtcd -fn 'xft:Monospace-16' -e screen")
  --, ("M-d", spawn "ding")
  --, ("M-d", runOrCopy "ding" (className =? "Ding"))
  , ("M-d", safePromptSelection "ding")
  , ("M-f", spawn "nautilus --no-desktop")
  , ("M-x j", spawn "xournal")
  , ("M-x x", spawn "urxvtcd -e screen")
  , ("M-x h", spawn "urxvtcd -e ghci +RTS -N")
  , ("M-x r", spawn "urxvtcd -e R")
  , ("M-x s", runOrCopy "skype" (className =? "Skype"))
  --, ("M-x i", spawn "empathy")
  , ("M-x t", runOrCopy "sflphone" (className =? "Sflphone"))
  , ("M-x p", spawn "xchat")
  , ("M-x g", spawn "gimp")
  --
  , ("M-+", withFocused (keysResizeWindow (50,50) (0.5,0.5)))
  , ("M--", withFocused (keysResizeWindow (-50,-50) (0.5,0.5)))
  --
  , ("M-S-x", kill)
  , ("M-S-k", kill1)
  --
  , ("M-<Home>", focusUrgent)
  ] ++ [ ("M-" ++ show i, windows $ onCurrentScreen W.greedyView w)
          | (i, w) <- zip [1..9] (workspaces' conf) ]
    ++ [ ("M-S-" ++ show i, windows $ onCurrentScreen W.shift w)
          | (i, w) <- zip [1..9] (workspaces' conf) ]
  )
  where
    notSP = WSIs ( return $ ("NSP" /=) . tag )
    prevWS = moveTo Prev notSP
    nextWS = moveTo Next notSP
    shiftToPrev = shiftTo Prev notSP
    shiftToNext = shiftTo Next notSP
    toggleWS = windows $ view =<< tag . head . filter (("NSP" /=) . tag) . hidden

-- vim: ft=haskell:sw=2:sts=2:et:nu
