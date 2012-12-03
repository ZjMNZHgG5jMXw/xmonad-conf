module XMonad.Plugin.Panel
  ( withPanel
  ) where

import XMonad
import XMonad.Actions.CopyWindow ( wsContainingCopies )
import XMonad.Util.Run ( spawnPipe, hPutStrLn )
import XMonad.Util.WorkspaceCompare ( getSortByXineramaRule )
import XMonad.Util.NamedScratchpad ( namedScratchpadFilterOutWorkspace )
import XMonad.Hooks.ManageDocks ( avoidStruts , manageDocks )
import XMonad.Hooks.UrgencyHook
  ( withUrgencyHookC
  , urgencyConfig
  , suppressWhen
  , SuppressWhen ( Focused )
  , NoUrgencyHook ( .. )
  , readUrgents
  )
import XMonad.Hooks.DynamicLog
  ( dynamicLogWithPP
  , xmobarPP
  , xmobarColor
  , ppOutput
  , ppTitle
  , ppCurrent
  , ppVisible
  , ppHidden
  , ppUrgent
  , ppSort
  )
import XMonad.Layout.IndependentScreens ( unmarshallW )
import Data.Char ( isPrint )

withPanel wm conf = do
  xmTop <- spawnPipe "xmobar"
  xmBot <- spawnPipe "xmobar .xmobar-bottomrc"
  wm (pluginPanel xmTop xmBot conf)

pluginPanel xmTop xmBot conf = withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused } $ conf
  { logHook = do
      {-copies <- wsContainingCopies
      let check ws  | ws `elem` copies  = xmobarColor "green" "black" $ ws
                    | otherwise         = ws
      -}
      dynamicLogWithPP $ xmobarPP
        { ppOutput  = hPutStrLn xmTop
        , ppTitle   = xmobarColor "orange" "" . filter isPrint -- . shorten 50
        , ppCurrent = \s -> xmobarColor "orange" "" ('[':(unmarshallW s) ++ "]")
        , ppVisible = xmobarColor "orange" "" . unmarshallW
        , ppHidden = unmarshallW -- check
        , ppUrgent  = \s -> xmobarColor "red" "" ('*':(unmarshallW s) ++ "*")
        , ppSort    = fmap (. namedScratchpadFilterOutWorkspace) getSortByXineramaRule
        } 
      ugs <- readUrgents
      nms <- mapM (runQuery title) ugs
      dynamicLogWithPP $ xmobarPP { ppOutput = (\_ -> hPutStrLn xmBot (if nms == [] then "" else " --- " ++ (filter isPrint $ head nms) ++ " ---  ")) } 
      logHook conf
  , manageHook = manageDocks <+> manageHook conf
  , layoutHook = avoidStruts $ layoutHook conf
  }

-- vim: ft=haskell:sw=2:sts=2:et:nu
