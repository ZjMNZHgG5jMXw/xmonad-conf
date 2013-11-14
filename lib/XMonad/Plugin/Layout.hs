module XMonad.Plugin.Layout
  ( autoLayout
  , pluginLayout
  ) where

import XMonad hiding ( openDisplay, closeDisplay )
import XMonad.Layout.Named ( named )
import XMonad.Layout.LayoutHints ( layoutHintsToCenter )
import XMonad.Layout.Spacing ( spacing )
import XMonad.Layout.PerWorkspace ( onWorkspaces )
import XMonad.Layout.IndependentScreens ( withScreens )

import Graphics.X11 ( openDisplay, closeDisplay )
import Graphics.X11.Xinerama ( xineramaQueryScreens, xsi_width, xsi_height )

import Data.List ( genericLength )

tall = Tall
  { tallNMaster = 1
  , tallRatioIncrement = 3/100
  , tallRatio = 61803398/100000000
  }

landscape = layoutHintsToCenter $ (named "|h|" $ spacing 1 tall) ||| (named "-v-" $ spacing 1 (Mirror tall)) ||| named "[f]" Full
portrait  = layoutHintsToCenter $ (named "-v-" $ spacing 1 (Mirror tall)) ||| (named "|h|" $ spacing 1 tall) ||| named "[f]" Full

pluginLayout fun conf = conf
  { layoutHook  = onWorkspaces (filter fun (workspaces conf)) portrait landscape }

autoLayout wm conf = do
  disp <- openDisplay ""
  info <- xineramaQueryScreens disp
  closeDisplay disp
  maybe (run wm conf []) (run wm conf) info
  
run wm conf xs
  = wm
  $ pluginLayout (foldr fun (const False) (zip [0..] xs))
  $ conf { workspaces = withScreens (genericLength xs) (workspaces conf) }
  where
    fun (i,x) f
      | xsi_width x > xsi_height x  = f
      | otherwise                   = \s -> (head s:[]) == show i || f s

-- vim: ft=haskell:sw=2:sts=2:et:nu
