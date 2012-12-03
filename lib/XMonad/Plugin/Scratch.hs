module XMonad.Plugin.Scratch
  ( pluginScratch
  , startScratch
  ) where

import XMonad
-- import XMonad.Util.Scratchpad ( scratchpadManageHookDefault )
import XMonad.Util.NamedScratchpad
  ( namedScratchpadAction
  , namedScratchpadManageHook
  , NamedScratchpad ( NS )
  , defaultFloating
  )

scratchpads =
  [ NS "music" "audacious" (className =? "Audacious") defaultFloating
  ]

startScratch = namedScratchpadAction scratchpads

pluginScratch conf = conf { manageHook = namedScratchpadManageHook scratchpads <+> manageHook conf }

-- vim: ft=haskell:sw=2:sts=2:et:nu
