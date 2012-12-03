--{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
--             MultiParamTypeClasses, TypeSynonymInstances, CPP#-}

module XMonad.Plugin.InstantM
  ( pluginIM
  ) where

import XMonad
import XMonad.Layout.IM
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Layout.Renamed ( renamed, Rename ( CutWordsLeft ) )
import Data.Ratio ((%))

reflect = renamed [CutWordsLeft 1] . reflectHoriz

--skype layout = reflect $ nameTail $ withIM (1428571%10000000) ((ClassName "Skype") `And` (Role "MainWindow")) $ reflect layout
skype layout = reflect $ renamed [CutWordsLeft 1] $ withIM (1428571%10000000) (ClassName "Skype" `And` (Title "stefan.berthold - Skype™" `Or` Title "Skype™ 4.0 for Linux")) $ reflect layout

pluginIM conf = conf { layoutHook = skype (layoutHook conf) }

-- vim: ft=haskell:sw=2:sts=2:et:nu
