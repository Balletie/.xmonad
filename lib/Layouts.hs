{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
module Layouts ( myLayout ) where


import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout (Full(..), Mirror(..), Tall(..), (|||))
import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess, modifyLayout,
                                    redoLayout),
                                    ModifiedLayout(..))
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.NoBorders (noBorders, smartBorders, withBorder)
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.Renamed (renamed, Rename(Replace))

import CopyPasteMonad.Layout.WorkspaceDir (workspaceDir)

import ImageButtonHandlerDecoration (addHandledButtonTabs)
import Tabbed (shrinkText)
import Themes (myButtonedTheme)

myLayout = workspaceModifiers layout
  where
        layout = vertical ||| horizontal ||| full

full = renamed [Replace "Full"] $ noBorders Full

horizontal = renamed [Replace "Hori"] $ Mirror vertical

vertical = renamed [Replace "Vert"] $ tabbed $ bordersAndSpacing $ Tall nmaster delta ratio
  where
        -- The default number of windows in the master pane
        nmaster        = 1
        -- Default proportion of screen occupied by master pane
        ratio          = 1/2
        -- Percent of screen to increment by when resizing panes
        delta          = 3/100

workspaceModifiers l = workspaceDir "/home/skip/" $ avoidStruts $ fullscreenFull
                     $ windowNavigation l

tabbed l = myThemedSubTabbed $ boringWindows l
  where
        myThemedSubTabbed x = addHandledButtonTabs shrinkText myButtonedTheme $ subLayout [] Simplest x

bordersAndSpacing l = smartBorders $ withBorder 1 $ smartSpacingWithEdge 1 l
