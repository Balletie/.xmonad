{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
module ImageButtonHandlerDecoration where

import XMonad
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ImageButtonDecoration
-- Copy-paste from xmonad-contrib, because it doesn't export "Top" and "WhenPlural"
import Tabbed

newtype ImageButtonHandler ds a = ImageButtonHandled { unHandleButtons :: ds a }
  deriving (Read, Show)

handleButtons = ImageButtonHandled

instance (Eq a, DecorationStyle ds a) => DecorationStyle (ImageButtonHandler ds) a where
    describeDeco        = ( ++ " ImageButtonHandled") . (describeDeco . unHandleButtons)
    -- decorationEventHook = decorationEventHook . unHandleButtons
    pureDecoration      = pureDecoration . unHandleButtons
    shrink              = shrink . unHandleButtons
    decorationCatchClicksHook _ = imageTitleBarButtonHandler
    decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()

addHandledButtonTabs sh th = decoration sh th (handleButtons (Tabbed Top WhenPlural))
