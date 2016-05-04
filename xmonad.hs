{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook(withUrgencyHook)
import XMonad.Layout.Decoration(Decoration, DefaultShrinker)
import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess, modifyLayout,
                                    redoLayout),
                                    ModifiedLayout(..))
import XMonad.Layout.Spacing
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.Fullscreen
import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Themes

import Data.Monoid
import Data.Map as M hiding (keys)

import Tabbed
import ImageButtonHandlerDecoration (addHandledButtonTabs)
import LibNotifyUrgency (LibNotifyUrgencyHook(..))

main :: IO()
main = xmonad $ withUrgencyHook LibNotifyUrgencyHook
     $ myConfig

boilerPlateConfig = desktopConfig

myNormalBorderColor = "#BDBDBD"
myFocusedBorderColor = "#FFCD07"

myButtonedTheme = defaultThemeWithImageButtons {
    fontName = "xft:Sans-9:bold"
  , decoHeight = 20
  , activeColor = "#FFDA4D"
  , inactiveColor = "#EDEDED"
  , urgentColor = activeColor myButtonedTheme
  , activeBorderColor = myFocusedBorderColor
  , inactiveBorderColor = myNormalBorderColor
  , urgentBorderColor = activeBorderColor myButtonedTheme
  , activeTextColor = "#000000"
  , inactiveTextColor = "#515151"
  , urgentTextColor = activeTextColor myButtonedTheme
  }

myTheme = myButtonedTheme {
    windowTitleAddons = []
  , windowTitleIcons  = []
  }

myThemedSubTabbed x = addHandledButtonTabs shrinkText myButtonedTheme $ subLayout [] Simplest x

myLayout = avoidStruts $ fullscreenFull $ windowNavigation $ myThemedSubTabbed
         $ boringWindows $ modifiedLayout ||| noBorders Full
  where
        modifiedLayout = smartBorders $ withBorder 3 $ smartSpacingWithEdge 4 $ layout
        layout         = tiled ||| Mirror tiled
        -- default tiling algorithm partitions the screen into two panes
        tiled          = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster        = 1
        -- Default proportion of screen occupied by master pane
        ratio          = 1/2
        -- Percent of screen to increment by when resizing panes
        delta          = 3/100

myLogHook = logHook boilerPlateConfig
         >> fadeInactiveLogHook opacity
         >> updatePointer pointerPos (0, 0)
  where opacity = 0.9
        pointerPos = (0.5, 0.5)

myStartupHook = do
  startupHook boilerPlateConfig
  -- Set left pointer of root screen
  spawn "xsetroot -cursor_name left_ptr"

myEventHook =
     docksEventHook
  <> fullscreenEventHook
  <> handleEventHook boilerPlateConfig

isNotification :: Query Bool
isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

myManageHook =
     manageHook boilerPlateConfig
  <> fullscreenManageHook
  <> (className =? "Xfrun4" --> doFloat)
  -- TODO: Merge surf windows with tabbed group
  -- <> (className =? "Surf" --> )
  <> (className =? "Eclipse" --> doShift "dev")
  <> (className =? "Firefox" --> doShift "web")
  <> (className =? "Emacs" --> doShift "write")
  <> (className =? "Claws-mail" --> doShift "mail-chat")
  <> (isNotification --> doIgnore)
  <> (isSplash --> doFloat)

dmenu_args = " -nb '#cccccc' -sb '#dddddd'\
             \ -nf '#000000' -sf '#000000'\
             \ -fn 'xft:Sans:size=9'"

myKeys = additionalKeys <> keys boilerPlateConfig
additionalKeys config@(XConfig { modMask = mod }) = M.fromList $
  [ ((noModMask, xK_Print)      , spawn "scrot -e 'mv $f ~/Desktop'")
  , ((controlMask, xK_F12)      , spawn "xmonad --recompile && xmonad --restart\
    \ && twmnc -i dialog-information -t Info -c \"XMonad recompiled and restarted\"\
    \ || twmnc -i dialog-error -t Error -c \"XMonad failed to compile\"")

  -- dmenu commands
  , ((mod, xK_p)                , spawn dmenu_run)
  , ((mod, xK_d)                , spawn $ dmenu_browse ++ " | xargs xdg-open")
  , ((mod .|. shiftMask, xK_d)  , spawn $ dmenu_browse ++ " --ls -A | xargs xdg-open")
  , ((mod, xK_f)                , spawn $ "printf '%s \"%s\"' $(dmenu_path | dmenu " ++ dmenu_args ++ ") \"$(" ++ dmenu_browse ++ ")\" | /bin/sh")
  , ((mod .|. shiftMask, xK_f)  , spawn $ "printf '%s \"%s\"' $(dmenu_path | dmenu " ++ dmenu_args ++ ") \"$(" ++ dmenu_browse ++ " --ls -A)\" | /bin/sh")

  -- These use boringWindows to skip over e.g. tabs when switching
  , ((mod, xK_k)                , focusUp)
  , ((mod, xK_j)                , focusDown)

  -- Submap for sublayouts (tabs)
  , ((mod, xK_s)                , submap $ defaultSublMap config)

  -- Merge and unmerge tabs
  , ((mod .|. controlMask, xK_h), sendMessage $ pullGroup L)
  , ((mod .|. controlMask, xK_l), sendMessage $ pullGroup R)
  , ((mod .|. controlMask, xK_k), sendMessage $ pullGroup U)
  , ((mod .|. controlMask, xK_j), sendMessage $ pullGroup D)

  , ((mod .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  , ((mod .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
  ]
  where dmenu_run = "dmenu_run -p 'Run:' " ++ dmenu_args
        dmenu_browse = "/home/skip/.local/bin/dmenu_browse /home/skip --dm " ++ dmenu_args

-- Uses the `ewmh` function for adding ewmh functionality
myConfig = ewmh boilerPlateConfig {
    workspaces = ["web", "mail-chat", "write", "dev"]
  , borderWidth = 2
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , modMask = mod4Mask
  , keys = myKeys
  , logHook = myLogHook
  , handleEventHook = myEventHook
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , layoutHook = myLayout
  , terminal = "urxvtc"
  }
