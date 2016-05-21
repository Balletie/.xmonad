{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook(withUrgencyHook)
import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess, modifyLayout,
                                    redoLayout),
                                    ModifiedLayout(..))
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.Fullscreen
import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

import Data.Monoid
import Data.Map as M hiding (keys)

import ImageButtonHandlerDecoration (addHandledButtonTabs)
import LibNotifyUrgency (LibNotifyUrgencyHook(..))
import Prompts ( shellPrompt
               , openFilePrompt, openHiddenFilePrompt
               , execWithFilePrompt, execWithHiddenFilePrompt )
import Tabbed
import Themes (myNormalBorderColor, myFocusedBorderColor, myButtonedTheme)
import Util (isNotification, isSplash, startsWith)

main :: IO()
main = xmonad $ withUrgencyHook LibNotifyUrgencyHook
     $ myConfig

boilerPlateConfig = desktopConfig

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
  where opacity = 0.9

myStartupHook = do
  startupHook boilerPlateConfig
  ewmhDesktopsStartup
  setWMName "LG3D"
  -- Set left pointer of root screen
  spawn "xsetroot -cursor_name left_ptr"

myEventHook =
     docksEventHook
  <> fullscreenEventHook
  <> handleEventHook boilerPlateConfig

myManageHook =
     manageHook boilerPlateConfig
  <> fullscreenManageHook
  -- TODO: Merge surf windows with tabbed group
  -- <> (className =? "Surf" --> )
  <> (className =? "Eclipse" --> doShift "dev")
  <> (className =? "Pharo" --> doShift "dev")
  <> (className =? "Firefox" --> doShift "web")
  <> (className =? "Emacs" --> doShift "write")
  <> (className =? "Claws-mail" --> doShift "mail-chat")
  <> (title `startsWith` "IRC" --> doShift "mail-chat")
  <> (isNotification --> doIgnore)
  <> (isSplash --> doFloat)

myKeys = additionalKeys <> keys boilerPlateConfig
additionalKeys config@(XConfig { modMask = mod }) = M.fromList $
  [ ((noModMask, xK_Print)      , spawn "scrot -e 'mv $f ~/Desktop'")
  , ((controlMask, xK_F12)      , spawn "xmonad --recompile && xmonad --restart\
    \ && twmnc -i dialog-information -t Info -c \"XMonad recompiled and restarted\"\
    \ || twmnc -i dialog-error -t Error -c \"XMonad failed to compile\"")

  -- Prompt keybindings
  , ((mod, xK_p)                , shellPrompt)
  , ((mod, xK_d)                , openFilePrompt)
  , ((mod .|. shiftMask, xK_d)  , openHiddenFilePrompt)
  , ((mod, xK_f)                , execWithFilePrompt)
  , ((mod .|. shiftMask, xK_f)  , execWithHiddenFilePrompt)

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
