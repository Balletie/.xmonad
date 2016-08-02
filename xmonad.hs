{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook(withUrgencyHook)
import XMonad.Layout.BoringWindows (focusUp, focusDown)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook)
import XMonad.Layout.SubLayouts (GroupMsg(UnMerge, MergeAll), defaultSublMap, pullGroup)
import XMonad.Util.Run (spawnPipe)

import Data.Monoid
import Data.Map as M hiding (keys)

import LibNotifyUrgency (LibNotifyUrgencyHook(..))
import Prompts ( changeDirPrompt, shellPrompt, terminalPrompt
               , openFilePrompt , execWithFilePrompt )
import Layouts (myLayout)
import Colors (focusedColor, normalColor)
import Themes (myButtonedTheme)
import Util (isNotification, isSplash, startsWith)
import XMobar (xmobarLogHook)

main :: IO()
main = do
  xmobarproc <- spawnPipe "xmobar -d ~/.xmonad/xmobar.hs"
  xmonad $ myConfig {
    logHook = do
      logHook myConfig
      xmobarLogHook xmobarproc
  }

boilerPlateConfig = desktopConfig

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
  , ((mod .|. shiftMask, xK_p)  , terminalPrompt)
  , ((mod, xK_c)                , changeDirPrompt)
  , ((mod, xK_d)                , openFilePrompt)
  , ((mod, xK_f)                , execWithFilePrompt)

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
myConfig = withUrgencyHook LibNotifyUrgencyHook
         $ ewmh boilerPlateConfig {
    workspaces = ["\xf269", "\xf086", "\xf040", "\xf121"]
  , borderWidth = 1
  , normalBorderColor = normalColor
  , focusedBorderColor = focusedColor
  , modMask = mod4Mask
  , keys = myKeys
  , logHook = myLogHook
  , handleEventHook = myEventHook
  , startupHook = myStartupHook
  , manageHook = myManageHook
  , layoutHook = myLayout
  , terminal = "urxvtc"
  }
