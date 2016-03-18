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
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Util.Themes
import Data.Monoid
import Data.Map as M hiding (keys)

main :: IO()
main = do
  xmonad myConfig

boilerPlateConfig = desktopConfig

myTheme = theme smallClean

myLayout = avoidStruts $ fullscreenFull $ windowNavigation $ subTabbed $ boringWindows
         $ modifiedLayout ||| Full
  where
        modifiedLayout = smartBorders $ withBorder 2 $ smartSpacingWithEdge 4 $ layout
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

myEventHook =
     docksEventHook
  <> fullscreenEventHook
  <> handleEventHook boilerPlateConfig

isNotification :: Query Bool
isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

myManageHook =
     manageHook boilerPlateConfig
  <> fullscreenManageHook
  <> (className =? "Xfrun4" --> doFloat)
  -- Merge surf windows with tabbed group
  -- <> (className =? "Surf" --> )
  <> (className =? "Firefox" --> doShift "web")
  <> (className =? "Emacs" --> doShift "write")
  <> (className =? "Claws-mail" --> doShift "mail-chat")
  <> (isNotification --> doIgnore)

myStartupHook = do
  startupHook boilerPlateConfig
  spawn "xfce4-panel"
  -- spawn "xfce4-power-manager"
  spawn "xfce4-volumed"
  spawn "nm-applet"
  spawn "pcmanfm --desktop"
  spawn "compton"
  spawn "redshift-gtk -l 51.913799:4.468502 -t 6500:2500"

myKeys config@(XConfig { modMask = mod }) = additionalKeys <> defaultKeys
  where defaultKeys = keys boilerPlateConfig $ config
        additionalKeys = M.fromList $
          [ ((noModMask, xK_Print)      , spawn "scrot -e 'mv $f ~/Desktop'")
          , ((controlMask, xK_F12)      , spawn "xmonad --recompile && xmonad --restart && tput bel")

          -- These use boringWindows to skip over e.g. tabs when switching
          , ((mod, xK_k)                , focusUp)
          , ((mod, xK_j)                , focusDown)

          , ((mod, xK_s)                , submap $ defaultSublMap config)

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
  , normalBorderColor = "#cccccc"
  , focusedBorderColor = "#dddddd"
  , modMask = mod4Mask
  , keys = myKeys
  , logHook = myLogHook
  , handleEventHook = myEventHook
  , manageHook = myManageHook
  , startupHook = myStartupHook
  , layoutHook = myLayout
  , terminal = "urxvt"
  }
