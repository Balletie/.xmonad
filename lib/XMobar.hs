module XMobar (xmobarLogHook) where

import Control.Arrow ((>>>), (&&&), (***), first, second)
import Control.Monad.State (gets)
import Data.Monoid ((<>), mappend)
import Data.List (intercalate, isPrefixOf, reverse)
import System.FilePath (splitDirectories, joinPath)

import XMonad.Core (spawn, windowset, X)
import XMonad.StackSet (peek)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Hooks.DynamicLog ( dynamicLogString, xmobarPP, ppOutput, ppSep
                               , ppCurrent, ppHidden, ppHiddenNoWindows
                               , ppUrgent, ppLayout, ppTitle, xmobarColor
                               , wrap, shorten, xmobarStrip, xmonadPropLog)

import qualified Colors as C

-- Lol.
myPP = xmobarPP {
    ppLayout = words
               >>> head                  &&& (!! 1)
               >>> first shortenDir
               >>> withDirIcon           *** toLayoutIcon
               >>> openFileManagerAction *** switchLayoutAction
               >>> tupleToList >>> reverse >>> separated
  , ppTitle = foregroundColor . shorten 40 . xmobarStrip
  , ppCurrent = currentColor . formatWorkspace
  , ppHidden = hiddenColor . formatWorkspace
  , ppHiddenNoWindows = foregroundColor . formatWorkspace
  , ppUrgent = urgentColor . formatWorkspace
  , ppSep = "       "
  }
  where withDirIcon = (" " ++) >>> mappend (yellowColor $ fontAwesome "\xf07c")
        separated = intercalate $ ppSep myPP
        tupleToList (x, y) = [x, y]

xmobarLogHook = do
  xid <- currentWindowId
  let fmt = windowIconPathFormat
  path <- runProcessWithInput "/home/skip/.local/bin/window_xpm_icon" [ fmt, xid ] ""
  let withWindowIcon = ((iconFromPath $ init path)  ++) . (ppTitle myPP)
  dynamicLogString myPP { ppTitle = withWindowIcon } >>= xmonadPropLog

windowIconPathFormat :: String
windowIconPathFormat = toIconPath ("window_icon_%s")

currentWindowId :: X String
currentWindowId = do
  ws <- gets windowset
  let xid = maybe "" show $ peek ws
  return xid

-- | Shortens the intermediate directories to only the first letter of their
-- name. Example: `/home/skip/Pictures/foo` becomes `~/P/foo/`.
shortenDir dir = joinPath $ shortened ++ [lastDir ++ "/"]
  where directories = splitDirectories replacedWithHomeDir
        replacedWithHomeDir = replaceHomeDir dir
        firstDirs = init directories
        shortened = map (take 1) $ firstDirs
        lastDir = last directories

replaceHomeDir string | "/home/skip" `isPrefixOf` string = '~' : (drop 10 string)
                      | otherwise = string

currentColor = xmobarColor C.focusedColor ""
hiddenColor = xmobarColor C.hiddenColor ""
urgentColor = xmobarColor C.urgentColor ""
foregroundColor = xmobarColor C.normalColor ""
greenColor = xmobarColor C.green ""
yellowColor = xmobarColor C.yellow ""
redColor = xmobarColor C.red ""
fontAwesome = wrap "<fn=1>" "</fn>"

-- | Translates the layout string description to an icon.
toLayoutIcon :: String -> String
toLayoutIcon "Full" = icon "layout_full"
toLayoutIcon "Vert" = icon "layout_tall"
toLayoutIcon "Hori" = icon "layout_mirror_tall"
toLayoutIcon other = other

-- | Translate the workspace identifier to an icon.
toWorkspaceIcon :: String -> String
toWorkspaceIcon = fontAwesome . iconForId
  where iconForId "web"       = "\xf269" -- Firefox logo
        iconForId "mail-chat" = "\xf086" -- Conversation icon
        iconForId "write"     = "\xf040" -- Pencil icon
        iconForId "dev"       = "\xf121" -- "</>" icon
        iconForId _           = "\xf059" -- Question mark icon

-- | Translate the workspace identifier to a switch action
toSwitchAction :: String -> String -> String
toSwitchAction "web"       = switchAction 1
toSwitchAction "mail-chat" = switchAction 2
toSwitchAction "write"     = switchAction 3
toSwitchAction "dev"       = switchAction 4
toSwitchAction x           = \_ -> x

switchAction i = wrap (wrap "<action=`xdotool key super+" "` button=1>" $ show i)
                      "</action>"

openFileManagerAction = wrap "<action=`pcmanfm` button=1>" "</action>"
switchLayoutAction = wrap "<action=`xdotool key super+space` button=1>" "</action>"

formatWorkspace :: String -> String
formatWorkspace = toSwitchAction &&& toWorkspaceIcon >>> uncurry ($)

icon = iconFromPath . toIconPath
iconFromPath = wrap "<icon=" "/>"

toIconPath = wrap "/home/skip/.xmonad/icons/" ".xpm"
