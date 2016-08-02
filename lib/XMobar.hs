module XMobar (xmobarLogHook) where

import Control.Arrow ((>>>), (&&&), (***), first, second)
import Data.Monoid ((<>), mappend)
import Data.List (intercalate, isPrefixOf, reverse)
import System.FilePath (splitDirectories, joinPath)

import XMonad.Util.Run (hPutStrLn)
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP, xmobarPP, ppOutput, ppSep
                               , ppCurrent, ppHidden, ppHiddenNoWindows
                               , ppUrgent, ppLayout, ppTitle, xmobarColor
                               , wrap, shorten)

import qualified Colors as C

-- Lol.
myPP = xmobarPP {
    ppLayout = words >>> head &&& (!! 1)
               >>> first shortenDir >>> withDirIcon *** toLayoutIcon
               >>> tupleToList >>> reverse >>> separated
  , ppTitle = foregroundColor . shorten 40
  , ppCurrent = currentColor . fontAwesome
  , ppHidden = hiddenColor . fontAwesome
  , ppHiddenNoWindows = foregroundColor . fontAwesome
  , ppUrgent = urgentColor . fontAwesome
  , ppSep = "       "
  }
  where withDirIcon = (" " ++) >>> mappend (yellowColor $ fontAwesome "\xf07c")
        separated = intercalate $ ppSep myPP
        tupleToList (x, y) = [x, y]

xmobarLogHook xmobarproc = dynamicLogWithPP $ myPP {
    ppOutput = hPutStrLn xmobarproc
  }

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

icon = wrap "<icon=/home/skip/.xmonad/icons/" ".xbm/>"
