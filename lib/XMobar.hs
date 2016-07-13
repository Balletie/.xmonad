module XMobar (xmobarLogHook) where

import Control.Arrow ((>>>), (&&&), (***), first, second)
import Data.Monoid ((<>), mappend)
import Data.List (intercalate, isPrefixOf, reverse)
import System.FilePath (splitDirectories, joinPath)

import XMonad.Util.Run (hPutStrLn)
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP, xmobarPP, ppOutput, ppSep
                               , ppCurrent, ppHidden, ppHiddenNoWindows
                               , ppLayout, xmobarColor, wrap)

xmobarLogHook xmobarproc = dynamicLogWithPP $ xmobarPP {
    ppOutput = hPutStrLn xmobarproc
  , ppLayout = words >>> head &&& (!! 1)
               >>> first shortenDir >>> withDirIcon *** toLayoutIcon
               >>> tupleToList >>> reverse >>> separated
  , ppCurrent = currentColor . fontAwesome
  , ppHidden = hiddenColor . fontAwesome
  , ppHiddenNoWindows = fontAwesome
  }
  where withDirIcon = (" " ++) >>> mappend (yellowColor $ fontAwesome "\xf07c")
        separated = intercalate $ ppSep xmobarPP
        tupleToList (x, y) = [x, y]

shortenDir dir = joinPath $ shortened ++ [lastDir ++ "/"]
  where directories = splitDirectories replacedWithHomeDir
        replacedWithHomeDir = replaceHomeDir dir
        firstDirs = init directories
        shortened = map (take 1) $ firstDirs
        lastDir = last directories

replaceHomeDir string | "/home/skip" `isPrefixOf` string = '~' : (drop 10 string)
                      | otherwise = string

currentColor = greenColor
hiddenColor = yellowColor
greenColor = xmobarColor "green" ""
yellowColor = xmobarColor "yellow" ""
fontAwesome = wrap "<fn=1>" "</fn>"

-- | Translates the layout string description to an icon.
toLayoutIcon :: String -> String
toLayoutIcon "Full" = icon "layout_full"
toLayoutIcon "Vert" = icon "layout_tall"
toLayoutIcon "Hori" = icon "layout_mirror_tall"
toLayoutIcon other = other

icon = wrap "<icon=/home/skip/.xmonad/icons/" ".xbm/>"
