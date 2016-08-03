module Prompts ( changeDirPrompt, Prompts.shellPrompt, terminalPrompt
               , openFilePrompt , execWithFilePrompt
               ) where

import XMonad

import XMonad.Prompt (XPPosition(..), XPConfig(..),
                      amberXPConfig, greenXPConfig,
                      defaultXPConfig, mkXPrompt)
import XMonad.Prompt.Shell as Shell (shellPrompt, unsafePrompt
                                    , getCommands, getShellCompl, Shell(..))
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Util.Run (unsafeSpawn)

import CopyPasteMonad.Layout.WorkspaceDir (changeDir)
import Themes (Theme (..), myTheme)
import Colors

myPromptConfig = defaultXPConfig {
    font = fontName myTheme
  , height = decoHeight myTheme
  , bgColor = backgroundColor
  , fgColor = foregroundColor
  , bgHLight = backgroundColor
  , fgHLight = focusedColor
  , borderColor = backgroundColor
  , position = Top
  }

-- | Super shell prompt which is (a bit) more general :).
shellPromptV2 :: XPConfig -> (String -> X()) -> X ()
shellPromptV2 c spawnFun = do
    cmds <- io getCommands
    mkXPrompt Shell c (getShellCompl cmds $ searchPredicate c) spawnFun

changeDirPrompt :: X ()
changeDirPrompt = changeDir myPromptConfig

shellPrompt :: X ()
shellPrompt = shellPromptV2 myPromptConfig unsafeSpawn

openFilePrompt :: X ()
openFilePrompt = runOrRaisePrompt myPromptConfig -- spawn $ dmenu_browse ++ " | xargs xdg-open"

execWithFilePrompt :: X ()
execWithFilePrompt = unsafeSpawn $ "printf '%s \"%s\"' $(dmenu_path | dmenu " ++ dmenu_args ++ ") \"$(" ++ dmenu_browse ++ ")\" | /bin/sh"

terminalPrompt :: X ()
terminalPrompt = shellPromptV2 myPromptConfig runInTerm

runInTerm :: String -> X()
runInTerm cmd = unsafeSpawn $ "urxvtc -e " ++ cmd

-- dmenu_args :: [String]
-- dmenu_args = split isSpace ...
dmenu_args = " -nb '#cccccc' -sb '#dddddd'\
             \ -nf '#000000' -sf '#000000'\
             \ -fn 'xft:Sans:size=9'"

dmenu_run = "dmenu_run -p 'Run:' " ++ dmenu_args
dmenu_browse = "/home/skip/.local/bin/dmenu_browse /home/skip --dm " ++ dmenu_args
