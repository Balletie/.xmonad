module Prompts ( changeDirPrompt, shellPrompt
               , openFilePrompt, openHiddenFilePrompt
               , execWithFilePrompt, execWithHiddenFilePrompt
               ) where

import XMonad
import XMonad.Core (spawn)
import XMonad.Prompt (XPPosition(..), XPConfig(..),
                      amberXPConfig, greenXPConfig,
                      defaultXPConfig)
import qualified XMonad.Prompt.Shell as Shell (shellPrompt)
import XMonad.Layout.WorkspaceDir (changeDir)
import Themes (Theme (..), myTheme)

myPromptConfig = defaultXPConfig {
    font = fontName myTheme
  , height = decoHeight myTheme
  , bgColor = inactiveColor myTheme
  , fgColor = inactiveTextColor myTheme
  , bgHLight = activeColor myTheme
  , fgHLight = activeTextColor myTheme
  , borderColor = activeBorderColor myTheme
  , position = Top
  }

changeDirPrompt :: X ()
changeDirPrompt = changeDir myPromptConfig

shellPrompt :: X ()
shellPrompt = Shell.shellPrompt myPromptConfig

openFilePrompt :: X ()
openFilePrompt = spawn $ dmenu_browse ++ " | xargs xdg-open"

openHiddenFilePrompt :: X ()
openHiddenFilePrompt = spawn $ dmenu_browse ++ " --ls -A | xargs xdg-open"

execWithFilePrompt :: X ()
execWithFilePrompt = spawn $ "printf '%s \"%s\"' $(dmenu_path | dmenu " ++ dmenu_args ++ ") \"$(" ++ dmenu_browse ++ ")\" | /bin/sh"

execWithHiddenFilePrompt :: X ()
execWithHiddenFilePrompt = spawn $ "printf '%s \"%s\"' $(dmenu_path | dmenu " ++ dmenu_args ++ ") \"$(" ++ dmenu_browse ++ " --ls -A)\" | /bin/sh"

-- dmenu_args :: [String]
-- dmenu_args = split isSpace ...
dmenu_args = " -nb '#cccccc' -sb '#dddddd'\
             \ -nf '#000000' -sf '#000000'\
             \ -fn 'xft:Sans:size=9'"

dmenu_run = "dmenu_run -p 'Run:' " ++ dmenu_args
dmenu_browse = "/home/skip/.local/bin/dmenu_browse /home/skip --dm " ++ dmenu_args