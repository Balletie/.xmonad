-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Directory
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A directory prompt for XMonad
--
-----------------------------------------------------------------------------

module CopyPasteMonad.Prompt.Directory (
                             -- * Usage
                             -- $usage
                             directoryPrompt,
                             Dir,
                              ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run ( runProcessWithInput )

import Control.Arrow ((>>>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.FilePath (splitDirectories)

-- $usage
-- For an example usage see "XMonad.Layout.WorkspaceDir"

data Dir = Dir String

instance XPrompt Dir where
    showXPrompt (Dir x) = x

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom = mkXPrompt (Dir prom) c getDirCompl

getDirCompl :: String -> IO [String]
getDirCompl s = (filter (notboring s) . lines) `fmap`
                runProcessWithInput "/run/current-system/sw/bin/bash" [] ("compgen -A directory " ++ s ++ "\n")

-- Does not show hidden files, unless input starts with dot.
notboring :: String -> String -> Bool
notboring input sugg = isPrevDir input || isHidden input || (not $ isHidden sugg)
  where isHidden  = pathSuffix >>> isPrefixOf "."
        isPrevDir = pathSuffix >>> ( == "..")

pathSuffix :: FilePath -> FilePath
pathSuffix = splitDirectories >>> lastMaybe >>> (fromMaybe "")

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs
