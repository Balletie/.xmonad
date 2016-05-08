module Util where

import Data.List

import XMonad.Core
import XMonad.Hooks.ManageHelpers (isInProperty)

isNotification :: Query Bool
isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

startsWith :: Eq a => Query [a] -> [a] -> Query Bool
qa `startsWith` a = qa >>= return . (isPrefixOf a)

endsWith :: Eq a => Query [a] -> [a] -> Query Bool
qa `endsWith` a = qa >>= return . (isSuffixOf a)
