module LibNotifyUrgency (LibNotifyUrgencyHook(..)) where

import XMonad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook (UrgencyHook(..))

-- Stolen from Hendrik's config.
-- Notifications for urgent windows
-- as per http://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "In " ++ idx, "--icon=important" ]
