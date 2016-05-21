module Themes (myNormalBorderColor, myFocusedBorderColor
              , myTheme, myButtonedTheme, Theme (..)
              ) where
import XMonad.Layout.Decoration(Theme(..), Decoration, DefaultShrinker)
import XMonad.Layout.ImageButtonDecoration

myNormalBorderColor = "#BDBDBD"
myFocusedBorderColor = "#FFCD07"

myButtonedTheme = defaultThemeWithImageButtons {
    fontName = "xft:Sans-9:bold"
  , decoHeight = 20
  , activeColor = "#FFDA4D"
  , inactiveColor = "#EDEDED"
  , urgentColor = activeColor myButtonedTheme
  , activeBorderColor = myFocusedBorderColor
  , inactiveBorderColor = myNormalBorderColor
  , urgentBorderColor = activeBorderColor myButtonedTheme
  , activeTextColor = "#000000"
  , inactiveTextColor = "#515151"
  , urgentTextColor = activeTextColor myButtonedTheme
  }

myTheme = myButtonedTheme {
    windowTitleAddons = []
  , windowTitleIcons  = []
  }
