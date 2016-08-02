{-# LANGUAGE CPP #-}
module Colors (black, red, green, yellow, blue, magenta, cyan, white
              , backgroundColor, foregroundColor, focusedColor, normalColor
              , hiddenColor, urgentColor) where

-- Define XMONAD so that the color codes are strings.
#define XMONAD 1
#include "/home/skip/.colors/base16-defs"

black = base16_black
red = base16_red
green = base16_green
yellow = base16_yellow
blue = base16_blue
magenta = base16_magenta
cyan = base16_cyan
white = base16_white

backgroundColor = base16_back
foregroundColor = base16_fore
focusedColor = green
normalColor = foregroundColor
hiddenColor = yellow
urgentColor = red
