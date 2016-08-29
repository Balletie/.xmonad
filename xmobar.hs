-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
-- Colors
--   bg:     #181818
--   fg:     #e8e8e8
--   red:    #ab4642
--   yellow: #f7ca88
--   orange: #dc9656
--   green:  #a1b56c

Config {
    font = "xft:RobotoCondensed:size=11",
    additionalFonts = ["xft:FontAwesome:size=10"],
    bgColor = "#181818",
    fgColor = "#d8d8d8",
    alpha = 255,
    position = Bottom,
    commands = [
        -- battery monitor
        Run BatteryP [ "BAT0" ] [
            "-t"         , "<acstatus>",
            "--Low"      , "10",       -- units: %
            "--High"     , "80",       -- units: %
            "-l"         , "#d8d8d8",
            "-h"         , "#d8d8d8",
            "-n"         , "#d8d8d8",
            "-p"         , "3",
            "--", -- battery specific options
            "-l"         , "#ab4642",
            "-m"         , "#f7ca88",
            "-h"         , "#a1b56c",
            "-p"         , "#f7ca88",
            -- discharging status
            "-o"	, "<fc=#dc9656><fn=1>\xf240</fn></fc> <left>%",
            -- AC "on" status
            "-O"	, "<fc=#f7ca88><fn=1>\xf0e7</fn></fc> <left>%",
            -- charged status
            "-i"	, "<fc=#a1b56c><fn=1>\xf1e6</fn></fc> <left>%",
            "-f"  , "ADP1/online"
            ] 5,
        Run Volume "default" "Master" [
            "-t", "<status> <volume>%",
            "-p", "3",
            "--",
            "-C", "#a1b56c",
            "-c", "#ab4642",
            "-o", "<fn=1>\xf026</fn>",
            "-O", "<fn=1>\xf028</fn>"
            ] 1,
        Run Brightness [
            "-t", "<fc=#a1b56c><fn=1>\xf185</fn></fc> <percent>%",
            "--",
            "-D", "gmux_backlight"
            ] 1,
        Run Date "%a %_d %b %H:%M" "date" 10,
        Run UnsafeStdinReader,
        Run Com "/home/skip/.xmonad/stalonetray-padding-icon.sh" [] "stalonetraypad" 10
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "  %UnsafeStdinReader% } %date% { %bright% %default:Master% %battery% %stalonetraypad%"
}

