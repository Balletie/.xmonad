-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

Config {
    font = "xft:Dina:size=10",
    additionalFonts = ["xft:NotoEmoji:size=10"],
    bgColor = "black",
    fgColor = "grey",
    position = BottomP 0 100,
    commands = [
        -- battery monitor
        Run BatteryP [ "BAT0" ] [
            "-t"         , "<acstatus>",
            "--Low"      , "10",       -- units: %
            "--High"     , "80",       -- units: %
            "-l"         , "red",
            "-p"         , "3",
            "--", -- battery specific options
            "-l"         , "red",
            "-m"         , "orange",
            "-h"         , "green",
            "-p"         , "yellow",
            -- discharging status
            "-o"	, "<fc=orange><fn=1>🔋</fn></fc><left>%",
            -- AC "on" status
            "-O"	, "<fc=yellow><fn=1>🔋</fn></fc><left>%",
            -- charged status
            "-i"	, "<fc=green><fn=1>🔋</fn><left>%</fc>",
            "-f"  , "ADP1/online"
            ] 5,
        Run Volume "default" "Master" [
            "-t", "<status> <volume>%",
            "-p"         , "3",
            "--",
            "-o", "<fn=1>🔇</fn>",
            "-O", "<fn=1>🔊</fn>"
            ] 1,
        Run Date "%a %_d %b %H:%M" "date" 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "  %StdinReader% } %date% { %default:Master% %battery% "
}

