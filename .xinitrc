#!/usr/bin/env bash
xset s blank
xset s 600
xset dpms 1200 1800 2400
xsetroot -solid black
setxkbmap dvorak
[ -e ~/.Xmodmap ] && xmodmap ~/.Xmodmap
xterm -e screen -R &
exec stumpwm
