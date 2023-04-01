#!/usr/bin/env bash

wlr-randr --output "Virtual-1" --mode 1440x900
# mkdir -p ~/Pictures && curl -L -o wallpaper.png  https://w.wallha.com/ws/13/kEOo167z.png
swaybg -i $HOME/Pictures/wallpaper.png -m fill -o "*" &
