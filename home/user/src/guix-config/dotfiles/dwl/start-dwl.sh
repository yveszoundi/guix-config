#!/usr/bin/env bash

export XDG_SESSION_TYPE="wayland"
export WLR_NO_HARDWARE_CURSORS="1"
export WLR_RENDERER_ALLOW_SOFTWARE="0"
export ELM_ENGINE="wayland_egl"
export MOZ_ENABLE_WAYLAND="1"
export GDK_BACKEND="wayland"

exec dbus-run-session dwl -s "dwlb  -ipc -font 'monospace:size=14'"
