#!/usr/bin/env bash

# Terminate already running bar instances
pkill sway || true

# Wait until the processes have been shut down
while pgrep -x waybar >/dev/null; do sleep 1; done

# Launch main
waybar
