{ pkgs }:

pkgs.writeScriptBin "niri-statusbar.sh" ''
  #!${pkgs.bash}/bin/bash
  # Terminate already running bar instances
  pkill waybar || true
  # Wait until the processes have been shut down
  while pgrep -x waybar >/dev/null; do sleep 1; done
  # Launch main
  ${pkgs.waybar}/bin/waybar -c $1
''
