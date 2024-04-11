{ pkgs }:

pkgs.writeScriptBin "sway-statusbar.sh" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail
  # Terminate already running bar instances
  PID=$(ps aux | grep "bin/waybar" | head -n 1 | awk '{print $2}')
  kill $PID || true
  # Wait until the processes have been shut down
  while pgrep -x waybar >/dev/null; do sleep 1; done
  # Launch main
  ${pkgs.waybar}/bin/waybar
''
