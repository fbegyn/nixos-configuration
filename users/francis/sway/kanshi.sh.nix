{ pkgs }:

pkgs.writeScriptBin "start-kanshi.sh" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail
  # Terminate already running kanshi instances
  PID=$(ps aux | grep "bin/kanshi" | head -n 1 | awk '{print $2}')
  kill $PID || true
  # Wait until the processes have been shut down
  while pgrep -x kanshi >/dev/null; do sleep 1; done
  # Launch main
  ${pkgs.kanshi}/bin/kanshi
''
