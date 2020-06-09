{ pkgs }:

pkgs.writeScriptBin "startsway" ''
  #!${pkgs.bash}/bin/bash
  systemctl --user import-environment
  systemctl --user start sway
''
