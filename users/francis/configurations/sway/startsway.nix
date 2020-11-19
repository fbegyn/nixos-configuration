{ pkgs }:

pkgs.writeScriptBin "startsway" ''
  #! ${pkgs.fish}/bin/fish
  systemctl --user import-environment
  exec systemctl --user start sway
''
