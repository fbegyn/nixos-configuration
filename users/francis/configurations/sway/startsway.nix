{ pkgs }:

pkgs.writeTextFile {
  name = "startsway";
  destination = "/bin/startsway";
  executable = true;
  text = ''
    #! ${pkgs.bash}/bin/bash
    systemctl --user import-environment
    exec systemctl --user start sway
  '';
}
