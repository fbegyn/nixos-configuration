{ pkgs }:

pkgs.writeTextFile {
  name = "startsway";
  destination = "/bin/startsway";
  executable = true;
  text = ''
    #! ${pkgs.bash}/bin/bash

    if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        exec sway
    fi
  '';
}
