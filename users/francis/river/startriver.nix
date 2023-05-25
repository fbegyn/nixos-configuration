{ pkgs }:

pkgs.writeTextFile {
  name = "startriver";
  destination = "/bin/startriver";
  executable = true;
  text = ''
    #! ${pkgs.bash}/bin/bash

    if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty2" ]; then
        exec river
    fi
  '';
}
