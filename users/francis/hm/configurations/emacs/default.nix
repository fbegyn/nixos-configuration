{ pkgs, ... }:

let
  e = pkgs.writeTextFile {
    name = "francis-emacs.desktop";
    destination = "/share/applications/francis-emacs.desktop";
    text = ''
[Desktop Entry]
Exec=emacsclient -nc
Icon=emacs
Name[en_US]=Emacs Client
Name=Emacs Client
StartupNotify=true
Terminal=false
Type=Application
      '';
    };
    nur-no-pkgs = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {};
in
{
  imports = [
    nur-no-pkgs.repos.rycee.hmModules.emacs-init
  ];

  home.packages = [
    pkgs.ispell
    e
  ];

  home.file = {
    ".local/bin/e" = {
      text = ''
        #!/bin/sh
        emacsclient -t -a "" $@
      '';
      executable = true;
    };
    ".local/bin/ew" = {
      text = ''
        #!/bin/sh
        emacsclient -a "" -nc $@
      '';
      executable = true;
    };
  };

  services.emacs.package = pkgs.unstable.emacsUnstable;
  programs.emacs = {
    package = pkgs.unstable.emacsUnstable;
    enable = true;
    init = import ./emacs.nix { inherit pkgs; };
  };
  xresources.properties = {
    "Emacs.font" = "DejaVu Sans Mono-16";
  };
}
