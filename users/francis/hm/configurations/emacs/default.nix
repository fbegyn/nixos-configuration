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
in
{
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

  xdg.configFile = {
    "emacs/init.el".source = ./init.el;
    "emacs/early-init.el".source = ./early-init.el;
  };

  # services.emacs = {
  #   enable= true;
  #   package = pkgs.emacsWithPackagesFromUsePackage;
  #   client.enable = true;
  #   socketActivation.enable = true;
  # };
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacsPgtkNativeComp;
      alwaysEnsure = true;
      # extraEmacsPackages = epkgs: [];
    };
  };

  xresources.properties = {
    "Emacs.font" = "DejaVu Sans Mono-16";
  };
}
