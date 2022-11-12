pkgs: rec {
  nur = import (builtins.fetchTarball
    "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  unstable = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      inherit pkgs;
      config.allowUnfree = true;
      overlays = [
        (import ../overlays/weechat.nix)
        (import ../overlays/browser-eid.nix)
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
    };
  stable = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-22.05.tar.gz") {
      inherit pkgs;
      config.allowUnfree = true;
      overlays = [
        (import ../overlays/weechat.nix)
        (import ../overlays/browser-eid.nix)
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
    };
  master = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {
      inherit pkgs;
      overlays = [
        (import ../overlays/browser-eid.nix)
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
    };
  fbegyn = {
    website = pkgs.callPackage ./fbegyn/website.nix {};
    f1multiviewer = pkgs.callPackage ./fbegyn/f1multiviewer.nix {};
  };
}
