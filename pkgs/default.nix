pkgs: rec {
  nur = import (builtins.fetchTarball
    "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  master = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {
      inherit pkgs;
      config.allowUnfree = true;
    };
  unstable = import (builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      inherit pkgs;
      config.allowUnfree = true;
      overlays = [
        (import ../overlays/weechat.nix)
        (import ../overlays/browser-eid.nix)
      ];
    };
  fbegyn = {
    website = pkgs.callPackage ./fbegyn/website.nix {};
  };
}
