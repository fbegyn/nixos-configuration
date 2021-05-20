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
    };
  fbegyn = {
    website = pkgs.callPackage ./fbegyn/website.nix {};
  };
}
