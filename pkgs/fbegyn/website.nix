{ pkgs ? import <nixpkgs>, buildGoModule ? pkgs.buildGoModule, fetchFromGitHub ? pkgs.fetchFromGitHub, lib ? pkgs.lib}:

let
  commit = "e166074b97bd496fd4c46501286d9838b7353213";
in buildGoModule {
  pname = "website";
  version = "${commit}";

  src = fetchFromGitHub {
    owner = "fbegyn";
    repo = "website";
    rev = "${commit}";
    sha256 = "0l8z6zy3carh91pwhn4amwpdmjiac8ib4zx0x37lkc5n85g4dc9l";
  };

  vendorSha256 = null;
  meta = with lib; {
    description = "fbegyn's website server";
    homepage = "https://github.com/fbegyn/website";
    license = licenses.mit;
    maintainers = with maintainers; [ fbegyn ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
