{ pkgs, lib, stdenv, buildGoModule, fetchFromGitHub }:

let
  commit = "162233dd9c07300057bbdb0ef57c002a39064fdb";
in buildGoModule rec {
  pname = "website";
  version = "${commit}";

  src = fetchFromGitHub {
    owner = "fbegyn";
    repo = "website"; 
    rev = "${commit}";
    sha256 = "0bspa5zclsd4588cibhpqsvgg6vcrawi1j4fm571hdnr45cygih8";
  };

  vendorSha256 = null;
  subPackages = [ "./cmd/server" ];
  meta = with lib; {
    description = "fbegyn's website server";
    homepage = "https://github.com/fbegyn/website";
    license = licenses.mit;
    maintainers = with maintainers; [ fbegyn ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
