{ pkgs, lib, stdenv, buildGoModule, fetchFromGitHub }:

let
  commit = "c85b8a21d47a8af9f6b88dd046149ed536c27e1f";
in buildGoModule rec {
  pname = "website";
  version = "${commit}";

  src = fetchFromGitHub {
    owner = "fbegyn";
    repo = "website";
    rev = "${commit}";
    sha256 = "1pk8w5w6ihccib5d6sca1n4vfr2z7hkj51gq0jdra08x2bbdsbmw";
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
