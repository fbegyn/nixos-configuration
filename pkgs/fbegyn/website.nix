{ pkgs, lib, stdenv, buildGoModule, fetchFromGitHub }:

let
  commit = "21531076838f52611cd3e9587e845d448447aa28";
in buildGoModule rec {
  pname = "website";
  version = "${commit}";

  src = fetchFromGitHub {
    owner = "fbegyn";
    repo = "website"; 
    rev = "${commit}";
    sha256 = "0v00dh3239jmdw153yqmb1yjbna67f75j5qinjp3xzybzpx41c7c";
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
