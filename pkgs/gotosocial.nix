{ pkgs }:

let
  pname = "gotosocial";
  version = "0.5.2";
in pkgs.stdenv.mkDerivation {
  inherit pname version;
  src = pkgs.fetchzip {
    url = "https://github.com/superseriousbusiness/${pname}/releases/download/v${version}/${pname}_${version}_linux_amd64.tar.gz";
    sha256 = "sha256-AfHXsQm0NHaqoyv7Jg6LHqzHmuahBiyAqHIBbY6rDJg=";
    stripRoot = false;
  };
  installPhase = ''
    mkdir -p "$out"/bin
    mv gotosocial $out/bin/
    mv web $out/
  '';
}
