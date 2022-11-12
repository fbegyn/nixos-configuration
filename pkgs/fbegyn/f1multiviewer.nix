{ stdenv, lib, pkgs
, fetchurl
, alsaLib
, openssl
, zlib
, pulseaudio
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  name = "MultiviewerF1-${version}";
  pname = "MultiviewerF1";
  version = "1.9.8";

  system = "x86_64-linux";

  src = fetchurl {
    url =
      "https://releases.f1mv.com/download/82829340/MultiViewer.for.F1-linux-x64-${version}.zip";
    sha256 = "sha256-j1AOhOR9e0ckMfSWW6bi3CsQorBEcCMtkLZ+BQq+VFs=";
  };
  sourceRoot = ".";

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = with pkgs; [
    unzip
  ];

  unpackPhase = "true";

  # Extract and copy executable in $out/bin
  installPhase = ''
    mkdir -p $out
    unzip $src
    ls -al ./'MultiViewer for F1-linux-x64'
    install -d -m755 -D ./'MultiViewer for F1-linux-x64' $out/bin/multiviewer
  '';

  meta = with lib; {
    description = "Multiviewer for F1";
    homepage = https://beta.f1mv.com/;
    license = licenses.unfree;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}

