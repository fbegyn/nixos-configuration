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
  version = "1.11.0";

  system = "x86_64-linux";

  src = fetchurl {
    url =
      "https://releases.f1mv.com/download/82829340/MultiViewer.for.F1-linux-x64-${version}.zip";
    sha256 = "sha256-j1AOhOR9e0ckMfSWW6bi3CsQorBEcCMtkLZ+BQq+VFs=";
  };
  sourceRoot = ".";

  nativeBuildInputs = with pkgs; [
    glib
    nss_latest
    at-spi2-atk
    xorg.libX11
    xorg.libXdamage
    mesa
    alsa-lib
    wayland
    cups
    gtk3-x11
    libdrm
  ];

  buildInputs = [
    pkgs.unzip
    autoPatchelfHook
  ];

  unpackPhase = "true";

  # Extract and copy executable in $out/bin
  installPhase = ''
    mkdir -p $out
    unzip $src
    mv ./'MultiViewer for F1-linux-x64' ./src
    cd ./src
    mv ./'MultiViewer for F1' ./multiviewer-for-f1
    for srcFile in $(ls ./); do
      cp -r $srcFile $out/
    done
  '';

  meta = with lib; {
    description = "Multiviewer for F1";
    homepage = https://beta.f1mv.com/;
    license = licenses.unfree;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}

