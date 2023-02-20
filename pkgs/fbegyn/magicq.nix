{ pkgs ? import <nixpkgs>, lib ? pkgs.lib, stdenv}:

let
  version = "1.9.3";
  src = ./magicq_ubuntu_v1_9_3_7.deb;
in stdenv.mkDerivation {
  pname = "mqqt";
  version = "v${version}";

  system = "x86_64-linux";

  inherit src;
  nativeBuildInputs = with pkgs; [
    dpkg
    autoPatchelfHook
    xorg.libX11
    xorg.libXi
    libGL
    libGLU
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-bad
    cups.lib
    libarchive
    db
  ];

  buildInputs = with pkgs; [
    glibc
    gcc-unwrapped
  ];

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out
    dpkg -x $src $out
    ls -al $out
  '';

  meta = with lib; {
    description = "Chamsys MagicQ PC";
    homepage = "https://chamsyslighting.com/pages/magicq-downloads";
    license = licenses.mit;
    maintainers = with maintainers; [ fbegyn ];
    platforms = platforms.linux;
  };
}
