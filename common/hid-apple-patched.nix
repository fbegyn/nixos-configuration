{ pkgs, kernel, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "hid-apple-patched";
  name = "hid-apple-patched";

  src = pkgs.fetchFromGitHub {
    owner = "free5lot";
    repo = "hid-apple-patched";
    rev = "40bd3163173b09543d9b828621a9a38890e9a54c";
    sha256 = "0imi2a0lcbzm1wsadwd8a3kiszz4d3snmp13y5zkqhls0wkk4fhn";
  };

  buildInput = [
    kernel.moduleBuildDependencies
  ];

  LINUX_HEADER_DIR = "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build";
  KERNELDIR = "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build";

  preConfigure = ''
    export INSTALL_MOD_PATH="$out/lib/modules/${kernel.modDirVersion}/kernel/drivers/hid";
    mkdir -p $out/lib/modules/${kernel.modDirVersion}/kernels/drivers/hid
  '';


  meta = with pkgs.stdenv.lib; {
    homepage = "https://github.com/free5lot/hid-apple-patched";
    description = "Patched Apple HID";
    platforms = platforms.linux;
    license = with licenses; [ gpl2 ];
  };
}
