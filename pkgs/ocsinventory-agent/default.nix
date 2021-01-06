{ stdenv, perlPackages, fetchurl, pkgs }:

let
  # The perl packages for Proc:Daemon and Proc:File:PID are no available in the
  # nixpkgs. We build them ourselves here
  ProcDaemon = perlPackages.buildPerlPackage {
    pname = "Proc-Daemon";
    version = "0.23";
    src = fetchurl {
      url = "mirror://cpan/authors/id/A/AK/AKREAL/Proc-Daemon-0.23.tar.gz";
      sha256 = "34c0b85b7948b431cbabc97cee580835e515ccf43badbd8339eb109474089b69";
    };
    buildInputs = [ perlPackages.ProcProcessTable ];
    meta = {
      homepage = https://github.com/akreal/Proc-Daemon;
      description = "Run Perl program(s) as a daemon process";
      license = with stdenv.lib.licenses; [ artistic1 gpl1Plus ];
    };
  };
in
perlPackages.buildPerlPackage rec {
  pname = "ocsinventory-agent";
  version = "2.8.1";
  src = fetchurl {
    url = "https://github.com/OCSInventory-NG/UnixAgent/releases/download/v${version}/Ocsinventory-Unix-Agent-${version}.tar.gz";
    sha256 = "1jrb3n9iy6nhi0i7gfby4a1r5w5jyahh10f9vl0agddckhvhym0h";
  };

  buildInputs = [
    pkgs.dmidecode
    pkgs.pciutils
    ProcDaemon
    perlPackages.perl
    perlPackages.NetIP
    perlPackages.NetSNMP
    perlPackages.NetNetmask
    perlPackages.XMLSimple
    perlPackages.LWP
    perlPackages.LWPProtocolhttps
    perlPackages.CryptSSLeay
    perlPackages.ProcProcessTable
  ];

  # dmidecode and pciutils provide runtime dependencies
  propagatedBuildInputs = [
    pkgs.dmidecode
    pkgs.pciutils
  ];

  meta = {
    homepage = https://github.com/OCSInventory-NG/UnixAgent;
    description = "OCS unified agent for Unix operating systems";
    license = with stdenv.lib.licenses; [ gpl2 ];
  };
}
