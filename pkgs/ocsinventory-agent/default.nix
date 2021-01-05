{ stdenv, perlPackages, fetchurl, pkgs }:

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
    perlPackages.perl
    perlPackages.NetIP
    perlPackages.NetSNMP
    perlPackages.NetNetmask
    perlPackages.XMLSimple
    perlPackages.LWP
    perlPackages.LWPProtocolhttps
    perlPackages.CryptSSLeay
    perlPackages.HTTPDaemon
  ];
}
