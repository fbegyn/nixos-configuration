{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [
    ledger
    hledger
  ];
}
