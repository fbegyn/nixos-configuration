{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.ledger
    unstable.hledger
  ];
}
