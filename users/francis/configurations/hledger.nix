{ pkgs, ... }:

{
  home-manager.users.francis = {
    home.packages = with pkgs; [
      unstable.ledger
      unstable.hledger
    ];
  };
}
