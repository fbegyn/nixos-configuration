
{ pkgs, ... }:

{
  home-manager.users.francis.services.redshift = {
    package = pkgs.unstable.redshift-wlr;
  };
}

