{ pkgs, ... }:

{
  home-manager.users.francis.services.redshift = {
    enable = true;
    package = pkgs.unstable.redshift-wlr;
    latitude = "51.05389";
    longitude = "3.705";
  };
}

