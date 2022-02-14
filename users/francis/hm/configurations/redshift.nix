{ pkgs, ... }:

{
  services.redshift = {
    enable = true;
    latitude = "51.05389";
    longitude = "3.705";
  };
}

