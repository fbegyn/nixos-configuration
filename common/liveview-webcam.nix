{ pkgs , ...}:

{
  services.udev = {
    extraRules = ''
      SUBSYSTEM=="video4linux", KERNEL=="video[0-9]*", ATTRS{product}=="Microsoft® LifeCam HD-3000", RUN+="${pkgs.v4l-utils}/bin/v4l2-ctl -d $devnode --set-ctrl=backlight_compensation=6"
    '';
  };
}
