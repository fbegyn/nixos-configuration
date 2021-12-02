{ configs, pkgs, ... }:

{
  hardware.video.hidpi.enable = false;
  # services.xserver.dpi = 180;
  # environment.variables = {
  #   _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  #   GDK_SCALE = "1";
  #   GDK_DPI_SCALE = "0.5";
  #   # QT_AUTO_SCREEN_SET_FACTOR = "1";
  #   # QT_SCALE_FACTOR = "1";
  #   # QT_FONT_DPI = "32";
  # };
}
