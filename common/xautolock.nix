{pkgs, config, ...}:

{
  environment.systemPackages = with pkgs.unstable; [
    xautolock
  ];
  services.xserver.xautolock = {
    enable = true;
    time = 2;
    locker = "${pkgs.unstable.betterlockscreen}/bin/betterlockscreen --off 300 -l";
    nowlocker = "${pkgs.unstable.betterlockscreen}/bin/betterlockscreen --off 300 -l";
    notifier = "${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'";
  };
}
