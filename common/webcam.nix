{ pkgs , ...}:

{
  environment.systemPackages = with pkgs; [
    guvcview
    ffmpeg
    v4l-utils
  ];
}
