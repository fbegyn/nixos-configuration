{ pkgs , ...}:

{
  environment.systemPackages = with pkgs; [
    guvcview
  ];
}
