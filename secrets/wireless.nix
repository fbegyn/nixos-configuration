{ config, pkgs, ... }:

{
  networking.wireless = {
    interfaces = ["wlp58s0"];
    extraConfig = ''
      ctrl_interface=/var/run/wpa_supplicant
      ctrl_interface_group=wheel
    '';
    networks = {
      "Begyn WiFi" = {
        psk = "B3gyn2331";
        priority = 50;
      };
      "Francis WiFi" = {
        psk = "FrancisOnTheGo";
        priority = 100;
      };
      "Zeus WPI" = {
        psk = "zeusisdemax";
        priority = 50;
      };
      "Zeus WPI 5G" = {
        psk = "zeusisdemax";
        priority = 60;
      };
      "Robovision" = {
        psk = "robo2018!";
        priority = 20;
      };
      "Kot Zonder Naam" = {
        psk = "wifizonderlorin";
        priority = 20;
      };
    };
  };
}
