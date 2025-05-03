{ config, ... }:

{

  ages.secrets."wireless" = {
    file = ../secrets/wireless.age;
  };

  networking.wireless = {
    extraConfig = ''
      ctrl_interface=/var/run/wpa_supplicant
      ctrl_interface_group=wheel
    '';
    secretsFile = config.age.secrets."wireless".path;
    networks = {
      "ext:home_ssid" = {
        priority = 100;
        psk = "ext:psk_home";
      };
      "ext:mobile_ssid" = {
        priority = 50;
        psk = "ext:psk_mobile";
        extraConfig = ''
          proto=RSN
          key_mgmt=SAE
          ieee80211w=2
          pairwise=CCMP
          group=CCMP
        '';
      };
      "Zeus WPI 5G" = {
        priority = 60;
        psk = "ext:psk_zeus";
        extraConfig = ''
          scan_ssid=1
        '';
      };
      "Zeus WPI" = {
        priority = 50;
        psk = "ext:psk_zeus";
        extraConfig = ''
          scan_ssid=1
        '';
      };
      "Zeus Event 5G" = {
        priority = 60;
        psk = "ext:psk_zeus";
        extraConfig = ''
          scan_ssid=1
        '';
      };
      "Take Five" = {
        priority = 5;
        psk = "ext:psk_take_five";
        extraConfig = ''
          disabled=1
        '';
      };
      "vooruit" = {
        priority = 1;
        extraConfig = ''
          key_mgmt=NONE
          disabled=1
        '';
      };
      "Krookwifi" = {
        priority = 1;
        extraConfig = ''
          key_mgmt=NONE
          disabled=1
        '';
      };
    };
  };
}
