{ nixpkgs, ... }:

{
  mkContainerNetworks = ip: {
    "10-tailscale0" = {
      matchConfig.Name = "tailscale*";
      linkConfig = {
	      Unmanaged = "yes";
        RequiredForOnline = "no";
      };
    };
    "30-veth0" = {
      matchConfig.Name = "veth0";
      address = [ "10.5.1.${ip}/24" ];
      routes = [
        { Gateway = "10.5.1.5"; }
      ];
      vlan = [
        "iot"
      ];
      networkConfig.DHCP = "ipv6";
      linkConfig.RequiredForOnline = "carrier";
    };
    "190-iot" = {
      matchConfig.Name = "mgmt";
      address = [ "10.5.90.${ip}/24" ];
      routes = [
        {
          Destination = "10.5.90.0/24";
          Table = "190";
        }
      ];
      networkConfig.DHCP = "ipv6";
      routingPolicyRules = [
        {
          To = "10.5.90.0/24";
          Table = "190";
        }
      ];
      linkConfig.RequiredForOnline = "routable";
    };
  };
  mkContainerNetdevs = {
    "190-iot" = {
      netdevConfig = { Kind = "vlan"; Name = "iot"; };
      vlanConfig.Id = 190;
    };
  };
}
