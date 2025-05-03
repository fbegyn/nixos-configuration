let
  francis_mac = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOa6QQLv+FCgoMej4BUuQRzpGsmTiwIJnspamE0wrsM2";
  francis_bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV";
  users = [ francis_mac francis_bia ];

  erebus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPJDyIr/FSz1cJdcoW69R+NrWzwGK/+3gJpqD1t8L2zE";
  bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKzxQgondgEYcLpcPdJLrTdNgZ2gznOHCAxMdaceTUT1";
  selene = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKzxQgondgEYcLpcPdJLrTdNgZ2gznOHCAxMdaceTUT1";
  systems = [ erebus bia selene ];
in
{
  "secrets/wireless.age".publicKeys = users ++ systems;

  "secrets/api/cf.age".publicKeys = users ++ systems;
  "secrets/api/tailscale-temp.age".publicKeys = users ++ systems;

  "secrets/passwords/mail/francis.age".publicKeys = users ++ systems;
  "secrets/passwords/mail/marc.age".publicKeys = users ++ systems;
  "secrets/passwords/mail/dmarc.age".publicKeys = users ++ systems;
  "secrets/passwords/mail/bots.age".publicKeys = users ++ systems;
  "secrets/passwords/mail/robot.age".publicKeys = users ++ systems;
  "secrets/passwords/mqtt/hass.age".publicKeys = users ++ systems;
  "secrets/passwords/mqtt/shelly.age".publicKeys = users ++ systems;

  "secrets/services/alertmanager-env.age".publicKeys = users ++ systems;

  "secrets/data/borgbase/key.age".publicKeys = users ++ systems;
  "secrets/data/borgbase/ssh.age".publicKeys = users ++ systems;
}
