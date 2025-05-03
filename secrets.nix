let
  francis_mac = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOa6QQLv+FCgoMej4BUuQRzpGsmTiwIJnspamE0wrsM2";
  francis_bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV";
  users = [ francis_mac francis_bia ];

  bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIErnN3QEyNE0JMsNLKKfRHck+Z3gJ43uYoQKHbM6jyas";
  systems = [ bia ];
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
  "secrets/services/hass-dcf.age".publicKeys = users ++ systems;
  "secrets/services/hass-dco.age".publicKeys = users ++ systems;

  "secrets/data/borgbase/key.age".publicKeys = users ++ systems;
  "secrets/data/borgbase/ssh.age".publicKeys = users ++ systems;
}
