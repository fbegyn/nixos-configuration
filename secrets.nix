let
  francis_mac = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOa6QQLv+FCgoMej4BUuQRzpGsmTiwIJnspamE0wrsM2";
  francis_bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINK7mMVKOmELe+FVvn1oWNRwKiANgTwcnzte3vWK3nMV";
  users = [ francis_mac francis_bia ];

  bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIErnN3QEyNE0JMsNLKKfRHck+Z3gJ43uYoQKHbM6jyas";
  selene = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB9nISfOAr6o/O9xLlmxeQHA0Q+Az7T+S+AQnNOTy1Jl";
  hosting-01 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPPxGLkIK1dVPcOyppsOk6im9qKwh3qKgv52Hhl4xdeT";
  mail-01 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILG/9d08f6x5F2Ub0KmooXoDJ2fWGN0UjhVDke0GcmN3";
  app-01 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ17ijpDXRzRqYpXRNUjDs9ZtdVmwhY5/N16aooOypPI";
  proxy-01 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDhtr0TU/pPOFsJWRZgVpqFs+Frhee2/5hwb1gF35xck";
  infra-01 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOKmvJ1IOOQrCCZrD7wTgiMgPEBS71oY5CpCnrYLBCCj";
  systems = [ bia selene hosting-01 mail-01 app-01 proxy-01 infra-01 ];
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
