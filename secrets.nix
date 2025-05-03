let
  francis = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOa6QQLv+FCgoMej4BUuQRzpGsmTiwIJnspamE0wrsM2";
  users = [ francis ];

  erebus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPJDyIr/FSz1cJdcoW69R+NrWzwGK/+3gJpqD1t8L2zE";
  bia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKzxQgondgEYcLpcPdJLrTdNgZ2gznOHCAxMdaceTUT1";
  selene = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKzxQgondgEYcLpcPdJLrTdNgZ2gznOHCAxMdaceTUT1";
  systems = [ erebus bia selene ];
in
{
  "secrets/wireless.age".publicKeys = users ++ systems;
  "secrets/api/cf.age".publicKeys = users ++ systems;
}
