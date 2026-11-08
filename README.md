# nixos-configuration

Flake for my NixOS machines, my Mac (nix-darwin) and standalone home-manager.

## Layout

| Path        | What                                                                    |
|-------------|-------------------------------------------------------------------------|
| `flake.nix` | Inputs, overlays and the `mkMachine` / `mkMac` / `mkHome` builders       |
| `hosts/`    | One directory per machine (`configuration.nix` + hardware config)       |
| `profiles/` | Shared baselines: `workstation.nix` (bia, ania, geras), `server.nix` (eos, selene, nix-builder-01) |
| `common/`   | Opt-in NixOS modules; `common/default.nix` is applied to every NixOS host |
| `services/` | Self-hosted service modules (prometheus, postgres, blocky, ...)         |
| `users/`    | User accounts (`users/default.nix`) and home-manager config (`users/francis/`) |
| `pkgs/`, `overlays/` | Local packages (exposed as `pkgs.fbegyn.*`) and overlays       |
| `lib/`      | Helpers for proxmox containers and hosting                              |
| `secrets/`  | agenix `.age` files and git-crypt encrypted Nix files                   |
| `attic/`    | Unused modules kept for reference; not evaluated                        |

## Hosts

- NixOS: `bia`, `ania`, `geras` (workstations), `eos`, `selene`, `nix-builder-01`, `zima432`
- nix-darwin: `erebus`
- home-manager: `francis-linux`, `francis-mac`, `vib-mac`

## Usage

Rebuild the local machine:

```
make linux   # nixos-rebuild switch --flake .#
make mac     # darwin-rebuild switch --flake .#
```

Deploy to a remote host:

```
scripts/nix-deploy.sh .#<host> root@<target> [extra nixos-rebuild options]
```

Standalone home-manager:

```
home-manager switch --flake .#francis-linux
```

Check that every NixOS host evaluates and builds:

```
nix flake check
```

`nix develop` gives a shell with `agenix`, `nixfmt`, `statix` and `deadnix`;
`nix fmt` formats with `nixfmt-rfc-style`.

## Secrets

Two mechanisms are in use:

- **git-crypt** encrypts whole files listed in `.gitattributes` (`secrets/**`,
  `vars/**`, some user/work modules). Run `git-crypt unlock` after cloning,
  otherwise evaluation fails on the encrypted Nix files.
- **agenix** handles runtime secrets. Recipients are declared in
  `secrets.nix`; hosts reference them through `age.secrets.<name>.file`.

```
agenix -e secrets/<path>.age   # create or edit a secret
agenix -r                      # re-encrypt after changing recipients
```
