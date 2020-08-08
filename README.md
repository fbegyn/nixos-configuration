# nixos-configuration

## nixops

Create a nixops `deployment` with the following command:

```
nixops create -d <name> <nix file path>
```

Ensure `root` access to the machine you wish to deploy to and run:

```
nixops deploy -d <name>
```
