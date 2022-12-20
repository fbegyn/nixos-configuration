self: super: {
  slack = super.slack.overrideAttrs (old: {
    runtimeDependencies = old.runtimeDependencies ++ [ super.wayland ];
  });
}
