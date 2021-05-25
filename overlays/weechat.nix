self: super: {
  weechatScripts.weechat-matrix-fixed = super.weechatScripts.weechat-matrix.overrideAttrs (oldAttrs: rec {
    postFixup = oldAttrs.postFixup + ''
      substituteInPlace $out/lib/*/site-packages/matrix/server.py --replace "\"matrix_sso_helper\"" "\"$out/bin/matrix_sso_helper\""
      substituteInPlace $out/lib/*/site-packages/matrix/server.py --replace "and (not self.config.username or self.config.password)):" "and (not self.config.username or not self.config.password)):"
    '';  
  });

  weechat =  with super.weechatScripts; super.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts =
        [ self.weechatScripts.weechat-matrix-fixed weechat-otr multiline ];
      extraBuildInputs =
        [ availablePlugins.python.withPackages (_: [ weechat-matrix ])];
    };
  };
}
