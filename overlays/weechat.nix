self: surer: {
  weechat =  with super.weechatScripts; {
    configure = { availablePlugins, ... }: {
      scripts = [
        multiline
        weechat-matrix
      ];
      extraBuildInputs = [];
    };
  };
}
