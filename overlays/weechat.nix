self: super: {
  weechat = super.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = with super.weechatScripts; [
        weechat-matrix
        weechat-otr
      ];
    };
  };
}
