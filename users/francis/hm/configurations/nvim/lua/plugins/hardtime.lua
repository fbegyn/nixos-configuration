-- Telescope: fuzzy finder
local hardtime = require("hardtime")

hardtime.setup({
  disabled_keys = {
    ["<Up>"] = false,
    ["<Down>"] = false,
  },
})
