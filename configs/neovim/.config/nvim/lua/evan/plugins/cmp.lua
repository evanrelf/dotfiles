-- hrsh7th/nvim-cmp
local cmp = require("cmp")
cmp.setup({
  mapping = {
    ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), {"i", "s"}),
    ["<S-Tab>"] = cmp.mapping(cmp.mapping.select_prev_item(), {"i", "s"}),
    ["<CR>"] = cmp.mapping.confirm(),
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "buffer" },
  }
})
