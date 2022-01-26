-- neovim/nvim-lspconfig
vim.o.signcolumn = "yes"
require("lspconfig").hls.setup({
  autostart = false,
})
require("lspconfig").rust_analyzer.setup({
  autostart = false,
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = { command = "clippy" }
    }
  }
})
