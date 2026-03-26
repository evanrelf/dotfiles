vim.cmd("colorscheme flexoki")

vim.pack.add({
  "https://github.com/nvim-treesitter/nvim-treesitter",
})
require("nvim-treesitter").install({
  "bash", "fish", "go", "haskell", "haskell_persistent", "json", "lua",
  "markdown", "nix", "rust", "sql", "zig",
})
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "sh", "fish", "go", "haskell", "json", "lua", "markdown", "nix", "rust",
    "sql", "zig",
  },
  callback = function() vim.treesitter.start() end,
})
