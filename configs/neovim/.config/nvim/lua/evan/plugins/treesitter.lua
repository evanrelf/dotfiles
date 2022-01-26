-- nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "bash",
    "fish",
    -- "haskell",
    "html",
    "javascript",
    "json",
    "lua",
    "nix",
    "python",
    "rust",
    "toml",
    "yaml",
  },
  highlight = {
    enable = true,
    disable = {"haskell"},
  },
  indent = { enable = true },
})

