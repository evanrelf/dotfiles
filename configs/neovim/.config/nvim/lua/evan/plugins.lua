-- bluz71/vim-moonfly-colors
vim.o.termguicolors = true
vim.cmd "silent! colorscheme moonfly"

-- ishan9299/modus-theme-vim
-- vim.o.termguicolors = true
-- vim.cmd "silent! colorscheme modus-vivendi"

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

-- nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "bash",
    "fish",
    "haskell",
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

-- sbdchd/neoformat
vim.g.neoformat_only_msg_on_error = true
vim.api.nvim_exec([[
  augroup neoformat
    autocmd!
    autocmd BufWritePre *.dhall,*.fish,*.rs undojoin | Neoformat
  augroup END
]], false)

-- tmsvg/pear-tree
vim.g.pear_tree_repeatable_expand = false
vim.g.pear_tree_smart_openers = true
vim.g.pear_tree_smart_closers = true
vim.g.pear_tree_smart_backspace = true
vim.api.nvim_set_keymap("i", "<Space>", "<Plug>(PearTreeSpace)", {})

-- junegunn/vim-easy-align
vim.api.nvim_set_keymap("n", "ga", "<Plug>(EasyAlign)", {})
vim.api.nvim_set_keymap("x", "ga", "<Plug>(EasyAlign)", {})
