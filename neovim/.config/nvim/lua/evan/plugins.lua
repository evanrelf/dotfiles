vim.api.nvim_exec([[
  call plug#begin(stdpath('data') . '/plugged')

  Plug 'ishan9299/modus-theme-vim'
  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-repeat'
  Plug 'wellle/targets.vim'
  Plug 'michaeljsmith/vim-indent-object'
  Plug 'sheerun/vim-polyglot'
  Plug 'neovim/nvim-lspconfig'
  Plug 'dense-analysis/ale'
  Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'sbdchd/neoformat'
  Plug 'tmsvg/pear-tree'
  Plug 'tomtom/tcomment_vim'
  Plug 'machakann/vim-sandwich'
  Plug 'junegunn/vim-easy-align'
  Plug 'sickill/vim-pasta'
  Plug 'tpope/vim-sleuth'
  Plug 'bronson/vim-trailing-whitespace'
  Plug 'critiqjo/husk-x.vim'

  call plug#end()
]], false)

-- ishan9299/modus-theme-vim
vim.o.termguicolors = true
vim.cmd "colorscheme modus-operandi"

-- neovim/nvim-lspconfig
require("lspconfig").rust_analyzer.setup({
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = { command = "clippy" }
    }
  }
})

-- dense-analysis/ale
vim.g.ale_linters =
  { nix = {"nix"}
  , rust = {}
  , sh = {"shellcheck"}
  }
vim.cmd "let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')"
vim.g.ale_lint_on_text_changed = "never"
vim.g.ale_lint_on_insert_leave = false

-- nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup(
  { ensure_installed =
      { "bash"
      , "fish"
      , "haskell"
      , "html"
      , "javascript"
      , "json"
      , "lua"
      , "nix"
      , "python"
      , "rust"
      , "toml"
      , "yaml"
      }
  , highlight =
      { enable = true
      , disable = {"haskell"}
      }
  , indent = { enable = true }
  }
)

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
