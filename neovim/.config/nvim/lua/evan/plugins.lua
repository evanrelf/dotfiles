require("paq")({
  "savq/paq-nvim";
  "ishan9299/modus-theme-vim";
  "tpope/vim-sensible";
  "tpope/vim-repeat";
  "wellle/targets.vim";
  "michaeljsmith/vim-indent-object";
  "sheerun/vim-polyglot";
  "dense-analysis/ale";
  "nvim-treesitter/nvim-treesitter";
  "sbdchd/neoformat";
  "tmsvg/pear-tree";
  "tomtom/tcomment_vim";
  "machakann/vim-sandwich";
  "junegunn/vim-easy-align";
  "sickill/vim-pasta";
  "tpope/vim-sleuth";
  "bronson/vim-trailing-whitespace";
  "critiqjo/husk-x.vim";
})

-- ishan9299/modus-theme-vim
vim.o.termguicolors = true
vim.cmd "colorscheme modus-operandi"

-- dense-analysis/ale
vim.g.ale_linters =
  { nix = {"nix"}
  , rust = {"analyzer", "cargo"}
  , sh = {"shellcheck"}
  }
vim.cmd "let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')"
vim.g.ale_lint_on_text_changed = "never"
vim.g.ale_lint_on_insert_leave = false

-- nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed =
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
    },
  highlight =
    { enable = true
    , disable = {"haskell"}
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
