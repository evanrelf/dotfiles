require("paq")({
  "savq/paq-nvim";
  "junegunn/seoul256.vim";
  "pacha/vem-tabline";
  "tpope/vim-sensible";
  "tpope/vim-repeat";
  "wellle/targets.vim";
  "michaeljsmith/vim-indent-object";
  "sheerun/vim-polyglot";
  "dense-analysis/ale";
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

-- junegunn/seoul256.vim
vim.g.seoul256_background = 256
vim.o.termguicolors = true
vim.o.background = "light"
vim.api.nvim_exec([[
  colorscheme seoul256-light
  highlight! link StatusLine CursorLineNr
  highlight! link StatusLineNC LineNr
]], false)

-- pacha/vem-tabline
vim.g.vem_tabline_show_icon = false

-- dense-analysis/ale
vim.g.ale_linters =
  { nix = {"nix"}
  , rust = {"analyzer", "cargo"}
  , sh = {"shellcheck"}
  }
vim.cmd "let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')"
vim.g.ale_lint_on_text_changed = "never"
vim.g.ale_lint_on_insert_leave = false

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
