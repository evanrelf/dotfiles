-- PLUGINS
local plug = vim.fn["plug#"]
vim.call("plug#begin", vim.call("stdpath", "data") .. "/plugged")
plug("ishan9299/modus-theme-vim")
plug("tpope/vim-sensible")
plug("tpope/vim-repeat")
plug("wellle/targets.vim")
plug("michaeljsmith/vim-indent-object")
plug("sheerun/vim-polyglot")
plug("nvim-treesitter/nvim-treesitter")
plug("sbdchd/neoformat", { ["on"] = {"Neoformat"} })
plug("tomtom/tcomment_vim")
plug("machakann/vim-sandwich")
plug("junegunn/vim-easy-align", { ["on"] = {"EasyAlign", "<Plug>(EasyAlign)"} })
plug("tpope/vim-eunuch")
plug("romainl/vim-cool")
plug("sickill/vim-pasta")
plug("svban/YankAssassin.vim")
plug("tpope/vim-sleuth")
plug("critiqjo/husk-x.vim")
vim.call("plug#end")

-- ishan9299/modus-theme-vim
vim.cmd("silent! colorscheme modus-operandi")

-- nvim-treesitter/nvim-treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fish", "go",
    "haskell", "html", "javascript", "json", "lua", "make", "markdown", "nix",
    "perl", "python", "ruby", "rust", "toml", "typescript", "vim", "yaml",
    "zig",
  },
  highlight = {
    enable = true,
    disable = {"haskell", "markdown"},
  },
  indent = { enable = true },
})

-- sbdchd/neoformat
vim.g.neoformat_only_msg_on_error = true
vim.api.nvim_exec([[
  augroup neoformat
    autocmd!
    autocmd BufWritePre *.dhall,*.fish,*.go,*.janet,*.rs,*.zig undojoin | Neoformat
  augroup END
]], false)

-- junegunn/vim-easy-align
vim.api.nvim_set_keymap("n", "ga", "<Plug>(EasyAlign)", {})
vim.api.nvim_set_keymap("x", "ga", "<Plug>(EasyAlign)", {})


-- OPTIONS
-- Global options
vim.o.termguicolors = true
vim.o.background = "light"
vim.o.colorcolumn = "81"
vim.o.showmode = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.wildmode = "longest:full,list:full"
vim.o.mouse = "a"
vim.o.shiftwidth = 2
vim.o.shiftround = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.cmd("set wildignore+=*/.git/*")
vim.o.wildignorecase = true
vim.o.gdefault = true
vim.o.virtualedit = "block,onemore"
vim.cmd("set shortmess+=I")

-- Window options
vim.wo.number = true
vim.wo.numberwidth = 3
vim.wo.wrap = false

-- Buffer options
vim.bo.expandtab = true


-- MAPPINGS
vim.api.nvim_set_keymap("n", "j", "gj", { noremap = true })
vim.api.nvim_set_keymap("n", "k", "gk", { noremap = true })
vim.api.nvim_set_keymap("x", "<", "<gv", { noremap = true })
vim.api.nvim_set_keymap("x", ">", ">gv", { noremap = true })


-- AUTOCMDS
vim.api.nvim_exec([[
  augroup autocmds
    autocmd!
    autocmd FileType rust setlocal colorcolumn=81,101
    autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  augroup END
]], false)
