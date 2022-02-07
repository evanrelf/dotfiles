-- PLUGINS
local plug = vim.fn["plug#"]
vim.call("plug#begin", vim.call("stdpath", "data") .. "/plugged")
plug("bakpakin/janet.vim")
plug("critiqjo/husk-x.vim")
plug("ishan9299/modus-theme-vim")
plug("itmecho/formatter.nvim", { ["branch"] = "synchronous-format" })
plug("junegunn/vim-easy-align", { ["on"] = {"EasyAlign", "<Plug>(EasyAlign)"} })
plug("lewis6991/gitsigns.nvim")
plug("machakann/vim-sandwich")
plug("michaeljsmith/vim-indent-object")
plug("nvim-lua/plenary.nvim")
plug("nvim-treesitter/nvim-treesitter")
plug("rhysd/clever-f.vim")
plug("rlane/pounce.nvim")
plug("romainl/vim-cool")
plug("sheerun/vim-polyglot")
plug("sickill/vim-pasta")
plug("svban/YankAssassin.vim")
plug("tomtom/tcomment_vim")
plug("tpope/vim-eunuch")
plug("tpope/vim-repeat")
plug("tpope/vim-sensible")
plug("tpope/vim-sleuth")
plug("wellle/targets.vim")
vim.call("plug#end")

-- ishan9299/modus-theme-vim
vim.cmd("silent! colorscheme modus-operandi")

-- junegunn/vim-easy-align
vim.api.nvim_set_keymap("n", "ga", "<Plug>(EasyAlign)", {})
vim.api.nvim_set_keymap("x", "ga", "<Plug>(EasyAlign)", {})

-- lewis6991/gitsigns.nvim
require("gitsigns").setup({
  signcolumn = false,
  numhl = true,
})

-- mhartington/formatter.nvim
function formatter_exe(exe, args)
  return {
    function()
      return { exe = exe, args = args or {}, stdin = true }
    end
  }
end
require("formatter").setup({
  filetype = {
    dhall = formatter_exe("dhall", {"format"}),
    elixir = formatter_exe("mix", {"format", "-"}),
    go = formatter_exe("go", {"fmt"}),
    haskell = formatter_exe("fourmolu"),
    janet = formatter_exe("janetfmt"),
    json = formatter_exe("jq", {"."}),
    nix = formatter_exe("nixpkgs-fmt"),
    rust = formatter_exe("rustfmt"),
    zig = formatter_exe("zig", {"fmt", "--stdin"}),
  }
})
vim.api.nvim_exec([[
  augroup formatter
    autocmd!
    autocmd BufWritePre *.dhall,*.go,*.janet,*.rs,*.zig FormatSync
  augroup END
]], false)

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

-- rlane/pounce.nvim
vim.api.nvim_set_keymap("n", "gs", "<Cmd>Pounce<CR>", {})
vim.api.nvim_set_keymap("n", "gS", "<Cmd>PounceRepeat<CR>", {})
vim.api.nvim_set_keymap("v", "gs", "<Cmd>Pounce<CR>", {})
vim.api.nvim_set_keymap("o", "gs", "<Cmd>Pounce<CR>", {})


-- OPTIONS
-- Global options
vim.o.termguicolors = true
vim.o.background = "light"
vim.o.colorcolumn = "81"
vim.o.cursorline = true
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
