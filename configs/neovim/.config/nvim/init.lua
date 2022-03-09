-- PLUGINS
vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "wbthomason/packer.nvim",
    config = function()
      vim.api.nvim_exec([[
        augroup packer
          autocmd!
          autocmd BufWritePost init.lua source <afile> | PackerCompile
        augroup end
      ]], false)
    end,
  })

  use({ "bakpakin/janet.vim" })

  use({ "critiqjo/husk-x.vim" })

  use({
    "ishan9299/modus-theme-vim",
    config = function()
      vim.cmd("silent! colorscheme modus-operandi")
    end,
  })

  use({
    "itmecho/formatter.nvim",
    branch = "synchronous-format",
    config = function()
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
        augroup end
      ]], false)
    end
  })

  use({
    "junegunn/vim-easy-align",
    config = function()
      vim.api.nvim_set_keymap("n", "ga", "<Plug>(EasyAlign)", {})
      vim.api.nvim_set_keymap("x", "ga", "<Plug>(EasyAlign)", {})
    end,
  })

  use({
    "lewis6991/gitsigns.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup({
        signcolumn = false,
        numhl = true,
      })
    end,
  })

  use({ "machakann/vim-sandwich" })

  use({ "michaeljsmith/vim-indent-object" })

  use({ "nathom/filetype.nvim" })

  use({
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fish",
          "go", "haskell", "html", "javascript", "json", "lua", "make",
          "markdown", "nix", "perl", "python", "ruby", "rust", "toml",
          "typescript", "vim", "yaml", "zig",
        },
        highlight = {
          enable = true,
          disable = {"haskell", "markdown"},
        },
        indent = { enable = true },
      })
    end,
  })

  use({ "rhysd/clever-f.vim" })

  use({
    "rlane/pounce.nvim",
    config = function()
      vim.api.nvim_set_keymap("n", "gs", "<Cmd>Pounce<CR>", {})
      vim.api.nvim_set_keymap("n", "gS", "<Cmd>PounceRepeat<CR>", {})
      vim.api.nvim_set_keymap("v", "gs", "<Cmd>Pounce<CR>", {})
      vim.api.nvim_set_keymap("o", "gs", "<Cmd>Pounce<CR>", {})
    end,
  })

  use({ "romainl/vim-cool" })

  use({ "sheerun/vim-polyglot" })

  use({ "sickill/vim-pasta" })

  use({ "svban/YankAssassin.vim" })

  use({ "tomtom/tcomment_vim" })

  use({ "tpope/vim-eunuch" })

  use({ "tpope/vim-repeat" })

  use({ "tpope/vim-sensible" })

  use({ "tpope/vim-sleuth" })

  use({ "wellle/targets.vim" })
end)


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

vim.api.nvim_set_keymap("n", "<C-u>", [[<Cmd>execute "normal! " .. (float2nr(winheight(0) / 2) * v:count1) .. "\<C-y>"<CR>]], { noremap = true })
vim.api.nvim_set_keymap("n", "<C-d>", [[<Cmd>execute "normal! " .. (float2nr(winheight(0) / 2) * v:count1) .. "\<C-e>"<CR>]], { noremap = true })
vim.api.nvim_set_keymap("n", "<C-b>", [[<Cmd>execute "normal! " .. (float2nr(winheight(0)) * v:count1) .. "\<C-y>"<CR>]], { noremap = true })
vim.api.nvim_set_keymap("n", "<C-f>", [[<Cmd>execute "normal! " .. (float2nr(winheight(0)) * v:count1) .. "\<C-e>"<CR>]], { noremap = true })


-- AUTOCMDS
vim.api.nvim_exec([[
  augroup autocmds
    autocmd!
    autocmd FileType rust setlocal colorcolumn=81,101
    autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  augroup end
]], false)
