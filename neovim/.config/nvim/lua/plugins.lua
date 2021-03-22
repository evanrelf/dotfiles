vim.cmd "packadd packer.nvim"

vim.api.nvim_exec([[
  augroup packer
    autocmd!
    autocmd BufWritePost plugins.lua PackerCompile
  augroup END
]], false)

return require("packer").startup(function()
  use {"wbthomason/packer.nvim", opt = true}

  use {
    "junegunn/seoul256.vim",
    config = function()
      vim.g.seoul256_background = 256
      vim.o.termguicolors = true
      vim.o.background = "light"
      vim.api.nvim_exec([[
        colorscheme seoul256-light
        highlight! link StatusLine CursorLineNr
        highlight! link StatusLineNC LineNr
      ]], false)
    end
  }

  use {
    "romgrk/barbar.nvim",
    config = function()
      vim.api.nvim_exec([[
        let bufferline = get(g:, 'bufferline', {})
        let bufferline.animation = v:false
        let bufferline.icons = v:false
        let bufferline.closable = v:false
      ]], false)
    end
  }

  use {"tpope/vim-sensible"}

  use {"tpope/vim-repeat"}

  use {"wellle/targets.vim"}

  use {"michaeljsmith/vim-indent-object"}

  use {"sheerun/vim-polyglot"}

  use {
    "dense-analysis/ale",
    ft = {"fish", "nix", "rust", "sh"},
    config = function()
      vim.g.ale_linters =
        { nix = {"nix"}
        , rust = {"analyzer", "cargo"}
        , sh = {"shellcheck"}
        }
      vim.cmd "let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')"
      vim.g.ale_lint_on_text_changed = "never"
      vim.g.ale_lint_on_insert_leave = false
    end
  }

  use {
    "sbdchd/neoformat",
    cmd = {"Neoformat"},
    config = function()
      vim.g.neoformat_only_msg_on_error = true
      vim.api.nvim_exec([[
        augroup neoformat
          autocmd!
          autocmd BufWritePre *.dhall,*.fish,*.rs undojoin | Neoformat
        augroup END
      ]], false)
    end
  }

  use {
    "tmsvg/pear-tree",
    config = function()
      vim.g.pear_tree_repeatable_expand = false
      vim.g.pear_tree_smart_openers = true
      vim.g.pear_tree_smart_closers = true
      vim.g.pear_tree_smart_backspace = true
      vim.api.nvim_set_keymap("i", "<Space>", "<Plug>(PearTreeSpace)", {})
    end
  }

  use {"tomtom/tcomment_vim"}

  use {"machakann/vim-sandwich"}

  use {
    "junegunn/vim-easy-align",
    config = function()
      vim.api.nvim_set_keymap("n", "ga", "<Plug>(EasyAlign)", {})
      vim.api.nvim_set_keymap("x", "ga", "<Plug>(EasyAlign)", {})
    end
  }

  use {"sickill/vim-pasta"}

  use {"tpope/vim-sleuth"}

  use {"bronson/vim-trailing-whitespace"}

  use {"critiqjo/husk-x.vim"}
end)
