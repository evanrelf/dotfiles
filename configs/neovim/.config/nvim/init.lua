vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "hrsh7th/nvim-cmp",
    requires = {"hrsh7th/cmp-buffer", "hrsh7th/cmp-path"},
    config = function()
      local has_words_before = function()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end
      local cmp = require("cmp")
      cmp.setup({
        sources = {
          { name = "buffer" },
          { name = "path" },
        },
        mapping = {
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end, {"i", "s"}),
          ["<S-Tab>"] = cmp.mapping(function()
            if cmp.visible() then
              cmp.select_prev_item()
            end
          end, {"i", "s"}),
        },
        window = {
          -- Hides file preview from `cmp-path`
          documentation = {
            max_width = 0,
            max_height = 0,
          },
        },
      })
      vim.opt.completeopt = {"menu", "menuone", "noselect"}
    end,
  })

  use({
    "ishan9299/modus-theme-vim",
    config = function()
      vim.opt.termguicolors = true
      vim.opt.background = "light"
      vim.cmd("colorscheme modus-operandi")
    end,
  })

  use({
    "jose-elias-alvarez/null-ls.nvim",
    requires = {"nvim-lua/plenary.nvim"},
    config = function()
      local null_ls = require("null-ls")
      null_ls.setup({
        sources = {
          null_ls.builtins.diagnostics.shellcheck,
          null_ls.builtins.diagnostics.trail_space,
          null_ls.builtins.formatting.fish_indent,
          null_ls.builtins.formatting.rustfmt.with({
            extra_args = { "--edition=2021" },
          }),
          null_ls.builtins.formatting.zigfmt,
        },
      })
      vim.opt.signcolumn = "number"
      vim.api.nvim_create_augroup("evan_null_ls", { clear = true })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = "evan_null_ls",
        pattern = {"*.fish", "*.rs"},
        callback = function()
          vim.lsp.buf.formatting_sync()
        end,
      })
    end,
  })

  use({
    "Julian/vim-textobj-variable-segment",
    requires = {"kana/vim-textobj-user"},
  })

  use({
    "machakann/vim-sandwich",
  })

  use({
    "michaeljsmith/vim-indent-object",
  })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
      -- Use `gco` and `gcO` to explicitly continue comment on new lines
      vim.opt.formatoptions = vim.opt.formatoptions - {"o"}
    end,
  })

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
        highlight = { enable = true },
        indent = {
          enable = true,
          disable = {"haskell", "markdown"},
        },
      })
    end,
  })

  use({
    "rlane/pounce.nvim",
    config = function()
      require("pounce")
      vim.cmd([[
        highlight! link PounceMatch Visual
        highlight! link PounceGap PounceMatch
        highlight! link PounceAccept Search
        highlight! link PounceAcceptBest PounceAccept
      ]])
      vim.keymap.set("n", "S", "<Cmd>Pounce<CR>")
      vim.keymap.set("x", "S", "<Cmd>Pounce<CR>")
      vim.keymap.set("o", "S", "<Cmd>Pounce<CR>")
    end,
  })

  use({
    "romainl/vim-cool",
  })

  use({
    "sickill/vim-pasta",
  })

  use({
    "tpope/vim-repeat",
  })

  use({
    "wbthomason/packer.nvim",
    config = function()
      vim.api.nvim_create_augroup("evan_packer", { clear = true })
      vim.api.nvim_create_autocmd("BufWritePost", {
        group = "evan_packer",
        pattern = "init.lua",
        command = "source <afile> | PackerCompile",
      })
    end,
  })

  use({
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({
        check_ts = true,
      })
    end,
  })
end)

vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.number = true
vim.opt.colorcolumn = "81"
vim.opt.mouse = "a"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wrap = false

vim.keymap.set("x", "<", "<gv")
vim.keymap.set("x", ">", ">gv")

vim.api.nvim_create_augroup("evan_filetype", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = "evan_filetype",
  pattern = "fish",
  callback = function()
    vim.opt_local.shiftwidth = 4
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  group = "evan_filetype",
  pattern = "rust",
  callback = function()
    vim.opt_local.colorcolumn = "81,101"
  end,
})
vim.api.nvim_create_autocmd("FileType", {
  group = "evan_filetype",
  pattern = "gitcommit",
  callback = function()
    vim.opt_local.colorcolumn = "51,73"
  end,
})
