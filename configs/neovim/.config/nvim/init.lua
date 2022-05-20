vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "hrsh7th/nvim-cmp",
    requires = { "hrsh7th/cmp-buffer", "hrsh7th/cmp-path" },
    config = function()
      local has_words_before = function()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0
          and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]
              :sub(col, col)
              :match("%s")
            == nil
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
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function()
            if cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end, { "i", "s" }),
        },
        window = {
          -- Hides file preview from `cmp-path`
          documentation = {
            max_width = 0,
            max_height = 0,
          },
        },
      })
      vim.opt.completeopt = { "menu", "menuone", "noselect" }
    end,
  })

  use({
    "ishan9299/modus-theme-vim",
    config = function()
      vim.opt.termguicolors = true
      vim.opt.background = "light"
      vim.g.modus_dim_inactive_window = false
      vim.cmd("colorscheme modus-operandi")
    end,
  })

  use({
    "jose-elias-alvarez/null-ls.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      local null_ls = require("null-ls")
      null_ls.setup({
        sources = {
          null_ls.builtins.diagnostics.shellcheck,
          null_ls.builtins.diagnostics.trail_space,
          null_ls.builtins.formatting.fish_indent,
          null_ls.builtins.formatting.fourmolu,
          null_ls.builtins.formatting.nixpkgs_fmt,
          null_ls.builtins.formatting.rustfmt.with({
            extra_args = { "--edition=2021" },
          }),
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.formatting.zigfmt,
        },
      })
      vim.opt.signcolumn = "number"
      vim.api.nvim_create_augroup("evan_null_ls", { clear = true })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = "evan_null_ls",
        pattern = { "*.fish", "*.lua", "*.rs" },
        callback = function()
          vim.lsp.buf.formatting_sync()
        end,
      })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = "evan_null_ls",
        pattern = { "*.hs", ".nix" },
        callback = function(args)
          local extension = string.sub(args.match, -3, -1)
          local is_haskell = extension == ".hs"
            and vim.env.EVAN_FORMAT_HASKELL == "true"
          local is_nix = extension == ".nix"
            and vim.env.EVAN_FORMAT_NIX == "true"
          if is_haskell or is_nix then
            vim.lsp.buf.formatting_sync()
          end
        end,
      })
      vim.cmd("command! Format lua vim.lsp.buf.formatting_sync()")
    end,
  })

  use({
    "Julian/vim-textobj-variable-segment",
    requires = { "kana/vim-textobj-user" },
  })

  use({
    "L3MON4D3/LuaSnip",
    config = function()
      local luasnip = require("luasnip")
      vim.keymap.set("i", "<A-Tab>", function()
        if luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        end
      end)
      vim.keymap.set("i", "<A-S-Tab>", function()
        if luasnip.jumpable(-1) then
          luasnip.jump(-1)
        end
      end)
      local s = luasnip.s
      local i = luasnip.insert_node
      local f = luasnip.function_node
      local fmt = require("luasnip.extras.fmt").fmt
      local same = function(index)
        return f(function(args)
          return args[1]
        end, { index })
      end
      luasnip.config.set_config({
        enable_autosnippets = true,
        update_events = "TextChanged,TextChangedI",
        region_check_events = "CursorMoved",
      })
      -- stylua: ignore
      luasnip.add_snippets("haskell", {
        s(
          { trig = "^lang ", regTrig = true },
          fmt("{{-# LANGUAGE {} #-}}", { i(0) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^opt ", regTrig = true },
          fmt("{{-# OPTIONS_GHC {} #-}}", { i(0) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^module ", regTrig = true },
          fmt("module {} where", { i(0) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^i ", regTrig = true },
          fmt("import {}", { i(0) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^ii ", regTrig = true },
          fmt("import qualified {}", { i(0) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^uio ", regTrig = true },
          fmt("import qualified UnliftIO.{} as {}", { same(1), i(1) }),
          { show_condition = function() return false end }
        ),
        s(
          { trig = "^instance ", regTrig = true },
          fmt("instance {} where", { i(0) }),
          { show_condition = function() return false end }
        ),
      }, { key = "haskell", type = "autosnippets" })
    end,
  })

  use({
    "linty-org/readline.nvim",
    config = function()
      local readline = require("readline")
      vim.keymap.set("!", "<C-n>", "<Down>")
      vim.keymap.set("!", "<C-p>", "<Up>")
      vim.keymap.set("!", "<C-f>", "<Right>")
      vim.keymap.set("!", "<C-b>", "<Left>")
      vim.keymap.set("!", "<M-f>", readline.forward_word)
      vim.keymap.set("!", "<M-b>", readline.backward_word)
      vim.keymap.set("!", "<C-a>", readline.beginning_of_line)
      vim.keymap.set("!", "<C-e>", readline.end_of_line)
      vim.keymap.set("!", "<C-k>", readline.kill_line)
      vim.keymap.set("!", "<C-u>", readline.backward_kill_line)
    end,
  })

  use({ "machakann/vim-sandwich" })

  use({ "michaeljsmith/vim-indent-object" })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
      -- Use `gco` and `gcO` to explicitly continue comment on new lines
      vim.opt.formatoptions = vim.opt.formatoptions - { "o" }
    end,
  })

  use({
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
    },
    config = function()
      local telescope = require("telescope")
      local telescope_builtin = require("telescope.builtin")
      telescope.setup({
        defaults = {
          preview = false,
        },
        pickers = {
          buffers = { theme = "ivy" },
          find_files = { theme = "ivy" },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ghc")
      vim.keymap.set("n", "<Leader>f", telescope_builtin.find_files)
      vim.keymap.set("n", "<Leader>b", telescope_builtin.buffers)
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    config = function()
        -- stylua: ignore
      local languages = {
        "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fish", "go",
        "haskell", "html", "javascript", "json", "lua", "make", "markdown",
        "nix", "perl", "python", "ruby", "rust", "toml", "typescript", "vim",
        "yaml", "zig",
      }
      require("nvim-treesitter.configs").setup({
        ensure_installed = languages,
        highlight = { enable = true },
        indent = {
          enable = true,
          disable = { "haskell", "markdown" },
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

  use({ "tpope/vim-repeat" })

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
vim.opt.gdefault = true
vim.opt.wrap = false
vim.opt.wildmode = "longest:full"

vim.keymap.set("n", "<Space>", "<Leader>", { remap = true })
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
