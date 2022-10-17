vim.cmd("packadd packer.nvim")
require("packer").startup(function(use)
  use({
    "ggandor/flit.nvim",
    requires = { "ggandor/leap.nvim" },
    config = function()
      require("flit").setup({
        labeled_modes = "nv",
      })
    end,
  })

  use({
    "ggandor/leap.nvim",
    config = function()
      require("leap")
      vim.keymap.set({ "n", "x", "o" }, "<A-f>", "<Plug>(leap-forward-to)")
      vim.keymap.set({ "n", "x", "o" }, "<A-S-f>", "<Plug>(leap-backward-to)")
      vim.keymap.set({ "n", "x", "o" }, "<A-t>", "<Plug>(leap-forward-till)")
      vim.keymap.set({ "n", "x", "o" }, "<A-S-t>", "<Plug>(leap-backward-till)")
    end,
  })

  use({ "gpanders/editorconfig.nvim" })

  use({
    "hrsh7th/nvim-cmp",
    requires = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
    },
    config = function()
      local has_words_before = function()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0
          and vim.api
              .nvim_buf_get_lines(0, line - 1, line, true)[1]
              :sub(col, col)
              :match("%s")
            == nil
      end
      local cmp = require("cmp")
      cmp.setup({
        sources = {
          { name = "nvim_lsp", max_item_count = 10 },
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
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup({
        sources = {
          ["null-ls"] = { ignore = true },
        },
        text = { spinner = "dots" },
      })
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
          null_ls.builtins.formatting.deno_fmt,
          null_ls.builtins.formatting.fish_indent,
          null_ls.builtins.formatting.fourmolu.with({
            args = function(params)
              return {
                "--stdin-input-file",
                params.bufname,
                "--ghc-opt",
                "-XImportQualifiedPost",
                "--ghc-opt",
                "-XOverloadedRecordDot",
                "--ghc-opt",
                "-XViewPatterns",
              }
            end,
          }),
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
        pattern = { "*.fish", "*.rs" },
        callback = function()
          vim.lsp.buf.format()
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
            vim.lsp.buf.format()
          end
        end,
      })
      vim.api.nvim_create_user_command("Format", function()
        vim.lsp.buf.format()
      end, {})
    end,
  })

  use({
    "L3MON4D3/LuaSnip",
    config = function()
      local luasnip = require("luasnip")
      luasnip.config.set_config({
        enable_autosnippets = true,
        update_events = "TextChanged,TextChangedI",
        region_check_events = "CursorMoved",
      })
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
      require("luasnip.loaders.from_lua").lazy_load({
        paths = "./snippets",
      })
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
    "neovim/nvim-lspconfig",
    after = { "nvim-cmp" },
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      -- stylua: ignore
      local on_attach = function(client, buffernr)
        vim.keymap.set("n", "<Leader>lh", vim.lsp.buf.hover, { buffer = buffer })
        vim.keymap.set("n", "<Leader>le", vim.diagnostic.open_float, {})
        vim.keymap.set("n", "<Leader>ln", vim.diagnostic.goto_next, {})
        vim.keymap.set("n", "<Leader>lp", vim.diagnostic.goto_prev, {})
        vim.keymap.set("n", "<Leader>ld", vim.lsp.buf.definition, { buffer = buffer })
        vim.keymap.set("n", "<Leader>lr", vim.lsp.buf.rename, { buffer = buffer })
        vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = buffer })
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, { buffer = buffer })
      end
      require("lspconfig").hls.setup({
        autostart = false,
        capabilities = capabilities,
        on_attach = on_attach,
      })
    end,
  })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
      -- Use `gco` and `gcO` to explicitly continue comment on new lines
      vim.opt.formatoptions = vim.opt.formatoptions - { "o" }
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    config = function()
      -- stylua: ignore
      local languages = {
        "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fish", "go",
        "haskell", "html", "javascript", "json", "lua", "make", "markdown",
        "markdown_inline", "nix", "perl", "python", "ruby", "rust", "toml",
        "typescript", "vim", "yaml", "zig",
      }
      require("nvim-treesitter.configs").setup({
        ensure_installed = languages,
        highlight = { enable = true },
        indent = {
          enable = true,
          disable = { "haskell", "markdown" },
        },
      })
      -- stylua: ignore
      require("vim.treesitter.query").set_query("haskell", "highlights", [[
        (comment) @comment
        [ (string) (quasiquote_body) ] @string
        (char) @character
        [ (type) (qualified_type) (constructor) (qualified_constructor) ] @type
        [
          "anyclass" "as" "case" "class" (comma) "data" "default" "deriving"
          "do" "else" (export_names) "family" "forall" "foreign" "hiding" "if"
          "import" (import_con_names) "in" "infix" "infixl" "infixr" "instance"
          "let" "mdo" "module" "newtype" "of" (pat_wildcard) "pattern"
          "qualified" "rec" "stock" "then" "type" "via" (where) "->" "=>" "::"
          "<-" "{" "}" "[" "]" "(" ")" "=" "|"  "\\"
        ] @keyword ;; missing "proc" "-<" "-<<"
        [ (operator) (type_operator) ] @operator
      ]])
    end,
  })

  use({ "purescript-contrib/purescript-vim" })

  use({
    "rktjmp/lush.nvim",
    config = function()
      -- vim.opt.termguicolors = true
      -- vim.opt.background = "light"
      -- require("lush")(require("evan.theme"))
    end,
  })

  use({ "tpope/vim-repeat" })

  use({ "vmchale/dhall-vim" })

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
vim.opt.relativenumber = true
vim.opt.colorcolumn = "81"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.gdefault = true
vim.opt.wrap = false
vim.opt.wildmode = "longest:full,full"
vim.opt.swapfile = false

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
