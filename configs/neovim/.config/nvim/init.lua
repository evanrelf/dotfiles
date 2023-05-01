vim.api.nvim_create_augroup("Evan", { clear = true })

require("hotpot")
require("evan.next")

local packer = require("packer")
packer.startup(function(use)
  use({ "chaoren/vim-wordmotion" })

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
    after = "vim-sandwich",
    config = function()
      require("leap").add_default_mappings()
    end,
  })

  use({ "gpanders/editorconfig.nvim" })

  use({ "gpanders/nvim-parinfer" })

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

  -- use({
  --   "ishan9299/modus-theme-vim",
  --   config = function()
  --     vim.opt.termguicolors = true
  --     vim.opt.background = "light"
  --     vim.g.modus_dim_inactive_window = false
  --     vim.cmd.colorscheme("modus-operandi")
  --   end,
  -- })

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
      vim.api.nvim_create_autocmd({ "BufWritePre" }, {
        group = "Evan",
        pattern = { "*.fish", "*.fnl", "*.rs" },
        callback = function()
          vim.lsp.buf.format()
        end,
      })
      vim.api.nvim_create_autocmd({ "BufWritePre" }, {
        group = "Evan",
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

  use({
    "machakann/vim-sandwich",
    before = "leap.nvim",
    config = function()
      vim.g.sandwich_no_default_key_mappings = 1
      vim.keymap.set("n", "gsa", "<Plug>(sandwich-add)")
      vim.keymap.set("x", "gsa", "<Plug>(sandwich-add)")
      vim.keymap.set("o", "gsa", "<Plug>(sandwich-add)")
      vim.keymap.set("n", "gsd", "<Plug>(sandwich-delete)")
      vim.keymap.set("x", "gsd", "<Plug>(sandwich-delete)")
      vim.keymap.set("n", "gsdb", "<Plug>(sandwich-delete-auto)")
      vim.keymap.set("n", "gsr", "<Plug>(sandwich-replace)")
      vim.keymap.set("x", "gsr", "<Plug>(sandwich-replace)")
      vim.keymap.set("n", "gsrb", "<Plug>(sandwich-replace-auto)")
    end,
  })

  use({ "michaeljsmith/vim-indent-object" })

  use({
    "neovim/nvim-lspconfig",
    after = { "nvim-cmp" },
    config = function()
      local lspconfig = require("lspconfig")
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
      lspconfig.hls.setup({
        autostart = false,
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.rust_analyzer.setup({
        autostart = false,
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.nil_ls.setup({
        autostart = true,
        capabilities = capabilities,
        on_attach = on_attach,
      })
      local configs = require("lspconfig.configs")
      local util = require("lspconfig.util")
      if not configs.static_ls then
        configs.static_ls = {
          default_config = {
            cmd = { "static-ls" },
            filetypes = { "haskell" },
            root_dir = util.root_pattern("*.cabal", "package.yaml"),
            single_file_support = false,
            settings = {},
          },
        }
      end
      lspconfig.static_ls.setup({
        autostart = false,
        capabilities = capabilities,
        on_attach = on_attach,
      })
      if not configs.sidekick then
        configs.sidekick = {
          default_config = {
            cmd = { "sidekick" },
            filetypes = { "haskell" },
            root_dir = util.root_pattern("*.cabal", "package.yaml"),
            single_file_support = true,
            settings = {},
          },
        }
      end
      lspconfig.sidekick.setup({
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
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
    },
    config = function()
      local telescope = require("telescope")
      local telescope_builtin = require("telescope.builtin")
      local telescope_actions = require("telescope.actions")
      telescope.setup({
        defaults = {
          preview = false,
          mappings = {
            i = {
              ["<Esc>"] = telescope_actions.close,
              ["<C-u>"] = false,
            },
          },
        },
        extensions = {
          fzf = {
            fuzzy = false,
          },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ghc")
      vim.keymap.set("n", "<A-o>", "<Cmd>Telescope git_files theme=ivy<CR>")
      vim.keymap.set("n", "<Leader>f", "<Cmd>Telescope git_files theme=ivy<CR>")
      vim.keymap.set("n", "<Leader>b", "<Cmd>Telescope buffers theme=ivy<CR>")
    end,
  })

  use({
    "nvim-treesitter/nvim-treesitter",
    config = function()
      -- stylua: ignore
      local languages = {
        "bash", "c", "cpp", "css", "dockerfile", "dot", "elixir", "fennel",
        "fish", "go", "haskell", "html", "javascript", "json", "lua", "make",
        "markdown", "markdown_inline", "nix", "perl", "python", "ruby", "rust",
        "toml", "typescript", "vim", "yaml", "zig",
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
      -- require("lush")(require("primer"))
    end,
  })

  use({ "sitiom/nvim-numbertoggle" })

  use({ "tpope/vim-repeat" })

  use({ "vmchale/dhall-vim" })

  use({
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup()
    end,
  })

  use({
    "windwp/nvim-ts-autotag",
    requires = { "nvim-treesitter/nvim-treesitter" },
    after = { "nvim-treesitter" },
    config = function()
      require("nvim-ts-autotag").setup()
    end,
  })

  local compile_path = vim.fn.stdpath("config") .. "/plugin/packer_compiled.lua"
  if vim.fn.empty(vim.fn.glob(compile_path)) > 0 then
    packer.compile()
  end
end)
