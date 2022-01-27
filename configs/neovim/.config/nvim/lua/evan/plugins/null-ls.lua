-- jose-elias-alvarez/null-ls.nvim
null_ls = require('null-ls')
null_ls.setup({
  sources = {
    null_ls.builtins.formatting.fourmolu,
    null_ls.builtins.formatting.nixfmt.with({
      command = "nixpkgs-fmt",
    }),
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.code_actions.shellcheck,
    null_ls.builtins.formatting.rustfmt,
    null_ls.builtins.formatting.zigfmt,
    null_ls.builtins.formatting.deno_fmt.with({
      filetypes = { "typescript" },
    }),
    null_ls.builtins.formatting.fish_indent,
  },
})
