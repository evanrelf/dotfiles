-- local hotpot_path = vim.fn.stdpath("data") .. "/plugged/hotpot.nvim"
-- if vim.fn.empty(vim.fn.glob(hotpot_path)) > 0 then
--   vim.fn.system({"git", "clone", "https://github.com/rktjmp/hotpot.nvim", hotpot_path})
-- end
-- vim.o.runtimepath = vim.o.runtimepath .. "," .. hotpot_path
-- require("hotpot")

require('evan.plugins')
require('evan.options')
require('evan.mappings')

vim.api.nvim_exec([[
  augroup autocmds
    autocmd!
    autocmd FileType rust setlocal colorcolumn=81,101
    autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  augroup END
]], false)
