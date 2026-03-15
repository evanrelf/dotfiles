local M = {}

-- Use this as a reference:
-- https://github.com/neovim/neovim/blob/master/runtime/colors/vim.lua

vim.cmd('highlight clear')
vim.g.colors_name = 'flexoki'

local hi = function(name, value)
  value.force = true
  value.cterm = value.cterm or {}
  vim.api.nvim_set_hl(0, name, value)
end

hi('Normal', {})

if vim.o.background == 'light' then
else
end

return M
