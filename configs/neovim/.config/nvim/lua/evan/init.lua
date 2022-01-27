require('evan.plugins.plug')
-- require('evan.plugins.moonfly-theme')
require('evan.plugins.modus-theme')
require('evan.plugins.lspconfig')
require('evan.plugins.null-ls')
require('evan.plugins.treesitter')
require('evan.plugins.cmp')
require('evan.plugins.neoformat')
require('evan.plugins.pear-tree')
require('evan.plugins.easy-align')

require('evan.options')
require('evan.mappings')

vim.api.nvim_exec([[
  augroup autocmds
    autocmd!
    autocmd FileType rust setlocal colorcolumn=81,101
    autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  augroup END
]], false)
