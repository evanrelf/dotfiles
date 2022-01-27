vim.api.nvim_exec([[
  call plug#begin(stdpath('data') . '/plugged')

  Plug 'bluz71/vim-moonfly-colors'
  Plug 'ishan9299/modus-theme-vim'
  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-repeat'
  Plug 'wellle/targets.vim'
  Plug 'michaeljsmith/vim-indent-object'
  Plug 'sheerun/vim-polyglot'
  Plug 'neovim/nvim-lspconfig'
  Plug 'jose-elias-alvarez/null-ls.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'sbdchd/neoformat'
  Plug 'tmsvg/pear-tree'
  Plug 'tomtom/tcomment_vim'
  Plug 'machakann/vim-sandwich'
  Plug 'junegunn/vim-easy-align'
  Plug 'sickill/vim-pasta'
  Plug 'tpope/vim-sleuth'
  Plug 'bronson/vim-trailing-whitespace'
  Plug 'critiqjo/husk-x.vim'

  call plug#end()
]], false)
