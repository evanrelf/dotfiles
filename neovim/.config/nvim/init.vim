call plug#begin()
Plug 'junegunn/seoul256.vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-repeat'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'sheerun/vim-polyglot'
Plug 'dense-analysis/ale', {'for': ['nix', 'rust', 'sh']}
Plug 'sbdchd/neoformat', {'on': 'Neoformat'}
Plug 'junegunn/fzf.vim', {'on': 'Files'} | Plug 'junegunn/fzf'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-eunuch'
Plug 'machakann/vim-sandwich'
Plug 'tmsvg/pear-tree'
Plug 'tomtom/tcomment_vim'
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'tpope/vim-sleuth'
Plug 'bronson/vim-trailing-whitespace'
Plug 'critiqjo/husk-x.vim'
call plug#end()

let g:seoul256_background = 256
let b:ale_linters =
  \ { 'nix': ['nix']
  \ , 'rust': ['analyzer', 'cargo']
  \ , 'sh': ['shellcheck']
  \ }
let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:neoformat_only_msg_on_error = 1
let g:neoformat_enabled_typescript = ['denofmt']
let g:fzf_preview_window = ''
let g:pear_tree_repeatable_expand = 0
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1

set termguicolors
set background=light
colorscheme seoul256
highlight! link StatusLine CursorLineNr
highlight! link StatusLineNC LineNr
set number
set relativenumber
set colorcolumn=81
set noshowmode
set splitbelow
set splitright
set nowrap
set expandtab
set shiftwidth=2
set shiftround
set nojoinspaces
set virtualedit=block,onemore
set noswapfile
set nowritebackup
set ignorecase
set smartcase
set wildignore+=*/.git/*,*/dist-newstyle/*
set wildignorecase
set gdefault
set mouse=a
set hidden
set inccommand=nosplit

nnoremap Y y$
nnoremap j gj
nnoremap k gk
vnoremap < <gv
vnoremap > >gv
noremap <silent> <Backspace> :<C-u>setlocal hlsearch!<CR>
noremap <silent> <A-o> :<C-u>Files<CR>
imap <Space> <Plug>(PearTreeSpace)
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

augroup autocmds
  autocmd!
  autocmd FileType gitcommit setlocal spell colorcolumn=51,73
  autocmd BufWritePre *.dhall,*.rs,*.tf,*.ts undojoin | Neoformat
augroup END
