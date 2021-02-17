call plug#begin()
Plug 'junegunn/seoul256.vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-repeat'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'sheerun/vim-polyglot'
Plug 'dense-analysis/ale', {'for': ['fish', 'nix', 'rust', 'sh']}
Plug 'sbdchd/neoformat', {'on': 'Neoformat'}
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'tmsvg/pear-tree'
Plug 'tomtom/tcomment_vim'
Plug 'machakann/vim-sandwich'
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'tpope/vim-sleuth'
Plug 'bronson/vim-trailing-whitespace'
Plug 'critiqjo/husk-x.vim'
call plug#end()

" junegunn/seoul256.vim
let g:seoul256_background = 256
set termguicolors
set background=light
colorscheme seoul256-light
highlight! link StatusLine CursorLineNr
highlight! link StatusLineNC LineNr

" dense-analysis/ale
let b:ale_linters =
  \ { 'nix': ['nix']
  \ , 'rust': ['analyzer', 'cargo']
  \ , 'sh': ['shellcheck']
  \ }
let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = v:false

" sbdchd/neoformat
let g:neoformat_only_msg_on_error = v:true
augroup neoformat
  autocmd!
  autocmd BufWritePre *.dhall,*.fish,*.rs undojoin | Neoformat
augroup END

" Shougo/deoplete.nvim
set completeopt+=menuone,noselect
set shortmess+=c
let g:deoplete#enable_at_startup = v:true
call deoplete#custom#option('max_list', 50)
inoremap <silent><expr> <Tab>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<Tab>" :
  \ deoplete#manual_complete()
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" tmsvg/pear-tree
let g:pear_tree_repeatable_expand = v:false
let g:pear_tree_smart_openers = v:true
let g:pear_tree_smart_closers = v:true
let g:pear_tree_smart_backspace = v:true
imap <Space> <Plug>(PearTreeSpace)

" junegunn/vim-easy-align
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

set number
set relativenumber
set colorcolumn=81
set noshowmode
set nowrap
set splitbelow
set splitright
set wildmode=longest:full,list:full
set mouse=a
set expandtab
set shiftwidth=2
set shiftround
set nojoinspaces
set ignorecase
set smartcase
set wildignore+=*/.git/*,*/dist/*,*/dist-newstyle/*
set wildignorecase
set gdefault
set noswapfile
set nowritebackup
set hidden
set inccommand=nosplit
set virtualedit=block,onemore

nnoremap Y y$
nnoremap j gj
nnoremap k gk
xnoremap < <gv
xnoremap > >gv
noremap <silent> <Backspace> :<C-u>setlocal hlsearch!<CR>
nnoremap \| :%!
xnoremap \| :!
noremap U :<C-u>echoerr "Use \<C-r\>"<CR>

augroup autocmds
  autocmd!
  autocmd FileType rust setlocal colorcolumn=81,101
  autocmd FileType gitcommit setlocal spell colorcolumn=51,73
augroup END
