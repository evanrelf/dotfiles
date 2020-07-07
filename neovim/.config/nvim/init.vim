call plug#begin()
Plug 'junegunn/seoul256.vim'
Plug 'NLKNguyen/papercolor-theme', {'on': []}
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-repeat'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'sheerun/vim-polyglot'
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
Plug 'moll/vim-bbye', {'on': 'Bdelete'}
Plug 'critiqjo/husk-x.vim'
call plug#end()

let g:seoul256_background = 256
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
set wildignore+=*/.git/*
set wildignorecase
set gdefault
set mouse=a
set hidden
set inccommand=nosplit

nnoremap Y y$
nnoremap j gj
nnoremap k gk
noremap <silent> <Backspace> :<C-u>setlocal hlsearch!<CR>
noremap <silent> <A-o> :<C-u>Files<CR>
imap <Space> <Plug>(PearTreeSpace)
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)
Command bd Bd

augroup autocmds
  autocmd!
  autocmd FileType gitcommit setlocal colorcolumn=51,73
augroup END
