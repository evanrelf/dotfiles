" PLUGINS {{{1
call plug#begin()

" Appearance
Plug 'evanrelf/papercolor-theme'
Plug 'roman/golden-ratio'
" Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Movement
Plug 'junegunn/vim-slash'
Plug 'critiqjo/husk-x.vim'

" Editing
Plug 'machakann/vim-sandwich'
Plug 'tpope/vim-commentary'
Plug 'wellle/targets.vim'
" Plug 'michaeljsmith/vim-indent-object'

" Completion
" Plug 'zxqfl/tabnine-vim', { 'on': [] }
" Plug 'jiangmiao/auto-pairs'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-endwise'

" Formatting
Plug 'sbdchd/neoformat'
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'ntpeters/vim-better-whitespace'

" Syntax
Plug 'w0rp/ale'
Plug 'sheerun/vim-polyglot'

" Files
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }

" Miscellaneous
Plug 'moll/vim-bbye'
Plug 'tpope/vim-repeat'

call plug#end()

" Plugin settings {{{2
" golden-ratio
let g:golden_ratio_autocommand = 0

" neoformat
let g:neoformat_only_msg_on_error = 1

" ale
let g:ale_completion_enabled = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = { 'haskell': ['stack_build', 'hlint'] }
let g:ale_fixers = { 'haskell': ['hlint'] }

" better-whitespace
let g:strip_whitelines_at_eof = 1

" polyglot
let g:vim_markdown_new_list_item_indent = 0
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 0

" }}}2


" SETTINGS {{{1
" Appearance {{{2
set termguicolors
set background=dark
colorscheme PaperColor
set colorcolumn=81
set number
" set relativenumber
set noshowmode
set shortmess=filmxTWIcF
set title
set splitbelow
set splitright

" Indentation {{{2
set expandtab
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting {{{2
set nowrap
set linebreak
set breakindent
set nojoinspaces
set formatoptions=cqnj

" Extra files {{{2
set noswapfile
set nobackup
set nowritebackup

" Searching {{{2
set nohlsearch
set ignorecase
set smartcase
set gdefault
set report=0

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set mouse=a
set hidden
set virtualedit=block
set lazyredraw
set updatetime=2000
set inccommand=nosplit
set keywordprg=:help

" }}}2


" COMMANDS {{{1
function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)

command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == 'dark' ? 'light' : 'dark')

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'options': '--exact --delimiter : --nth 4..' },
      \   <bang>0)

command! -bang -nargs=* GRg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0],
      \     'options': '--exact --delimiter : --nth 4..' },
      \   <bang>0)

Command bd Bd


" MAPPINGS {{{1
noremap Y y$
noremap j gj
noremap k gk
nnoremap J m`J``
xnoremap < <gv
xnoremap > >gv
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]
tnoremap <Esc> <C-\><C-n>
vnoremap <silent> p :<C-u>let @p = @+<CR>gvp:let @+ = @p<CR>
noremap Q @@
noremap H ^
noremap L g_
nnoremap <silent> <C-n> :ALENext<CR>
nnoremap <silent> <C-p> :ALEPrevious<CR>
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)
map <Plug>(slash-after) zz
noremap <Left> 5zh
noremap <Right> 5zl
noremap <Up> 5<C-y>
noremap <Down> 5<C-e>

" Leader
map <Space> <Leader>
noremap <Leader>y "+y
noremap <Leader>Y "+y$
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
noremap <silent> <Leader>= :Neoformat<CR>
noremap <silent> <Leader>ad :<C-u>ALEDetail<CR>
noremap <silent> <Leader>f :<C-u>GFiles<CR>
noremap <silent> <Leader>F :<C-u>Files<CR>
noremap <silent> <Leader>r :<C-u>GRg<CR>
noremap <silent> <Leader>R :<C-u>Rg<CR>
noremap <silent> <Leader>h :<C-u>Helptags<CR>
noremap <silent> <Leader>b :<C-u>Buffers<CR>
nnoremap <Leader>S vip:sort<CR>
xnoremap <Leader>S :sort<CR>
noremap <silent> <Leader>G <C-w>=:<C-u>GoldenRatioToggle<CR>

" Available
noremap M <Nop>
noremap S <Nop>
noremap + <Nop>
noremap _ <Nop>
noremap <Tab> <Nop>
noremap <S-Tab> <Nop>
xnoremap P <Nop>
xnoremap R <Nop>
xnoremap Z <Nop>


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  autocmd FileType haskell setlocal keywordprg=hoogle\ --info
  autocmd FileType cpp setlocal commentstring=//\ %s
  autocmd FileType gitcommit setlocal colorcolumn=73 spell
  autocmd BufEnter .gitconfig* setlocal filetype=gitconfig noexpandtab shiftwidth=8
  autocmd BufEnter Dockerfile* setlocal filetype=Dockerfile
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd TermOpen * setlocal wrap nonumber norelativenumber
  autocmd FileType man
        \  setlocal laststatus=0 noruler wrap
        \| noremap <buffer> h 2zh
        \| noremap <buffer> j 2<C-e>L0
        \| noremap <buffer> k 2<C-y>H0
        \| noremap <buffer> l 2zl
        \| noremap <buffer> d <C-d>
        \| noremap <buffer> u <C-u>
        " \| autocmd! LazyLoadPlugins
  autocmd FileType vim-plug
        \  setlocal wrap nonumber norelativenumber
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType fzf noremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType ale-preview
        \  setlocal wrap nonumber norelativenumber
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType markdown,text,latex,tex setlocal nonumber norelativenumber wrap
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

augroup AutoRead " {{{2
  autocmd!
  autocmd FocusGained,BufEnter * :checktime
augroup END

augroup IgnoreCaseCommandMode " {{{2
  autocmd!
  autocmd CmdLineEnter : set nosmartcase
  autocmd CmdLineLeave : set smartcase
augroup END

" augroup LazyLoadPlugins " {{{2
"   autocmd!
"   autocmd CursorHold,CursorHoldI *
"         \  call plug#load('tabnine-vim')
"         \| autocmd! LazyLoadPlugins
" augroup END

" }}}2

" vim: foldenable foldmethod=marker
