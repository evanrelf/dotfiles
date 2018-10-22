" META {{{1
" vim: foldenable foldmethod=marker

" Evan Relf's Neovim config
" https://github.com/evanrelf/dotfiles/


" PLUGINS {{{1
call plug#begin()

" Appearance {{{2
Plug 'NLKNguyen/papercolor-theme'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'junegunn/goyo.vim'
Plug 'haya14busa/vim-operator-flashy' | Plug 'kana/vim-operator-user'

" Editing {{{2
Plug 'machakann/vim-sandwich'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'michaeljsmith/vim-indent-object'

" Navigation {{{2
Plug 'justinmk/vim-dirvish'
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }

" Movement {{{2
Plug 'wellle/targets.vim'
Plug 'critiqjo/husk-x.vim'
Plug 'junegunn/vim-slash'

" Intelligence {{{2
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"       \| Plug 'eagletmt/neco-ghc'
"       \| Plug 'zchee/deoplete-clang'
"       \| Plug 'Shougo/neoinclude.vim'
"       \| Plug 'pbogut/deoplete-elm'
"       \| Plug 'ponko2/deoplete-fish'
"       \| Plug 'Shougo/neco-vim'
"       \| Plug 'Shougo/neco-syntax'
"       \| Plug 'Shougo/echodoc.vim'
Plug 'w0rp/ale'
" TODO
" Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug '~/Projects/ghcid/plugins/nvim'
Plug 'sbdchd/neoformat'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-endwise'
Plug 'alvan/vim-closetag'
Plug 'sickill/vim-pasta'

" Syntax {{{2
Plug 'sheerun/vim-polyglot'
Plug 'lervag/vimtex'
Plug 'tpope/vim-sleuth', { 'on': [] } " Doesn't get along with vim-polyglot
Plug 'ntpeters/vim-better-whitespace'

" Miscellaneous {{{2
Plug 'vimlab/split-term.vim'
Plug 'tpope/vim-eunuch'
Plug 'Carpetsmoker/undofile_warn.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'amiorin/vim-eval'
Plug 'tpope/vim-repeat'
Plug 'Konfekt/FastFold'

" }}}2

call plug#end()

" Plugin settings {{{2
" PaperColor
let g:PaperColor_Theme_Options = { 'theme': { 'default': { 'allow_bold': 0 } } }

" lightline
let g:lightline = { 'colorscheme': 'wombat' }

" gitgutter
let g:gitgutter_map_keys = 0

" deoplete
set completeopt+=menu
set completeopt-=preview
inoremap <expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
augroup Deoplete
  autocmd!
  autocmd CompleteDone * silent! pclose!
augroup END
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/Library/Developer/CommandLineTools/usr/lib/libclang.dylib'
let g:deoplete#sources#clang#clang_header = '/Library/Developer/CommandLineTools/usr/lib/clang'

" ALE
let g:ale_lint_on_text_changed = 'never'
" let g:ale_linters = { 'haskell': ['stack_build', 'hlint'] }
let g:ale_linters = { 'haskell': [] }
let g:ale_fixers = { 'haskell': ['hlint'] }

" Ghcid
let g:ghcid_verbosity = 1

" Neoformat
let g:neoformat_basic_format_trim = 1

" polyglot
let g:polyglot_disabled = ['latex']
let g:vim_markdown_new_list_item_indent = 0
let g:elm_format_autosave = 0

" vimtex
let g:vimtex_view_method = 'skim'

" split-term
let g:disable_key_mappings = 1

" undofile_warn
let g:undofile_warn_mode = 2

" }}}2


" SETTINGS {{{1
" Apperance {{{2
augroup ColorSchemes " {{{3
  autocmd!
  autocmd ColorScheme *
        \  highlight CursorLineNr NONE
        \| highlight link CursorLineNr Normal
augroup END " }}}3
set termguicolors
set background=dark
colorscheme PaperColor
set noshowmode
set title
set shortmess=filmxTWIc
set splitbelow
set splitright
set list
set listchars=tab:▸\ ,nbsp:␣
set number
set numberwidth=2
set colorcolumn=81

" Indentation {{{2
set expandtab
set tabstop=8
set softtabstop=2
set shiftwidth=2
set shiftround

" Formatting {{{2
set nowrap
set textwidth=79
set linebreak
set breakindent
set formatoptions=crqnj
set nojoinspaces

" Extra files {{{2
set undofile
set noswapfile
set nobackup
set nowritebackup

" Searching {{{2
set nohlsearch
set ignorecase
set smartcase
set gdefault
set report=0
set grepprg=rg\ --vimgrep

" Wild mode {{{2
set wildmode=longest:full,full
set wildignore+=*/.git/*,*/tmp/*,*.swp,.DS_Store
set wildignorecase

" Miscellaneous {{{2
set hidden
set autoread
set mouse=a
set virtualedit=block
set keywordprg=:help
set lazyredraw
set updatetime=100
set inccommand=nosplit

" }}}2


" FUNCTIONS {{{1
function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)


" COMMANDS {{{1
command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == 'dark' ? 'light' : 'dark')

" Command w update

command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'options': '--delimiter : --nth 4..' },
      \   <bang>0)

command! -bang -nargs=* GRg
      \ call fzf#vim#grep(
      \   "rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1,
      \   { 'dir': systemlist('git rev-parse --show-toplevel')[0],
      \     'options': '--delimiter : --nth 4..' },
      \   <bang>0)

command! -bang -nargs=? -complete=dir FilesP
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)


" MAPPINGS {{{1
" General {{{2
noremap ; :
noremap : ;
noremap Y y$
noremap j gj
noremap k gk
noremap gj j
noremap gk k
xnoremap > >gv
xnoremap < <gv
nnoremap J m`J``
xnoremap p "_dP
xnoremap P p

noremap Q @@
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]

nnoremap <Tab> zo
nnoremap <S-Tab> zc

noremap <C-Tab> :<C-u>bnext<CR>
noremap <C-S-Tab> :<C-u>bprev<CR>
inoremap <C-Tab> <Esc>:<C-u>bnext<CR>
inoremap <C-S-Tab> <Esc>:<C-u>bprev<CR>
tnoremap <C-Tab> <C-\><C-n>:<C-u>bnext<CR>
tnoremap <C-S-Tab> <C-\><C-n>:<C-u>bprev<CR>

tnoremap <Esc> <C-\><C-n>
tnoremap <C-w> <C-\><C-n><C-w>

" EasyAlign
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" vim-operator-flashy
map y <Plug>(operator-flashy)
nmap Y <Plug>(operator-flashy)$

" Leader {{{2
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>G :%g!/
xnoremap <Leader>G :g!/
nnoremap <Leader>= :<C-u>Neoformat<CR>
xnoremap <Leader>= :Neoformat<CR>

" ALE
noremap <silent> <Leader>ap :<C-u>ALEPrevious<CR>
noremap <silent> <Leader>an :<C-u>ALENext<CR>
noremap <silent> <Leader>ad :<C-u>ALEDetail<CR>
noremap <silent> <Leader>af :<C-u>ALEFix<CR>

" FZF
noremap <silent> <Leader>f :<C-u>GFiles<CR>
noremap <silent> <Leader>F :<C-u>Files<CR>
noremap <silent> <Leader>r :<C-u>GRg<CR>
noremap <silent> <Leader>R :<C-u>Rg<CR>

" Terminal
noremap <silent> <Leader>t :<C-u>Term<CR>
noremap <silent> <Leader>T :<C-u>VTerm<CR>

" Disabled {{{2
nnoremap U <Nop>
noremap S <Nop>
noremap X <Nop>
inoremap <C-c> <Nop>

" }}}2


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  " Haskell
  autocmd FileType haskell
        \ setlocal keywordprg=hoogle\ --info
  " Elm
  autocmd BufWritePre *.elm undojoin | Neoformat
  " C++
  autocmd FileType cpp setlocal commentstring=//\ %s
  " Markdown
  autocmd FileType markdown setlocal wrap
  " Git
  autocmd FileType gitcommit setlocal textwidth=72 colorcolumn=73
  autocmd BufEnter .gitconfig*
        \  setlocal filetype=gitconfig
        \| setlocal noexpandtab shiftwidth=8
  " Docker
  autocmd BufEnter Dockerfile* setlocal ft=Dockerfile
  " Vim
  autocmd FileType vim,help setlocal keywordprg=:help
  autocmd FileType help
        \  noremap <buffer> q :q<CR>
        \| nnoremap <buffer> <Esc> :q<CR>
  " Terminal
  autocmd TermOpen *
        \  setlocal nonumber norelativenumber
        \| noremap <buffer> <C-c> i<C-c>
        \| noremap <buffer> <C-d> i<C-d>
  " autocmd BufEnter,WinEnter term://* startinsert
  " dirvish
  autocmd FileType dirvish
        \  noremap <buffer> u u
        \| noremap <buffer> <C-r> <C-r>
  " Man pages
  autocmd FileType man
        \  setlocal laststatus=0
        \| noremap <buffer> h <Nop>
        \| noremap <buffer> j <C-e>L0
        \| noremap <buffer> k <C-y>H0
        \| noremap <buffer> l <Nop>
  " plug
  autocmd FileType vim-plug setlocal nonumber norelativenumber
  " FZF
  autocmd FileType fzf
        \  nnoremap <buffer> <Esc> :q<CR>
        \| nnoremap <buffer> q :q<CR>
  " ALE
  autocmd FileType ale-preview
        \  setlocal wrap nonumber norelativenumber
        \| noremap <buffer> <Esc> :<C-u>q<CR>
  " Fish
  " autocmd FileType fish silent MUcompleteAutoOff
  " No name buffers
  autocmd BufEnter * if &filetype == "" | setlocal filetype=text | endif
  " Fix vim-sleuth and vim-polyglot not getting along
  autocmd Filetype * if &filetype != 'markdown' | call plug#load('vim-sleuth') | endif
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

" augroup ResizeSplits " {{{2
"   autocmd!
"   autocmd VimResized * wincmd =
" augroup END

augroup AutoRead " {{{2
  autocmd!
  autocmd FocusGained,BufEnter * :checktime
augroup END

" }}}2


" }}}1
