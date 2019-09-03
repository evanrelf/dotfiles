" PLUGINS {{{1
call plug#begin()

" Color schemes
Plug 'rakr/vim-one'
Plug 'bluz71/vim-moonfly-colors'
Plug 'Lokaltog/vim-monotone'
Plug 'evanrelf/papercolor-theme'

" Appearance
Plug 'itchyny/lightline.vim' | Plug 'maximbaz/lightline-ale'
Plug 'jeffkreeftmeijer/vim-numbertoggle'

" Movement
Plug 'critiqjo/husk-x.vim'
Plug 'andymass/vim-matchup'

" Editing
Plug 'machakann/vim-sandwich'
Plug 'tomtom/tcomment_vim'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'

" Completion
Plug 'zxqfl/tabnine-vim'
" Plug 'neoclide/coc.nvim', { 'do': './install.sh nightly' }
Plug 'tmsvg/pear-tree'

" Formatting
Plug 'sbdchd/neoformat', { 'on': ['Neoformat'] }
Plug 'junegunn/vim-easy-align'
Plug 'sickill/vim-pasta'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-sleuth'

" Information
Plug 'w0rp/ale'
Plug 'airblade/vim-gitgutter'
Plug 'simnalamburt/vim-mundo', { 'on': ['MundoToggle', 'MundoShow'] }

" Syntax
Plug 'sheerun/vim-polyglot'
Plug 'evanrelf/purescript-vim'
Plug 'vmchale/dhall-vim'

" Files
Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'tpope/vim-eunuch'
Plug 'EinfachToll/DidYouMean'

" Miscellaneous
Plug 'sgur/vim-editorconfig'
Plug 'direnv/direnv.vim'
Plug 'moll/vim-bbye'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'Konfekt/FastFold'
Plug 'tpope/vim-repeat'

call plug#end()

" Plugin settings {{{2

" one
let g:one_allow_italics = 1
set t_8b=^[[48;2;%lu;%lu;%lum
set t_8f=^[[38;2;%lu;%lu;%lum

" monotone
" let g:monotone_color = [30, 50, 80]

" lightline
let g:lightline = {}
let g:lightline.colorscheme = 'one'
let g:lightline.component_expand = {
      \ 'linter_checking': 'lightline#ale#checking',
      \ 'linter_errors': 'lightline#ale#errors',
      \ 'linter_warnings': 'lightline#ale#warnings',
      \ 'linter_ok': 'lightline#ale#ok',
      \ }
let g:lightline.component_function = {
      \ 'gitbranch': 'fugitive#head',
      \ 'cocstatus': 'coc#status',
      \ }
let g:lightline.component_type = {
      \ 'linter_checking': 'middle',
      \ 'linter_errors': 'error',
      \ 'linter_warnings': 'warning',
      \ 'linter_ok': 'middle',
      \ }
let g:lightline.active = {
      \ 'left': [[], ['paste', 'filename', 'readonly', 'modified'], ['cocstatus', 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok']],
      \ 'right': [[], ['filetype'], ['lineinfo']],
      \}
let g:lightline#ale#indicator_checking = ''
let g:lightline#ale#indicator_ok = ''

" match up
let g:matchup_matchparen_status_offscreen = 0
let g:matchup_matchparen_deferred = 1

" ultisnips
" let g:UltiSnipsExpandTrigger = '<Tab>'
" let g:UltiSnipsJumpForwardTrigger = '<C-j>'
" let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
" let g:UltiSnipsJumpForwardTrigger = '<Tab>'
" let g:UltiSnipsJumpBackwardTrigger = '<S-Tab>'
let g:UltiSnipsEditSplit = 'vertical'

" pear tree
let g:pear_tree_repeatable_expand = 0
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
let g:pear_tree_pairs = {
      \ '(': {'closer': ')'},
      \ '[': {'closer': ']'},
      \ '{': {'closer': '}'},
      \ "'": {'closer': "'"},
      \ '"': {'closer': '"'},
      \ '{-': {'closer': '-}'},
      \ '{-#': {'closer': '#-}'},
      \ '/*': {'closer': '*/'},
      \ '<!--': {'closer': '-->'},
      \ }

" neoformat
let g:neoformat_only_msg_on_error = 1

" ale
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
      \ 'haskell': ['stack-build', 'hlint'],
      \ 'rust': ['rls', 'cargo', 'rustc']
      \ }
let g:ale_fixers = {
      \ 'elm': ['elm-format'],
      \ 'rust': ['rustfmt'],
      \ }
      " \ 'javascript': ['prettier']
      " \ }
let g:ale_fix_on_save = 1
let g:ale_sign_error = '▌'
let g:ale_sign_warning = '▌'
let g:ale_echo_msg_info_str = '[INFO]'
let g:ale_echo_msg_error_str = '[ERR]'
let g:ale_echo_msg_warning_str = '[WARN]'
let g:ale_echo_msg_format = '%severity% %code: %%s [%linter%]'

" better-whitespace
let g:strip_whitelines_at_eof = 1

" polyglot
let g:polyglot_disabled = ['purescript']
let g:vim_markdown_conceal = 0
let g:vim_markdown_new_list_item_indent = 0
let g:haskell_indent_if = 2
let g:haskell_indent_in = 0
let g:elm_setup_keybindings = 0
let g:elm_format_autosave = 0
" let g:purescript_indent_case = 2

" evanrelf/purescript-vim
let g:purescript_indent_if = 2
let g:purescript_indent_in = 0

" gitgutter
let g:gitgutter_map_keys = 0
let g:gitgutter_grep = 'rg'

" didyoumean
let g:dym_use_fzf = 1

" }}}2

" SETTINGS {{{1
" Appearance {{{2
set termguicolors
colorscheme one
set background=dark
set cursorline
set colorcolumn=81
set number
set relativenumber
set signcolumn=yes
set cmdheight=2
set rulerformat=%7(%3(%l%),%3(%c%V%)%)
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
set hlsearch
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
set updatetime=250
set inccommand=nosplit
set keywordprg=:help
scriptencoding 'utf-8'

" }}}2


" COMMANDS {{{1
function! CommandCabbr(abbreviation, expansion) abort
  silent execute 'cabbrev ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ Command call CommandCabbr(<f-args>)
Command bd Bd

command! Cd setlocal autochdir! | setlocal autochdir!
command! V edit $MYVIMRC
command! Marked silent !open % -a 'Marked 2.app'
command! Bg let &background=(&background == 'dark' ? 'light' : 'dark')
command! Num let &number=(&number == 0 ? 1 : 0) | let &relativenumber=&number
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


" MAPPINGS {{{1
noremap ; :
noremap : ;
noremap Y y$
noremap j gj
noremap k gk
nnoremap J m`J``
xnoremap < <gv
xnoremap > >gv
nnoremap > >>
nnoremap < <<
xnoremap p "_dP
nnoremap gp `[v`]
xnoremap gp <Esc>`[v`]
tnoremap <Esc> <C-\><C-n>
noremap Q @q
noremap <silent> g. :<C-u>ALENextWrap<CR>zz
noremap <silent> g, :<C-u>ALEPreviousWrap<CR>zz
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)
noremap <Left> 5zh
noremap <Right> 5zl
noremap <Up> 5<C-y>
noremap <Down> 5<C-e>
noremap gh 0
noremap gi ^
noremap gj G
noremap ge G$
noremap gk gg
noremap gl g_
noremap <silent> <Backspace> :<C-u>nohlsearch<CR>
noremap <silent> <Tab> :tabnext<CR>
noremap <silent> <S-Tab> :tabprev<CR>
noremap <C-]> <C-]>zz
noremap <C-t> <C-t>zz
imap <Space> <Plug>(PearTreeSpace)
noremap n nzz
noremap N Nzz

" Leader
map <Space> <Leader>
nnoremap <Leader>s :%s/
xnoremap <Leader>s :s/
nnoremap <Leader>g :%g/
xnoremap <Leader>g :g/
nnoremap <Leader>n :%norm 0
xnoremap <Leader>n :norm 0
noremap <silent> <Leader>= :Neoformat<CR>
map <silent> <Leader>ad <Plug>(ale_detail)
noremap <silent> <Leader>f :<C-u>GFiles<CR>
noremap <silent> <Leader>F :<C-u>Files<CR>
noremap <silent> <Leader>r :<C-u>GRg<CR>
noremap <silent> <Leader>R :<C-u>Rg<CR>
noremap <silent> <Leader>b :<C-u>Buffers<CR>
xnoremap <Leader>S :sort<CR>
noremap <silent> <Leader>G <C-w>=:<C-u>GoldenRatioToggle<CR>
noremap <silent> <Leader>T :TagbarToggle<CR>
noremap <silent> <Leader>U :MundoToggle<CR>

" Available
noremap S <Nop>
noremap + <Nop>
noremap _ <Nop>
xnoremap P <Nop>
xnoremap Z <Nop>


" AUTOCOMMANDS {{{1
augroup FileTypeSettings " {{{2
  autocmd!
  " autocmd FileType purescript
  "       \  setlocal indentexpr=GetHaskellIndent()
  "       \| setlocal indentkeys=!^F,o,O,0{,0},0(,0),0[,0],0,,0=where,0=let,0=in\ ,0=::\ ,0=\-\>\ ,0=\=\>\ ,0=\|\ ,=\=\,0=deriving
  autocmd FileType elm
        \  setlocal softtabstop=4 shiftwidth=4
  autocmd FileType fish
        \  setlocal softtabstop=4 shiftwidth=4
  autocmd FileType cpp
        \  setlocal commentstring=//\ %s
  autocmd BufRead git-revise-todo
        \  setlocal filetype=gitrebase
        \| %s/\s\+$//e
        \| normal! gg
  autocmd FileType gitcommit
        \  setlocal colorcolumn=73 spell
  autocmd BufRead Dockerfile*
        \  setlocal filetype=Dockerfile
  autocmd FileType vim,help
        \  setlocal keywordprg=:help
  autocmd FileType help
        \  nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd TermOpen *
        \  setlocal wrap nonumber norelativenumber
  autocmd FileType man
        \  setlocal laststatus=0 noruler wrap colorcolumn=
        \| noremap <buffer> h 2zh
        \| noremap <buffer> j 2<C-e>L0
        \| noremap <buffer> k 2<C-y>H0
        \| noremap <buffer> l 2zl
        \| noremap <buffer> d <C-d>
        \| noremap <buffer> u <C-u>
  autocmd FileType vim-plug
        \  setlocal wrap nonumber norelativenumber colorcolumn=
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType fzf
        \  nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType ale-preview
        \  setlocal wrap nonumber norelativenumber colorcolumn=
        \| nnoremap <buffer> <Esc> :<C-u>q<CR>
  autocmd FileType markdown,text,latex,tex
        \  setlocal wrap nonumber norelativenumber
  autocmd FileType json
        \  syntax match Comment +\/\/.\+$+
augroup END

augroup FormatOptions " {{{2
  autocmd!
  autocmd FileType * set formatoptions=cqnj
augroup END

augroup AutoRead " {{{2
  autocmd!
  autocmd FocusGained,BufEnter * :checktime
augroup END

" augroup IgnoreCaseCommandMode " {{{2
"   autocmd!
"   autocmd CmdLineEnter : setlocal nosmartcase
"   autocmd CmdLineLeave : setlocal smartcase
" augroup END

augroup AutoRedraw " {{{2
  autocmd!
  autocmd VimResized * redraw!
  autocmd CursorHold * redraw!
  autocmd CursorHoldI * redraw!
augroup END

" }}}2


" COC {{{1
" " Use tab for trigger completion with characters ahead and navigate.
" " Use command ':verbose imap <Tab>' to make sure Tab is not mapped by other plugin.
" " inoremap <silent> <expr> <Tab>
" "       \ pumvisible() ? "\<C-n>" :
" "       \ <SID>check_back_space() ? "\<Tab>" :
" "       \ coc#refresh()
"
" inoremap <silent> <expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
"       \ <SID>check_back_space() ? "\<Tab>" :
"       \ coc#refresh()
"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"
"
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction
"
" " Use <C-Space> to trigger completion.
" " inoremap <silent> <expr> <C-Space> coc#refresh()
"
" " Use <CR> to confirm completion, `<C-g>u` means break undo chain at current position.
" " Coc only does snippet and additional edit on confirm.
" inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"
" " Use K to show documentation in preview window
" nnoremap <silent> K :call <SID>show_documentation()<CR>
"
" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     call CocAction('doHover')
"   endif
" endfunction


" vim: foldenable foldmethod=marker
