(import-macros {: set!
                : setlocal!
                : autocmd!} :evan.macros)

(local paq (require :paq))

(paq.register "ishan9299/modus-theme-vim")
(set! termguicolors)
(set vim.g.modus_dim_inactive_window false)
(fn sync-colorscheme [options]
  (if (let [style (vim.fn.system "defaults read -g AppleInterfaceStyle")]
        (style:match "Dark"))
      (do
        (set! background "dark")
        (vim.cmd.colorscheme options.dark))
      (do
        (set! background "light")
        (vim.cmd.colorscheme options.light))))
(vim.api.nvim_create_user_command
  "SyncColorscheme"
  (lambda [] (sync-colorscheme {:dark "modus-vivendi" :light "modus-operandi"}))
  {})
(sync-colorscheme {:dark "modus-vivendi" :light "modus-operandi"})

(set! expandtab)
(set! shiftwidth 2)
(set! number)
(set! relativenumber)
(set! colorcolumn "81")
(set! noshowmode)
(set! ignorecase)
(set! smartcase)
(set! gdefault)
(set! nowrap)
(set! wildmode "longest:full,full")
(set! noswapfile)
(set! splitright)
(set! splitbelow)

(when vim.o.diff
  (set! nonumber)
  (set! norelativenumber))

(vim.keymap.set "n" "<Space>" "<Leader>" {:remap true})
(vim.keymap.set "x" "<" "<gv")
(vim.keymap.set "x" ">" ">gv")
(vim.keymap.set "t" "<Esc>" "<C-\\><C-n>")

(autocmd! "BufWritePost" "init.lua"
  (vim.cmd "source <afile> | PackerCompile"))

(autocmd! "BufEnter" "git-revise-todo"
  (setlocal! filetype "gitrebase"))

(autocmd! "FileType" "fish"
  (setlocal! shiftwidth 4))

(autocmd! "FileType" "rust"
  (setlocal! colorcolumn "81,101"))

(autocmd! "FileType" "gitcommit"
  (setlocal! colorcolumn "51,73"))

(autocmd! "TermOpen" "*"
  (setlocal! nonumber)
  (setlocal! norelativenumber)
  (vim.cmd "startinsert!"))

(autocmd! ["BufEnter" "BufWinEnter" "WinEnter"] "*"
  (when (= vim.bo.buftype "terminal")
    (vim.cmd "startinsert!")))

(autocmd! ["BufLeave" "BufWinLeave" "WinLeave"] "*"
  (when (= vim.bo.buftype "terminal")
    (vim.cmd "stopinsert")))
