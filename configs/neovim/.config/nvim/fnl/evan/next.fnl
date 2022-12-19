(import-macros {: set! : setlocal!} :evan.macros)

(local paq (require :paq))

(paq.register "ishan9299/modus-theme-vim")
(set! termguicolors)
(set! background "light")
(set vim.g.modus_dim_inactive_window false)
(vim.cmd.colorscheme "modus-operandi")

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

(vim.api.nvim_create_autocmd
  ["BufWritePost"]
  {:group "Evan"
   :pattern "init.lua"
   :command "source <afile> | PackerCompile"})

(vim.api.nvim_create_autocmd
  ["BufEnter"]
  {:group "Evan"
   :pattern ["git-revise-todo"]
   :callback (lambda [] (setlocal! filetype "gitrebase"))})

(vim.api.nvim_create_autocmd
  ["FileType"]
  {:group "Evan"
   :pattern "fish"
   :callback (lambda [] (setlocal! shiftwidth 4))})

(vim.api.nvim_create_autocmd
  ["FileType"]
  {:group "Evan"
   :pattern "rust"
   :callback (lambda [] (setlocal! colorcolumn "81,101"))})

(vim.api.nvim_create_autocmd
  ["FileType"]
  {:group "Evan"
   :pattern "gitcommit"
   :callback (lambda [] (setlocal! colorcolumn "51,73"))})

(vim.api.nvim_create_autocmd
  ["TermOpen"]
  {:group "Evan"
   :callback (lambda []
               (setlocal! nonumber)
               (setlocal! norelativenumber)
               (vim.cmd "startinsert!"))})

(vim.api.nvim_create_autocmd
  ["BufEnter" "BufWinEnter" "WinEnter"]
  {:group "Evan"
   :callback (lambda []
               (when (= vim.bo.buftype "terminal")
                 (vim.cmd "startinsert!")))})

(vim.api.nvim_create_autocmd
  ["BufLeave" "BufWinLeave" "WinLeave"]
  {:group "Evan"
   :callback (lambda []
               (when (= vim.bo.buftype "terminal")
                 (vim.cmd "stopinsert")))})
