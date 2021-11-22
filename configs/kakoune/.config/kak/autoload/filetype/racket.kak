hook global BufCreate (.*/)?(.*\.rkt) %{
  set-option buffer filetype scheme
}
