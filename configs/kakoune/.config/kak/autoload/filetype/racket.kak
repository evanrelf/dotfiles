hook global BufCreate (.*/)?(.*\.rkt) %{
  set-option buffer filetype scheme
  set-option window formatcmd "scmindent"
}
