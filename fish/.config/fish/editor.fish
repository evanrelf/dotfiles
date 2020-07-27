# if _exists kak
#     set --export EDITOR "kak"
# else
if _exists nvim
    set --export EDITOR "nvim"
else
    set --export EDITOR "vi"
end

if _exists emacs
    set --export ALTERNATE_EDITOR ""
    alias emacs "env TERM=xterm-24bit emacs"
    alias emacsclient "env TERM=xterm-24bit emacsclient"
    alias evil "env TERM=xterm-24bit emacsclient --tty"
end

# Better support for Emacs' vterm package
function vterm_printf
    if [ -n "$TMUX" ]
	# tell tmux to pass the escape sequences through
	# (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
	printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
	# GNU screen (screen, screen-256color, screen-256color-bce)
	printf "\eP\e]%s\007\e\\" "$argv"
    else
	printf "\e]%s\e\\" "$argv"
    end
end
