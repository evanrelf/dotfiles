function fish_default_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
    __fish_cursor_xterm line
end

function fish_default_cursor_handle_preexec --on-event fish_preexec
    if echo "$argv[1]" | grep -qE '^(sudo\s+)?(evil|emacs|emacsclient|nvim|vim|vi|kak)\b'
        __fish_cursor_xterm block
    else
        __fish_cursor_xterm line
    end
end
