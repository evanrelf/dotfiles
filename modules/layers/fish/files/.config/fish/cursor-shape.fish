function fish_default_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
    __fish_cursor_xterm line
end

function fish_default_cursor_handle_preexec --on-event fish_preexec
    __fish_cursor_xterm line
end
