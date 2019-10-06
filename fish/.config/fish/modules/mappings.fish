function hybrid_bindings
    # fish_default_key_bindings -M default
    fish_default_key_bindings -M insert
    # fish_default_key_bindings -M visual
    fish_vi_key_bindings --no-erase
end

set -U fish_cursor_default block
set -U fish_cursor_unknown block
set -U fish_cursor_insert line
set -U fish_cursor_visual block

# Default
fish_default_key_bindings
# Hybrid
# set -U fish_key_bindings hybrid_bindings
# Vim
# fish_vi_key_bindings --no-erase

# Uncomment this to use a line cursor shape with the default keybindings
function fish_default_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
    __fish_cursor_xterm line
end

function fish_default_cursor_handle_preexec --on-event fish_preexec
    __fish_cursor_xterm line
end

# Uncomment this to use a dynamic cursor shape with the hybrid or vim keybindings
# if test $TERM = "xterm-kitty"
#     function fish_vi_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec --on-event fish_focus_in
#         set -l varname fish_cursor_$fish_bind_mode
#         if not set -q $varname
#             set varname fish_cursor_unknown
#         end
#         __fish_cursor_xterm $$varname
#     end
#     function fish_vi_cursor_handle_preexec --on-event fish_preexec
#         set -l varname fish_cursor_default
#         if not set -q $varname
#             set varname fish_cursor_unknown
#         end
#         __fish_cursor_xterm $$varname
#     end
# else
#     fish_vi_cursor
# end
