function _log
    echo -e $argv >&2
end

function _warn
    set_color yellow
    echo -e "WARN: $argv" >&2
    set_color normal
end

function _error
    set_color red
    echo -e "ERROR: $argv" >&2
    set_color normal
    return 1
end

function _silently
    eval "$argv" >/dev/null 2>&1
end

function _exists
    command -sq $argv[1]
    or type -q $argv[1]
end
