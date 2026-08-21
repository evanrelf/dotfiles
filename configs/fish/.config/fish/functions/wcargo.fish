function wcargo --wraps "cargo lclippy"
    watchexec \
        --quiet \
        --clear \
        --exts rs,toml \
        --restart \
        -- cargo lclippy --all-targets $argv
end
