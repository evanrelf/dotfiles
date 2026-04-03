function wcargo --wraps "cargo lclippy"
    watchexec \
        --quiet \
        --clear \
        --exts rs,toml \
        -- cargo lclippy --all-targets $argv
end
