function wcargo --wraps "cargo lclippy"
    set --local --export CARGO_TERM_PROGRESS_TERM_INTEGRATION false
    watchexec \
        --quiet \
        --clear \
        --exts rs,toml \
        --restart \
        -- cargo lclippy --all-targets $argv
end
