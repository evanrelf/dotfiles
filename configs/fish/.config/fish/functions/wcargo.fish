function wcargo --wraps "cargo lclippy"
    cargo watch \
        --exec "lclippy --all-targets $argv" \
        --clear \
        --quiet \
        --ignore '.jj/'
end
