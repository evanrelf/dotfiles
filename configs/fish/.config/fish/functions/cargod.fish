function cargod
    cargo watch \
        --exec 'lclippy --all-targets' \
        --clear \
        --quiet \
        --ignore '.jj/' \
        $argv
end
