function wcargo
    set --local --export CARGO_TERM_PROGRESS_TERM_INTEGRATION false
    set --local command (
        if test -z $argv[1]
            echo "lclippy"
        else if test $argv[1] = "check"
            echo "lcheck"
        else if test $argv[1] = "clippy"
            echo "lclippy"
        else
            echo $argv[1]
        end
    )
    watchexec \
        --quiet \
        --clear \
        --exts rs,toml \
        --restart \
        -- cargo $command --all-targets
end
