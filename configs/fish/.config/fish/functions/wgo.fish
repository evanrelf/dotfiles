function wgo --wraps "go build"
    watchexec \
        --quiet \
        --clear \
        --restart \
        -- go build -o /dev/null $argv '&&' echo 'ʕ⚆,⚆ʔ'
end
