function wpython
    watchexec \
        --quiet \
        --clear \
        --restart \
        -- echo 'TY:' ';' ty check $argv ';' echo 'RUFF:' ';' ruff check $argv
end
