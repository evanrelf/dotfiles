function sqlite3 --wraps sqlite3
    if command -v rlwrap >/dev/null
        rlwrap --complete-filenames -- sqlite3 $argv
    else
        command sqlite3 $argv
    end
end
