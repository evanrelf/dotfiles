function prqlcd
    watchexec --exts prql --clear -- prqlc compile --hide-signature-comment $argv
end
