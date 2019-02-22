# function v -d "Fuzzy project file open in editor"
#     set -l git_root (realpath --relative-to=(pwd) (git rev-parse --show-toplevel))
#     set -l fd_cmd "fd --type file --follow --hidden --exclude '.git' . $git_root"
#     set -l fzf_cmd "fzf -1 -0 --height=30% --exact"
#     set -l file (eval "$fd_cmd | $fzf_cmd")
#     if test $status -eq 130
#         # Ctrl-C
#         return 0
#     end
#     if test -n "$file"
#         eval $EDITOR $file
#     else
#         _error "Empty file path"
#     end
# end

function fn -d "Search for Elm/Haskell function definition"
    set -l match (rg -Hin "^\s*(,|\{)?\s*\w*$argv[1]\w* ::? " -g "*.hs" -g "*.elm" | fzf -1 -0 --height 50% --exact)
    set -l file (echo $match | cut -d ':' -f 1)
    set -l line (echo $match | cut -d ':' -f 2)
    if test -e $file
        nvim $file +$line +"norm zz"
    else
        echo "No results found" >&2
        return 1
    end
end
