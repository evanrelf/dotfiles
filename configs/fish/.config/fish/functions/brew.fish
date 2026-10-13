set --local brew_prefix /opt/homebrew
if test -d "$brew_prefix/share/fish/completions"
    set -p fish_complete_path "$brew_prefix/share/fish/completions"
end
if test -d "$brew_prefix/share/fish/vendor_completions.d"
    set -p fish_complete_path "$brew_prefix/share/fish/vendor_completions.d"
end
