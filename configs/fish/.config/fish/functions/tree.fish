function tree --wraps fd
    if isatty stdout
        fd $argv | as-tree --color always | command less -RMFXK
        return $pipestatus[1]
    else
        fd $argv | as-tree
        return $pipestatus[1]
    end
end
