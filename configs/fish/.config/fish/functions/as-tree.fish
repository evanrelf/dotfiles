function as-tree --wraps as-tree
    if isatty stdout
        command as-tree --color always $argv | command less -RMFXK
        return $pipestatus[1]
    else
        command as-tree $argv
    end
end
