function rg --wraps rg
    if isatty stdout
        command rg --pretty $argv | command less -RMFXK --mouse
        return $pipestatus[1]
    else
        command rg $argv
    end
end
