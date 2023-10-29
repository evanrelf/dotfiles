if command -q lima
    function nerdctl
        lima nerdctl $argv
    end
end
