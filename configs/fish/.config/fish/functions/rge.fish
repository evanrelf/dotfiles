function rge --wraps rg
    for file in (rg --vimgrep $argv | cut -d ':' -f 1-3 | sort -u -t ':' -k 1,1)
        $EDITOR $file || break
    end
end
