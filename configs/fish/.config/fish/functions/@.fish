function @
    while read --line line
        set --local path (string trim --left -- "$line")
        set --local indent_length (math (string length -- "$line") - (string length -- "$path"))
        set --local indent (string sub --length $indent_length -- "$line")
        if test -n "$path"; and test -f "$path"
            echo "$indent@$path"
        else
            echo "$line"
        end
    end
end
