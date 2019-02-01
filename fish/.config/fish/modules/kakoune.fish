function kakd
    kak -c daemon -e 'kill'
    kak -d -s daemon 2>/dev/null
end

function kakc
    if test (count $argv) -eq 0
        kak -c daemon -e 'buffer *scratch*'
    else
        kak -c daemon $argv
    end
end
complete --command kakc --wraps kak
