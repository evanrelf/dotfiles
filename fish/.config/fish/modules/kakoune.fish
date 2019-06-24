if _exists kak; and status --is-interactive
  abbr --add k "kak"
  abbr --add kc "kakc"
end

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
    if test $status -eq 255
        echo "Starting Kakoune daemon"
        kakd
        kakc $argv
    end
end
complete --command kakc --wraps kak
