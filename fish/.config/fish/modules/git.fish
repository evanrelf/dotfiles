if _exists git; and status --is-interactive
  abbr --add g "git"
end

function r -d "cd to project root"
    set -l root (git rev-parse --show-toplevel 2>/dev/null; or echo "")
    test -n $root
    and cd $root
    or return 1
end

