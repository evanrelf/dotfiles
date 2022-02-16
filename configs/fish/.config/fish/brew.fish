set --export HOMEBREW_NO_AUTO_UPDATE 1

if test -e /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
end
