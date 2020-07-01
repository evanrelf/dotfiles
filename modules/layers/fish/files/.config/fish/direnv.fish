if _exists direnv
    set --export DIRENV_LOG_FORMAT
    eval (direnv hook fish)
end
