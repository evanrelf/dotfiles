function aichat --wraps aichat
    if test -z "$OPENAI_API_KEY"
        echo "error: OPENAI_API_KEY not defined" >&2
        return 1
    end
    command aichat $argv
end
