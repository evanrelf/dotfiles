function whereami
    if test (uname -a) = Darwin
        scutil --get ComputerName
    else
        hostname -s
    end
end
