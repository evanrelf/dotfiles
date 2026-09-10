# https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations

function __set_usage --argument-names name
    set --erase argv[1]
    if test (count $argv) -ne 2 || ! test -f $argv[1] || ! test -f $argv[2]
        echo "usage: $name [-z | --zero-terminated] FILE FILE" >&2
        return 1
    end
end

function set-union
    # TODO: Check that arguments are files
    argparse z/zero-terminated -- $argv || return
    sort -u $_flag_z $argv
end

function set-intersect
    argparse z/zero-terminated -- $argv || return
    __set_usage set-intersect $argv || return
    comm -12 $_flag_z (sort -u $_flag_z $argv[1] | psub) (sort -u $_flag_z $argv[2] | psub)
end

function set-difference
    argparse z/zero-terminated -- $argv || return
    __set_usage set-difference $argv || return
    comm -23 $_flag_z (sort -u $_flag_z $argv[1] | psub) (sort -u $_flag_z $argv[2] | psub)
end

function set-symmetric-difference
    argparse z/zero-terminated -- $argv || return
    __set_usage set-symmetric-difference $argv || return
    cat (sort -u $_flag_z $argv[1] | psub) (sort -u $_flag_z $argv[2] | psub) | sort $_flag_z | uniq -u $_flag_z
end

function set-is-disjoint
    argparse z/zero-terminated -- $argv || return
    __set_usage set-is-disjoint $argv || return
    test (set-intersect $_flag_z $argv | wc -c) -eq 0
end

function set-is-subset
    argparse z/zero-terminated -- $argv || return
    __set_usage set-is-subset $argv || return
    test (set-difference $_flag_z $argv | wc -c) -eq 0
end

function set-is-superset
    argparse z/zero-terminated -- $argv || return
    __set_usage set-is-superset $argv || return
    set-is-subset $_flag_z $argv[2] $argv[1]
end
