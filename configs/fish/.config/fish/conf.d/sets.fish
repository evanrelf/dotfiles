# TODO: Check number of arguments
# TODO: Check that arguments are file paths
# TODO: Implement predicates
# TODO: Allow customizing delimiter (currently assumes newline, but `comm` lets
#       you customize it)

# https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations

function set-intersect
    set --local left (sort -u $argv[1] | psub)
    set --local right (sort -u $argv[2] | psub)
    comm -12 $left $right
end

function set-union
    cat $argv | sort -u
end

function set-difference
    set --local left (sort -u $argv[1] | psub)
    set --local right (sort -u $argv[2] | psub)
    comm -23 $left $right
end

function set-symmetric-difference
    set --local left (sort -u $argv[1] | psub)
    set --local right (sort -u $argv[2] | psub)
    set-difference (set-union $left $right) (set-intersect $left $right)
end

# function set-is-disjoint
# end

# function set-is-subset
# end

# function set-is-superset
# end
