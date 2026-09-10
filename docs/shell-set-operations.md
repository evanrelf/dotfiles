# Set operations in the shell

<https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations>

Elements are newline-delimited.

TODO: Write nice Fish functions to abstract this stuff.

## Intersect

```fish
$ comm -12 (seq 1 4 | sort -u | psub) (seq 3 6 | sort -u | psub)
```

## Union

```fish
$ { seq 1 4; seq 3 6 } | sort -u
$ cat (seq 1 4 | sort -u | psub) (seq 3 6 | sort -u | psub) | sort -u
```

## Difference

```fish
$ comm -23 (seq 1 4 | sort -u | psub) (seq 3 6 | sort -u | psub)
```

## Symmetric difference

TODO: Come up with a more concise representation.

```fish
$ comm -23 ({ seq 1 4; seq 3 6 } | sort -u | psub) (comm -12 (seq 1 4 | sort -u | psub) (seq 3 6 | sort -u | psub) | psub)
```

## Insert

## Remove

## Contains

## Is Disjoint

## Is Subset

## Is Superset
