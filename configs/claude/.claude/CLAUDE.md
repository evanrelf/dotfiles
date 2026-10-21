- Never mutate version control history. Any `jj` or `git` commands you run must
  be read-only, without side effects, unless explicitly requested by the user.
  That means no `jj edit` or `jj new` to fix commits in the past, no `jj commit`
  or `jj desc` to create or describe commits, etc.
