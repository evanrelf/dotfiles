---
name: jj-workspace
description: >-
  Create jj workspaces (like git worktrees). Must use if the user requests the
  use of jj workspaces. Consider using with subagents that could conflict in a
  shared working copy.
---

# jj workspaces

A jj workspace is an extra working copy attached to the same repo, like a git
worktree. Each workspace has its own working-copy commit, shown as `<name>@` in
`jj log`.

## Create

Always put the workspace in a fresh temporary directory, and always pass an
explicit `--name` and `--revision`:

```sh
dir=$(mktemp -d)
jj workspace add "$dir" --name <name> --revision <rev>
```

- `<name>`: short, descriptive, kebab-case (e.g. `fix-parser`, `bench-main`).
  It must not clash with an existing workspace (check `jj workspace list`).
- `<rev>`: the revision the user asked for (a change ID, bookmark, `@`, `@-`,
  etc.). If they didn't specify one, ask or pick deliberately; don't rely on the
  default. The workspace gets a new empty working-copy commit on top of `<rev>`,
  like `jj new <rev>`.

Then work in `$dir`. Ignored files are not copied over.

## Clean up

When done, and only after any changes made there are where they need to be:

```sh
jj workspace remove <name>
```

This snapshots the working copy and deletes the directory. The commits stay in
the repo. Use `jj workspace forget <name>` instead to stop tracking it without
touching the files on disk.

For anything else, see `jj workspace --help` and `jj workspace <cmd> --help`.
