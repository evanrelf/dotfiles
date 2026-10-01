# Iris Apps

Easily serve little web apps from `iris`. Custom `<name>.internal.evanrelf.com`
hostname, HTTPS, optional on-demand mode (start on request, stop on idle), etc.

## Create

- Write `apps/<name>/default.nix` to satisfy `modules/nixos/iris/apps.nix`.
- Apps must follow `$PORT` and `$ON_DEMAND` (panic if unsupported).
- Store persistent state in `$STATE_DIRECTORY` (i.e. `/var/lib/app-<name>/`).
- If on-demand mode is supported and `ON_DEMAND=true`:
  - The app is started lazily in response to requests.
  - The app should shut down after an idle period / when work is complete.
  - `systemd` provides a unique `INVOCATION_ID` for each run, if that's helpful.

Here's a trivial example app:

```
$ mkdir -p apps/foo/www/
$ echo "<h1>Hello, world!</h1>" > apps/foo/www/index.html
$ cat <<EOF > apps/foo/default.nix
{ pkgs, ... }:
{
  package = pkgs.writeShellScriptBin "foo" ''
    #!/usr/bin/env bash
    ${pkgs.python3}/bin/python3 -m http.server 12345 --directory ${./www}
  '';
  port = 12345;
}
EOF
```

For a full-fledged example, see `apps/demo/`.

## Build

Currently apps' packages are only available inside of the `iris` NixOS
configuration. The easiest way to build is:

```
$ nixos-rebuild build --flake .#iris --build-host iris --target-host iris
```

## Deploy

```
$ nixos-rebuild dry-activate --flake .#iris --build-host iris --target-host iris --sudo
$ nixos-rebuild switch --flake .#iris --build-host iris --target-host iris --sudo
```

## Monitor

```
$ ssh iris -- systemctl status app-foo.service
$ ssh iris -- ls -lah /var/lib/app-foo/
```
