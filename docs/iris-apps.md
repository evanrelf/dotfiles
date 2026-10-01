# Iris Apps

Easily serve little web apps from `iris`. Custom `<name>.internal.evanrelf.com`
hostname, HTTPS, optional on-demand mode (start on request, stop on idle), etc.

## Create

- Write `apps/<name>/default.nix` to satisfy `modules/nixos/iris/apps.nix`.
- Listen on `127.0.0.1:$PORT`. By default the port is derived from the app's
  name, but you can override it.
- Store persistent state in `$STATE_DIRECTORY` (i.e. `/var/lib/app-<name>/`).
- Set `onDemand = true` to start lazily on the first request, and stop once
  there's been no open connections for a while.
  - `systemd-socket-proxyd` handles wake on socket and stop on idle
    automatically; your app doesn't have to do anything except listen on a port.
  - `systemd` provides a unique `INVOCATION_ID` for each run, if that's helpful.
  - After `idleTimeout` (defaults to 5 minutes), your process receives a
    `SIGTERM`. Make sure to gracefully shutdown when receiving this signal if
    necessary.

Here's a trivial example app:

```
$ mkdir -p apps/foo/www/
$ echo "<h1>Hello, world!</h1>" > apps/foo/www/index.html
$ cat <<EOF > apps/foo/default.nix
{ pkgs, ... }:
{
  package = pkgs.writeShellScriptBin "foo" ''
    exec ${pkgs.python3}/bin/python3 -m http.server "$PORT" --bind 127.0.0.1 --directory ${./www}
  '';
  onDemand = true;
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
$ ssh iris -- systemctl status app-foo-proxy.{socket,service} # on-demand
$ ssh iris -- journalctl --unit 'app-foo*' --follow
$ ssh iris -- ls -lah /var/lib/app-foo/
```
