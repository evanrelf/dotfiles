# Lima

## Using a custom NixOS instance

### Build the VM image

Assumes you have a Linux builder, e.g. from `nix-darwin`.

```
[host]$ nix build --system aarch64-linux --max-jobs 0 .#nixosImages.lima
```

The `./result` symlink will be used in the next step; it's referenced in the
Lima template.

### Create the Lima instance

Pick whatever name you want.

```
[host]$ limactl start --yes --name default configs/lima/nixos-template.yaml
```

## Fixing problems after instance creation

### Install Nix

_Not required for NixOS instances._

Copy install command from the [Lix website](https://lix.systems/install/).

Then, add yourself as a trusted user:

```
[guest]$ sudo bash -c "echo 'extra-trusted-users = $(whoami)' >> /etc/nix/nix.conf"
[guest]$ sudo systemctl restart nix-daemon.service
```

### Fix Ghostty issues

Fix `TERM` issues by copying Ghostty's terminfo entry to the VM:

```
[host]$ infocmp -x | lima nix-shell -p ncurses --run 'tic -x -'
```

You can tell things have improved if Ctrl-L / `clear` starts working.

### Install dotfiles

```
[guest]$ nix run .#home-manager -- --flake .#$(hostname -s) switch
```

### Change shell to Fish

```
[guest]$ sudo bash -c "echo $(which fish) >> /etc/shells"
[guest]$ sudo bash -c "chsh $(whoami) -s $(which fish)"
```
