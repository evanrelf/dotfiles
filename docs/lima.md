# Lima

## Fixing problems after instance creation

### Install Nix

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
