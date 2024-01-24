# Lima

## Create VM

The Fedora Cloud image Lima uses is more minimal than the default Ubuntu image.
I would use Alpine, but it uses OpenRC instead of systemd. I would use Arch, but
it doesn't work with the `vz` VM type, at the time of writing.

```
[host]$ limactl create \
  --name=default \
  template://fedora \
  --vm-type=vz \
  --mount-type=virtiofs \
  --mount-writable
```

## Install Nix

Copy install command from Determinate Systems' [nix-installer] repo.

[nix-installer]: https://github.com/DeterminateSystems/nix-installer#readme

Then, add yourself as a trusted user:

```
[guest]$ sudo bash -c "echo 'extra-trusted-users = $(whoami)' >> /etc/nix/nix.conf"
[guest]$ sudo systemctl restart nix-daemon.service
```

## Fix Ghostty issues

Fix `TERM` issues by copying Ghostty's terminfo entry to the VM:

```
[host]$ infocmp -x | lima nix-shell -p ncurses --run 'tic -x -'
```

You can tell things have improved if Ctrl-L / `clear` starts working.

## Install dotfiles

TODO: Come up with a more concise way of doing this bootstrapping.

```
[guest]$ nix-shell -p git --run 'nix shell .#{home-manager,home-rebuild} -c home-rebuild switch'
```

or

```
[guest]$ nix-shell -p git --run 'nix build .#homeConfigurations.$(hostname -s).activationPackage' && ./result/activate && rm result
```
