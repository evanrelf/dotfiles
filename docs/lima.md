# Lima

## Create VM

The Fedora Cloud image Lima uses is more minimal than the default Ubuntu image.
I would use Alpine, but it uses OpenRC instead of systemd. I would use Arch, but
it doesn't work with the `vz` VM type, at the time of writing.

```
limactl create \
  --name=default \
  template://fedora \
  --vm-type=vz \
  --mount-type=virtiofs \
  --mount-writable
```

## Install Nix

Copy install command from Determinate Systems' [nix-installer] repo.

[nix-installer]: https://github.com/DeterminateSystems/nix-installer

## Fix Ghostty issues

Fix `TERM` issues by copying Ghostty's terminfo entry to the VM:

```
infocmp -x | limactl shell default -- tic -x -
```

You can tell things have improved if Ctrl-L / `clear` starts working.
