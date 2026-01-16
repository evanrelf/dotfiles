# Bootstrap

## 1. Install Lix

<https://lix.systems/install/>

## 2. Activate Darwin configuration

First time:

```
$ sudo nix run .#darwin-rebuild -- switch --flake .
```

After first time:

```
$ sudo darwin-rebuild switch --flake .
```

## 3. Activate home configuration

First time:

```
# macOS
$ sudo nix run .#home-manager -- switch --flake .#$(scutil --get ComputerName)

# Linux
$ sudo nix run .#home-manager -- switch --flake .#$(hostname -s)
```

After first time:

```
$ home-manager switch --flake .#(whereami)
```
