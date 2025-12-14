# macOS hostname

## Hostname

Sometimes the same as the local hostname, sometimes defined elsewhere(??).

```
$ hostname
# or (if manually set)
$ scutil --get HostName
```

## Local hostname

Defined in System Settings > General > Sharing > Local hostname.

```
# Note: prints without `.local` TLD
$ scutil --get LocalHostName
```

## Computer name

Defined in System Settings > General > About > Name.

```
$ scutil --get ComputerName
```

## References

- <https://www.autodesk.com/support/technical/article/caas/sfdcarticles/sfdcarticles/Setting-the-Mac-hostname-or-computer-name-from-the-terminal.html>
- <https://apple.stackexchange.com/a/461489>
