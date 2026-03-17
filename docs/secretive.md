# Secretive

## SSH config

```
# ~/.ssh/config

Host iris
  ForwardAgent yes

Host *
  IdentityAgent /Users/evanrelf/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh
```

## Resources

- Secretive website: https://secretive.dev/
- jj commit signing docs: https://www.jj-vcs.dev/latest/config/#commit-signing
