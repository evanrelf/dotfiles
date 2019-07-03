#!/usr/bin/env bash

if [ -f "$HOME/.bashrc" ]; then
  # shellcheck source=/dev/null
  source "$HOME/.bashrc"
fi
if [ -e /Users/evanrelf/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/evanrelf/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
