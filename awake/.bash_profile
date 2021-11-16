#!/usr/bin/env bash

# Forcibly set 'fish' as my shell, even though I can't modify '/etc/shells'
if [ "$(hostname -s)" = "hydra-dev" ] && [ "$(basename "${SHELL}")" != "fish" ]; then
  sudo chsh "$(whoami)" --shell "$(command -v fish)"
  exec fish --login
fi
