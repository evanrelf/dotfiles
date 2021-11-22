#!/usr/bin/env bash

set -Eeuo pipefail
IFS=$'\n\t'

# shellcheck source=/dev/null
[ -f ~/.xprofile ] && . ~/.xprofile
